package majorupgradechecklist

import (
	"bytes"
	"encoding/json"
	"fmt"
	"html/template"
	"io"
	"io/ioutil"
	"math"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"

	"github.com/pkg/errors"

	dc "github.com/chef/automate/api/config/deployment"
	oss "github.com/chef/automate/api/config/opensearch"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/lib/platform/sys"
	"github.com/sirupsen/logrus"
)

const MAX_OPEN_FILE_DEFAULT = "65535"
const MAX_LOCKED_MEM_DEFAULT = "unlimited"

const MAX_OPEN_FILE_KEY = "Max open files"
const MAX_LOCKED_MEM_KEY = "Max locked memory"

const INDICES_TOTAL_SHARD_DEFAULT = 6000
const INDICES_BREAKER_TOTAL_LIMIT_DEFAULT = "95"

var V3ESSettingFile string = "/hab/svc/deployment-service/old_es_v3_settings.json"
var AutomateOpensearchConfigPatch string = "/hab/svc/deployment-service/oss-config.toml"

type ESSettings struct {
	TotalShardSettings       int64  `json:"totalShardSettings" toml:"totalShardSettings"`
	IndicesBreakerTotalLimit string `json:"indicesBreakerTotalLimit" toml:"indicesBreakerTotalLimit"`
	RuntimeMaxOpenFile       string `json:"runtimeMaxOpenFile" toml:"runtimeMaxOpenFile"`
	RuntimeMaxLockedMem      string `json:"runtimeMaxLockedMem" toml:"runtimeMaxLockedMem"`
	HeapMemory               string `json:"heapMemory" toml:"heapMemory"`
}

type IndicesShardTotal struct {
	Indices struct {
		Shards struct {
			Total int64 `json:"total"`
		} `json:"shards"`
	} `json:"indices"`
}

type ESClusterSetting struct {
	Persistent struct {
	} `json:"persistent"`
	Transient struct {
	} `json:"transient"`
	Defaults struct {
		Indices struct {
			Breaker struct {
				Total struct {
					Limit string `json:"limit"`
				} `json:"total"`
			} `json:"breaker"`
		} `json:"indices"`
	} `json:"defaults"`
}

func execRequest(url, methodType string, requestBody io.Reader) ([]byte, error) {
	method := methodType

	client := &http.Client{}
	req, err := http.NewRequest(method, url, requestBody) // nosemgrep

	if err != nil {
		return nil, err
	}
	req.Header.Add("Content-Type", "application/json")

	res, err := client.Do(req)
	if err != nil {
		fmt.Println(err)
		return nil, err
	}
	defer res.Body.Close()
	body, err := ioutil.ReadAll(res.Body) // nosemgrep
	if err != nil {
		return nil, err
	}

	if res.StatusCode != http.StatusOK {
		return nil, errors.Errorf("Request failed with status %d\n%s\n", res.StatusCode, string(body))
	}
	return body, nil
}

func getSearchEngineBasePath() string {

	var basePath = "http://localhost:10144/"
	cfg := dc.DefaultAutomateConfig()
	defaultHost := cfg.GetEsgateway().GetV1().GetSys().GetService().GetHost().GetValue()
	defaultPort := cfg.GetEsgateway().GetV1().GetSys().GetService().GetPort().GetValue()

	if defaultHost != "" || defaultPort > 0 {
		basePath = fmt.Sprintf(`http://%s/`, net.JoinHostPort(defaultHost, fmt.Sprintf("%d", defaultPort)))
	}

	res, err := client.GetAutomateConfig(10)
	if err != nil {
		return basePath
	}

	host := res.Config.GetEsgateway().GetV1().GetSys().GetService().GetHost().GetValue()
	port := res.Config.GetEsgateway().GetV1().GetSys().GetService().GetPort().GetValue()

	if host != "" || port > 0 {
		url := net.JoinHostPort(host, fmt.Sprintf("%d", port))
		if url != "" {
			basePath = fmt.Sprintf(`http://%s/`, url)
		}
	}
	return basePath
}

func getClusterStats() (*IndicesShardTotal, error) {
	indicesShardTotal := &IndicesShardTotal{}
	basePath := getSearchEngineBasePath()
	totalShard, err := execRequest(basePath+"_cluster/stats?filter_path=indices.shards.total", "GET", nil)
	if err != nil {
		return indicesShardTotal, err
	}
	err = json.Unmarshal(totalShard, indicesShardTotal)
	if err != nil {
		return indicesShardTotal, err
	}
	return indicesShardTotal, nil
}

func getCusterSetting() (*ESClusterSetting, error) {
	clusterSetting := &ESClusterSetting{}
	basePath := getSearchEngineBasePath()
	allClusterSettings, err := execRequest(basePath+"_cluster/settings?include_defaults=true", "GET", nil)
	if err != nil {
		return clusterSetting, err
	}
	err = json.Unmarshal(allClusterSettings, clusterSetting)
	if err != nil {
		return clusterSetting, err
	}
	return clusterSetting, nil
}
func getElasticsearchPID() (string, error) {
	pid, err := ioutil.ReadFile("/hab/svc/automate-elasticsearch/PID") // nosemgrep
	if err != nil {
		return "", err
	}
	return string(pid), nil
}
func getProcessRuntimeSettings(pid string) (map[string]string, error) {
	var result = make(map[string]string)
	if pid != "" {
		pidInt64, err := strconv.ParseInt(strings.TrimSpace(string(pid)), 10, 64)
		if err != nil {
			return result, err
		}
		processProcFile := filepath.Join("/proc/", strconv.FormatInt(pidInt64, 10), "/limits")
		data, err := ioutil.ReadFile(processProcFile) // nosemgrep
		if err != nil {
			return result, err
		}
		var lines = strings.Split(string(data), "\n")
		for i := 1; i < len(lines); i++ {
			r := regexp.MustCompile("[^\\s]+")
			r.FindAllString(lines[i], -1)
			s := strings.Fields(lines[i])
			if len(s) >= 4 {
				if strings.TrimSpace(s[0]) == "Max" && strings.TrimSpace(s[1]) == "open" && strings.TrimSpace(s[2]) == "files" {
					result[MAX_OPEN_FILE_KEY] = s[3]
				}
				if strings.TrimSpace(s[0]) == "Max" && strings.TrimSpace(s[1]) == "locked" && strings.TrimSpace(s[2]) == "memory" {
					result[MAX_LOCKED_MEM_KEY] = s[3]
				}
			}
		}
	} else {
		return result, errors.New("Process id is empty")
	}

	return result, nil
}

func getHeapMemorySettings() (string, error) {
	basePath := getSearchEngineBasePath()
	heapMemorySettings, err := execRequest(basePath+"_cat/nodes?h=heap*&v", "GET", nil)
	if err != nil {
		return "", err
	}
	lines := strings.Split(string(heapMemorySettings), "\n")
	heapsize := strings.Fields(lines[1])[2]
	return heapsize, nil
}

// Supported search engines are Elastic Search and Open Search only as of now
func getAllSearchEngineSettings(searchEnginePid string) (*ESSettings, error) {
	clusterStats, err := getClusterStats()
	if err != nil {
		logrus.Debug("not able to fetch total shard values, moving to default", err.Error())
		clusterStats.Indices.Shards.Total = INDICES_TOTAL_SHARD_DEFAULT
	}
	clusterSettings, err := getCusterSetting()
	if err != nil {
		logrus.Debug("not able to fetch indices breaker total limit, moving to default", err.Error())
		clusterSettings.Defaults.Indices.Breaker.Total.Limit = INDICES_BREAKER_TOTAL_LIMIT_DEFAULT
	}
	runtimeSettings, err := getProcessRuntimeSettings(searchEnginePid)
	if err != nil {
		logrus.Debug("not able to fetch max open file and max locked memory, moving to default", err.Error())
		runtimeSettings[MAX_OPEN_FILE_KEY] = MAX_OPEN_FILE_DEFAULT
		runtimeSettings[MAX_LOCKED_MEM_KEY] = MAX_LOCKED_MEM_DEFAULT
	}
	heapmemSettings, err := getHeapMemorySettings()
	if err != nil {
		logrus.Debug("not able to fetch heap memory, moving to default", err.Error())
		heapmemSettings = defaultHeapSize()
	}
	esSettings := &ESSettings{}
	esSettings.HeapMemory = heapmemSettings
	esSettings.IndicesBreakerTotalLimit = clusterSettings.Defaults.Indices.Breaker.Total.Limit
	esSettings.TotalShardSettings = clusterStats.Indices.Shards.Total
	esSettings.RuntimeMaxOpenFile = runtimeSettings["Max open files"]
	esSettings.RuntimeMaxLockedMem = runtimeSettings["Max locked memory"]
	return esSettings, err
}

func defaultHeapSize() string {
	sysMem, err := sys.SystemMemoryKB()
	if err != nil {
		sysMem = 0
	}
	return fmt.Sprintf("%d", oss.RecommendedHeapSizeGB(sysMem))
}

func StoreESSettings(isEmbeded bool) error {
	if isEmbeded {
		pid, err := getElasticsearchPID()
		if err != nil {
			logrus.Debug("No process id found for running elasticsearch", err.Error())
		}
		logrus.Debug("fetching elastic search settings.")
		esSettings, err := getAllSearchEngineSettings(pid)
		if err != nil {
			return errors.Wrap(err, "error in fetching elastic search settings.")
		}
		esSettingsJson, err := json.Marshal(esSettings)
		if err != nil {
			return errors.Wrap(err, "error in mapping elastic search settings to json.")
		}
		logrus.Debug("writing json in file.")
		err = ioutil.WriteFile(V3ESSettingFile, esSettingsJson, 775) // nosemgrep
		if err != nil {
			return errors.Wrap(err, "error in writing json in file.")
		}
	}
	return nil
}

func PatchBestOpenSearchSettings(isEmbedded bool) error {
	if isEmbedded {
		bestSettings := GetBestSettings()
		temp := template.Must(template.New("init").
			Funcs(template.FuncMap{"StringsJoin": strings.Join}).
			Parse(patchOpensearchSettingsTemplate))

		var buf bytes.Buffer
		err := temp.Execute(&buf, bestSettings)
		if err != nil {
			return errors.Wrap(err, "some error occurred while rendering template")
		}
		finalTemplate := buf.String()
		err = ioutil.WriteFile(AutomateOpensearchConfigPatch, []byte(finalTemplate), 0600) // nosemgrep
		if err != nil {
			return errors.Wrap(err, fmt.Sprintf("Writing into file %s failed\n", AutomateOpensearchConfigPatch))
		}
		logrus.Debug(fmt.Sprintf("Config written %s to \n", AutomateOpensearchConfigPatch))
		defer func() {
			err := os.Remove(AutomateOpensearchConfigPatch)
			if err != nil {
				logrus.Debug(fmt.Sprintf("error in removing file %s", AutomateOpensearchConfigPatch), err.Error())
			}
			err = os.Remove(V3ESSettingFile)
			if err != nil {
				logrus.Debug(fmt.Sprintf("error in removing file %s", V3ESSettingFile), err.Error())
			}
		}()
		cmd := exec.Command("chef-automate", "config", "patch", AutomateOpensearchConfigPatch)
		cmd.Stdin = os.Stdin
		cmd.Stdout = io.MultiWriter(os.Stdout)
		cmd.Stderr = io.MultiWriter(os.Stderr)
		err = cmd.Run()
		if err != nil {
			return errors.Wrap(err, "Error in patching opensearch configuration")
		}
		logrus.Debug("config patch execution done, exiting\n")
	}
	return nil
}

func getBestHeapSetting(esSetting *ESSettings, ossSetting *ESSettings) string {
	esHeapSize, err := extractNumericFromText(esSetting.HeapMemory, 0)
	if err != nil {
		logrus.Debug("Not able to parse heapsize from elasticsearch settings", err.Error())
		return ossSetting.HeapMemory
	} else {
		ossHeap, _ := strconv.ParseFloat(ossSetting.HeapMemory, 64)
		if esHeapSize >= 32 || ossHeap >= 32 {
			return "32"
		} else if esHeapSize > ossHeap {
			return fmt.Sprintf("%d", int64(math.Round(esHeapSize)))
		} else {
			return fmt.Sprintf("%d", int64(math.Round(ossHeap)))
		}
	}
}

func getIndicesBreakerTotalLimitSetting(esSetting *ESSettings, ossSetting *ESSettings) string {
	esIndicesBreakerTotalLimit, err := extractNumericFromText(esSetting.IndicesBreakerTotalLimit, 0)
	if err != nil {
		logrus.Debug("Not able to parse indicesBreakerTotalLimit from opensearch settings", err.Error())
		return ossSetting.IndicesBreakerTotalLimit
	} else {
		ossIndicesBreakerTotalLimit, _ := strconv.ParseFloat(ossSetting.IndicesBreakerTotalLimit, 64)
		if esIndicesBreakerTotalLimit > ossIndicesBreakerTotalLimit {
			return fmt.Sprintf("%d", int64(math.Round(esIndicesBreakerTotalLimit)))
		} else {
			return fmt.Sprintf("%d", int64(math.Round(ossIndicesBreakerTotalLimit)))
		}
	}
}

func getRuntimeMaxOpenFile(esSetting *ESSettings, ossSetting *ESSettings) string {
	esRunTimeMaxOpenFile, err := extractNumericFromText(esSetting.RuntimeMaxOpenFile, 0)
	if err != nil {
		logrus.Debug("Not able to parse esRunTimeMaxOpenFile from opensearch settings", err.Error())
		return ossSetting.RuntimeMaxOpenFile
	} else {
		ossRunTimeMaxOpenFile, _ := strconv.ParseFloat(ossSetting.RuntimeMaxOpenFile, 64)
		if esRunTimeMaxOpenFile > ossRunTimeMaxOpenFile {
			return fmt.Sprintf("%d", int64(esRunTimeMaxOpenFile))
		} else {
			return fmt.Sprintf("%d", int64(ossRunTimeMaxOpenFile))
		}
	}
}

func getMaxLockedMemorySetting() string {
	return "unlimited"
}

func getTotalShardsSetting(esSetting *ESSettings, ossSetting *ESSettings) int64 {
	if esSetting.TotalShardSettings > ossSetting.TotalShardSettings {
		return esSetting.TotalShardSettings
	} else {
		return ossSetting.TotalShardSettings
	}
}

func GetBestSettings() *ESSettings {
	ossSetting := GetDefaultOSSettings()
	esSetting, err := getOldElasticSearchSettings()
	if err != nil {
		logrus.Debug("error in reading old es settigs\n", err)
		return ossSetting
	}
	bestSetting := &ESSettings{}
	bestSetting.HeapMemory = getBestHeapSetting(esSetting, ossSetting)
	bestSetting.IndicesBreakerTotalLimit = getIndicesBreakerTotalLimitSetting(esSetting, ossSetting)
	bestSetting.RuntimeMaxOpenFile = getRuntimeMaxOpenFile(esSetting, ossSetting)
	bestSetting.RuntimeMaxLockedMem = getMaxLockedMemorySetting()
	bestSetting.TotalShardSettings = getTotalShardsSetting(esSetting, ossSetting)
	return bestSetting
}

func extractNumericFromText(text string, index int) (float64, error) {
	re := regexp.MustCompile(`[-]?\d[\d,]*[\.]?[\d{2}]*`)
	submatchall := re.FindAllString(text, -1)
	if len(submatchall) < 1 {
		return 0, errors.New("No match found")
	}
	//fmt.Println(submatchall)
	numeric, err := strconv.ParseFloat(submatchall[index], 64)
	if err != nil {
		return 0, err
	}
	return numeric, nil
}

func getOldElasticSearchSettings() (*ESSettings, error) {
	esSetting := &ESSettings{}
	jsonData, err := ioutil.ReadFile(V3ESSettingFile) // nosemgrep
	if err != nil {
		return esSetting, err
	}
	err = json.Unmarshal(jsonData, esSetting)
	if err != nil {
		return esSetting, err
	}
	return esSetting, nil
}

func GetDefaultOSSettings() *ESSettings {
	defaultSettings := &ESSettings{}
	defaultSettings.HeapMemory = defaultHeapSize()
	defaultSettings.IndicesBreakerTotalLimit = INDICES_BREAKER_TOTAL_LIMIT_DEFAULT
	defaultSettings.RuntimeMaxLockedMem = MAX_LOCKED_MEM_DEFAULT
	defaultSettings.RuntimeMaxOpenFile = MAX_OPEN_FILE_DEFAULT
	defaultSettings.TotalShardSettings = INDICES_TOTAL_SHARD_DEFAULT
	return defaultSettings
}
