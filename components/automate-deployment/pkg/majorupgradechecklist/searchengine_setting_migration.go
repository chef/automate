package majorupgradechecklist

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"html/template"
	"io"
	"io/ioutil"
	"math"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"

	oss "github.com/chef/automate/api/config/opensearch"
	"github.com/chef/automate/lib/platform/sys"
	"github.com/sirupsen/logrus"
)

const MAX_OPEN_FILE_DEFAULT = "65535"
const MAX_LOCKED_MEM_DEFAULT = "unlimited"

const MAX_OPEN_FILE_KEY = "Max open files"
const MAX_LOCKED_MEM_KEY = "Max locked memory"

const INDICES_TOTAL_SHARD_DEFAULT = 6000
const INDICES_BREAKER_TOTAL_LIMIT_DEFAULT = "95%"

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

func getDataFromUrl(url string) ([]byte, error) {
	method := "GET"

	client := &http.Client{}
	req, err := http.NewRequest(method, url, nil) // nosemgrep

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
	return body, nil
}

func getSearchEngineBasePath() string {
	var basePath = "http://localhost:10144/"

	habpath := getHabRootPath(habrootcmd)

	input, err := ioutil.ReadFile(habpath + "svc/automate-es-gateway/config/URL") // nosemgrep
	if err != nil {
		fmt.Printf("Failed to read URL file")
	}
	url := strings.TrimSuffix(string(input), "\n")
	if url != "" {
		basePath = "http://" + url + "/"
	}
	return basePath
}

func getClusterStas() (*IndicesShardTotal, error) {
	indicesShardTotal := &IndicesShardTotal{}
	basePath := getSearchEngineBasePath()
	totalShard, err := getDataFromUrl(basePath + "_cluster/stats?filter_path=indices.shards.total")
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
	allClusterSettings, err := getDataFromUrl(basePath + "_cluster/settings?include_defaults=true")
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
	heapMemorySettings, err := getDataFromUrl(basePath + "_cat/nodes?h=heap*&v")
	if err != nil {
		return "", err
	}
	lines := strings.Split(string(heapMemorySettings), "\n")
	heapsize := strings.Fields(lines[1])[2]
	return heapsize, nil
}

// Supported search engines are Elastic Search and Open Search only as of now
func getAllSearchEngineSettings(searchEnginePid string) (*ESSettings, error) {
	clusterStats, err := getClusterStas()
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
	return fmt.Sprintf("%dg", oss.RecommendedHeapSizeGB(sysMem))
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
			logrus.Debug("error in fetching elastic search settings.", err.Error())
		}
		esSettingsJson, err := json.Marshal(esSettings)
		if err != nil {
			logrus.Debug("error in mapping elastic search settings to json.", err.Error())
		}
		logrus.Debug("writing json in file.")
		err = ioutil.WriteFile(V3ESSettingFile, esSettingsJson, 775) // nosemgrep
		if err != nil {
			logrus.Debug("error in writing json in file.", err.Error())
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
			logrus.Debug("some error occurred while rendering template", err.Error())
		}
		finalTemplate := buf.String()
		err = ioutil.WriteFile(AutomateOpensearchConfigPatch, []byte(finalTemplate), 0600) // nosemgrep
		if err != nil {
			logrus.Debug(fmt.Sprintf("Writing into file %s failed\n", AutomateOpensearchConfigPatch), err.Error())
			return err
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
			logrus.Debug("Error in patching opensearch configuration", err.Error())
		}
		logrus.Debug("config patch execution done, exiting\n")
	}
	return nil
}

func GetBestSettings() *ESSettings {
	esSetting, err := getOldElasticSearchSettings()
	if err != nil {
		fmt.Println(err)
	}
	ossSetting := GetDefaultOSSettings()
	bestSetting := &ESSettings{}

	esHeapSize, err := extractNumericFromText(esSetting.HeapMemory, 0)
	if err != nil {
		logrus.Debug("Not able to parse heapsize from elasticsearch settings", err.Error())
		bestSetting.HeapMemory = ossSetting.HeapMemory
	} else {
		ossHeapSize, err := extractNumericFromText(ossSetting.HeapMemory, 0)
		if err != nil {
			logrus.Debug("Not able to parse heapsize from opensearch settings", err.Error())
			bestSetting.HeapMemory = ossSetting.HeapMemory
		} else {
			if esHeapSize >= 32 || ossHeapSize >= 32 {
				bestSetting.HeapMemory = "32"
			} else if esHeapSize > ossHeapSize {
				bestSetting.HeapMemory = fmt.Sprintf("%d", int64(math.Round(esHeapSize)))
			} else {
				bestSetting.HeapMemory = fmt.Sprintf("%d", int64(math.Round(ossHeapSize)))
			}
		}
	}

	esIndicesBreakerTotalLimit, err := extractNumericFromText(esSetting.IndicesBreakerTotalLimit, 0)
	if err != nil {
		logrus.Debug("Not able to parse indicesBreakerTotalLimit from opensearch settings", err.Error())
		bestSetting.IndicesBreakerTotalLimit = ossSetting.IndicesBreakerTotalLimit
	} else {
		ossIndicesBreakerTotalLimit, err := extractNumericFromText(ossSetting.IndicesBreakerTotalLimit, 0)
		if err != nil {
			logrus.Debug("Not able to parse indicesBreakerTotalLimit from opensearch settings", err.Error())
			bestSetting.IndicesBreakerTotalLimit = ossSetting.IndicesBreakerTotalLimit
		} else {
			if esIndicesBreakerTotalLimit > ossIndicesBreakerTotalLimit {
				bestSetting.IndicesBreakerTotalLimit = fmt.Sprintf("%d", int64(math.Round(esIndicesBreakerTotalLimit)))
			} else {
				bestSetting.IndicesBreakerTotalLimit = fmt.Sprintf("%d", int64(math.Round(ossIndicesBreakerTotalLimit)))
			}
		}
	}

	esRunTimeMaxOpenFile, err := extractNumericFromText(esSetting.RuntimeMaxOpenFile, 0)
	if err != nil {
		logrus.Debug("Not able to parse esRunTimeMaxOpenFile from opensearch settings", err.Error())
		bestSetting.RuntimeMaxOpenFile = ossSetting.RuntimeMaxOpenFile
	} else {
		ossRunTimeMaxOpenFile, err := extractNumericFromText(ossSetting.RuntimeMaxOpenFile, 0)
		if err != nil {
			logrus.Debug("Not able to parse ossRunTimeMaxOpenFile from opensearch settings", err.Error())
			bestSetting.RuntimeMaxOpenFile = ossSetting.RuntimeMaxOpenFile
		} else {
			if esRunTimeMaxOpenFile > ossRunTimeMaxOpenFile {
				bestSetting.RuntimeMaxOpenFile = fmt.Sprintf("%d", int64(esRunTimeMaxOpenFile))
			} else {
				bestSetting.RuntimeMaxOpenFile = fmt.Sprintf("%d", int64(ossRunTimeMaxOpenFile))
			}
		}
	}

	if esSetting.TotalShardSettings > ossSetting.TotalShardSettings {
		bestSetting.TotalShardSettings = esSetting.TotalShardSettings
	} else {
		bestSetting.TotalShardSettings = ossSetting.TotalShardSettings
	}

	if esSetting.RuntimeMaxLockedMem == "unlimited" || ossSetting.RuntimeMaxLockedMem == "unlimited" {
		bestSetting.RuntimeMaxLockedMem = "unlimited"
	} else {
		esRunTimeMaxLockedMem, err := extractNumericFromText(esSetting.RuntimeMaxLockedMem, 0)
		if err != nil {
			logrus.Debug("Not able to parse esRunTimeMaxLockedMem from opensearch settings", err.Error())
			bestSetting.RuntimeMaxLockedMem = ossSetting.RuntimeMaxLockedMem
		} else {
			ossRunTimeMaxLockedMem, err := extractNumericFromText(ossSetting.RuntimeMaxLockedMem, 0)
			if err != nil {
				logrus.Debug("Not able to parse ossRunTimeMaxLockedMem from opensearch settings", err.Error())
				bestSetting.RuntimeMaxLockedMem = ossSetting.RuntimeMaxLockedMem
			} else {
				if esRunTimeMaxLockedMem > ossRunTimeMaxLockedMem {
					bestSetting.RuntimeMaxLockedMem = fmt.Sprintf("%d", int64(math.Round(esRunTimeMaxLockedMem)))
				} else {
					bestSetting.RuntimeMaxLockedMem = fmt.Sprintf("%d", int64(math.Round(ossRunTimeMaxLockedMem)))
				}
			}
		}
	}
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
