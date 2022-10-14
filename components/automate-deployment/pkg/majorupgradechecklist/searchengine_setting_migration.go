package majorupgradechecklist

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"math"
	"net"
	"net/http"
	"os"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
	"text/template"

	"github.com/pkg/errors"

	dc "github.com/chef/automate/api/config/deployment"
	oss "github.com/chef/automate/api/config/opensearch"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/lib/platform/sys"
)

const (
	MAX_OPEN_FILE_DEFAULT  = "65535"
	MAX_LOCKED_MEM_DEFAULT = "unlimited"
	MAX_OPEN_FILE_KEY      = "Max open files"
	MAX_LOCKED_MEM_KEY     = "Max locked memory"

	INDICES_TOTAL_SHARD_DEFAULT         = 2000
	INDICES_BREAKER_TOTAL_LIMIT_DEFAULT = "95"

	INDICES_TOTAL_SHARD_INCREMENT_DEFAULT = 500

	MAX_POSSIBLE_HEAP_SIZE = 32

	V3ESSettingFile               = "/hab/svc/deployment-service/old_es_v3_settings.json"
	AutomateOpensearchConfigPatch = "/hab/svc/deployment-service/oss-config.toml"

	heapSizeExceededError   = `heap size : %s, max allowed is (50%% of ram = %dgb) but not exceeding %dgb`
	shardCountExceededError = `total shards per node : %d, max allowed is %d, 
having this more than %d decreases perfomance to avoid breaching this limit, 
you can reduce data retention policy`
	errorUserConcent = `we recommend you to move to external/managed opensearch cluster for better performance.
but if you still want to continue with the upgrade`

	upgradeFailed = "due to pre-condition check failed"
)

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

type IndicesTotalLimit struct {
	Defaults struct {
		ClusterMaxShardsPerNode  string `json:"cluster.max_shards_per_node"`
		IndicesBreakerTotalLimit string `json:"indices.breaker.total.limit"`
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
	if res.StatusCode != http.StatusOK {
		return nil, errors.Errorf("url: %s not reachable", url)
	}
	body, err := ioutil.ReadAll(res.Body) // nosemgrep
	if err != nil {
		return nil, err
	}
	return body, nil
}

func getTotalShards() (*IndicesShardTotal, error) {
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

func getElasticsearchPID() (string, error) {
	pid, err := ioutil.ReadFile("/hab/svc/automate-elasticsearch/PID") // nosemgrep
	if err != nil {
		return "", err
	}
	return string(pid), nil
}

func getIndicesBreakerLimit() (*IndicesTotalLimit, error) {
	indicesTotalLimit := &IndicesTotalLimit{}
	basePath := getSearchEngineBasePath()
	data, err := getDataFromUrl(basePath + "_cluster/settings?include_defaults=true&flat_settings=true&pretty")
	if err != nil {
		return indicesTotalLimit, err
	}
	err = json.Unmarshal(data, indicesTotalLimit)
	if err != nil {
		return indicesTotalLimit, err
	}
	return indicesTotalLimit, nil
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
func getAllSearchEngineSettings(writer cli.FormatWriter, searchEnginePid string) (*ESSettings, error) {
	esSettings := &ESSettings{}
	totalShard, err := getTotalShards()
	if err != nil {
		writer.Warnf("not able to fetch total shard values, moving to default %s \n", err.Error())
		esSettings.TotalShardSettings = INDICES_TOTAL_SHARD_DEFAULT
	} else {
		esSettings.TotalShardSettings = totalShard.Indices.Shards.Total
	}
	maxShardPerNodeAndIndicesTotalLimit, err := getIndicesBreakerLimit()
	if err != nil {
		writer.Warnf("not able to fetch indices breaker total limit, moving to default \n %s \n", err.Error())
		esSettings.IndicesBreakerTotalLimit = INDICES_BREAKER_TOTAL_LIMIT_DEFAULT
	} else {
		esSettings.IndicesBreakerTotalLimit = maxShardPerNodeAndIndicesTotalLimit.Defaults.IndicesBreakerTotalLimit
	}
	heapMemSettings, err := getHeapMemorySettings()
	if err != nil {
		writer.Warnf("not able to fetch heap memory, moving to default \n %s \n", err.Error())
		esSettings.HeapMemory = fmt.Sprintf("%d", defaultHeapSizeInGB())
	} else {
		esSettings.HeapMemory = heapMemSettings
	}
	return esSettings, err
}

func defaultHeapSizeInGB() int {
	sysMem, err := sys.SystemMemoryKB()
	if err != nil {
		sysMem = 0
	}
	return oss.RecommendedHeapSizeGB(sysMem)
}

func GetESSettings(writer cli.FormatWriter) (*ESSettings, error) {
	pid, err := getElasticsearchPID()
	if err != nil {
		writer.Warnf("No process id found for running elasticsearch, %s \n", err.Error())
	}
	writer.Println("fetching elastic search settings.")
	esSettings, err := getAllSearchEngineSettings(writer, pid)
	if err != nil {
		return esSettings, err
	}
	return esSettings, nil
}

func storeESSettings(writer cli.FormatWriter, esSettings *ESSettings) error {
	esSettingsJson, err := json.Marshal(esSettings)
	if err != nil {
		return errors.Wrap(err, "error in mapping elasticsearch settings to json.")
	}
	writer.Println("writing elasticsearch settings in file.")
	err = ioutil.WriteFile(V3ESSettingFile, esSettingsJson, 775) // nosemgrep
	if err != nil {
		return errors.Wrap(err, "error in elasticsearch settings in file.")
	}
	return nil
}

func PatchBestOpenSearchSettings(writer cli.FormatWriter, isEmbedded bool) error {
	if isEmbedded {
		bestSettings := GetBestSettings(writer)
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
		writer.Println("below config will pached to automate opensearch")
		writer.Println(finalTemplate)
		writer.Printf(fmt.Sprintf("patch config written %s to \n", AutomateOpensearchConfigPatch))
		defer func() {
			err := os.Remove(AutomateOpensearchConfigPatch)
			if err != nil {
				writer.Warnf(fmt.Sprintf("error in removing file %s \n", AutomateOpensearchConfigPatch), err.Error())
			}
			err = os.Remove(V3ESSettingFile)
			if err != nil {
				writer.Warnf(fmt.Sprintf("error in removing file %s \n", V3ESSettingFile), err.Error())
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
		writer.Success("config patch execution done, exiting\n")
	}
	return nil
}

func getBestHeapSetting(writer cli.FormatWriter, esSetting *ESSettings, ossSetting *ESSettings) string {
	esHeapSize, err := extractNumericFromText(esSetting.HeapMemory, 0)
	if err != nil {
		writer.Warnf("Not able to parse heapsize from elasticsearch settings %s \n", err.Error())
		writer.Warnf("applying default heap size %s \n", ossSetting.HeapMemory)
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

func getIndicesBreakerTotalLimitSetting(writer cli.FormatWriter, esSetting *ESSettings, ossSetting *ESSettings) string {
	esIndicesBreakerTotalLimit, err := extractNumericFromText(esSetting.IndicesBreakerTotalLimit, 0)
	if err != nil {
		writer.Warnf("Not able to parse indicesBreakerTotalLimit from opensearch settings %s \n", err.Error())
		writer.Warnf("applying default indices breaker total limit %s \n", ossSetting.IndicesBreakerTotalLimit)
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

func getTotalShardsSetting(esSetting *ESSettings, ossSetting *ESSettings) int64 {
	if esSetting.TotalShardSettings > ossSetting.TotalShardSettings {
		return esSetting.TotalShardSettings
	} else {
		return ossSetting.TotalShardSettings
	}
}

func GetBestSettings(writer cli.FormatWriter) *ESSettings {
	ossSetting := GetDefaultOSSettings()
	esSetting, err := getOldElasticSearchSettings()
	if err != nil {
		writer.Warnf("error in reading old es settigs %s \n", err)
		return ossSetting
	}
	bestSetting := &ESSettings{}
	bestSetting.HeapMemory = getBestHeapSetting(writer, esSetting, ossSetting)
	bestSetting.IndicesBreakerTotalLimit = getIndicesBreakerTotalLimitSetting(writer, esSetting, ossSetting)
	bestSetting.RuntimeMaxOpenFile = ossSetting.RuntimeMaxOpenFile
	bestSetting.RuntimeMaxLockedMem = ossSetting.RuntimeMaxLockedMem
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
	defaultSettings.HeapMemory = fmt.Sprintf("%d", defaultHeapSizeInGB())
	defaultSettings.IndicesBreakerTotalLimit = INDICES_BREAKER_TOTAL_LIMIT_DEFAULT
	defaultSettings.RuntimeMaxLockedMem = MAX_LOCKED_MEM_DEFAULT
	defaultSettings.RuntimeMaxOpenFile = MAX_OPEN_FILE_DEFAULT
	defaultSettings.TotalShardSettings = INDICES_TOTAL_SHARD_DEFAULT
	return defaultSettings
}
