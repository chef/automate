package majorupgradechecklist

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"net/http"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"

	oss "github.com/chef/automate/api/config/opensearch"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/lib/platform/sys"
	"github.com/sirupsen/logrus"
)

type Checklist struct {
	Name        string
	Description string
	TestFunc    func(ChecklistHelper) error
}

type ChecklistHelper struct {
	Writer cli.FormatWriter
}

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

const MAX_OPEN_FILE_DEFAULT = "65535"
const MAX_LOCKED_MEM_DEFAULT = "unlimited"

const MAX_OPEN_FILE_KEY = "Max open files"
const MAX_LOCKED_MEM_KEY = "Max locked memory"

const INDICES_TOTAL_SHARD_DEFAULT = 6000
const INDICES_BREAKER_TOTAL_LIMIT_DEFAULT = "95%"

var V3ESSettingFile string = "/hab/svc/deployment-service/old_es_v3_settings.json"
var AutomateOpensearchConfigPatch string = "/hab/svc/deployment-service/oss-config.toml"

func ReadJsonFile(path string) (*PostChecklist, error) {
	byteValue, err := ioutil.ReadFile(path) // nosemgrep
	if err != nil {
		return nil, err
	}
	params := PostChecklist{}

	err = json.Unmarshal(byteValue, &params)
	if err != nil {
		return nil, err
	}
	return &params, nil
}

func CreateJsonFile(params *PostChecklist, path string) error {
	var buffer bytes.Buffer
	data, err := json.Marshal(*params)
	if err != nil {
		return err
	}
	buffer.Write(data)
	buffer.WriteString("\n")
	err = ioutil.WriteFile(path, buffer.Bytes(), 0644) // nosemgrep
	if err != nil {
		return err
	}
	return nil
}

func GetMajorVersion(version string) (string, bool) {
	resp, is_major_version := manifest.IsSemVersionFmt(version)
	if is_major_version {
		return resp, is_major_version
	}
	return version, false
}

func IsExternalElasticSearch(writer cli.FormatWriter) bool {
	res, err := client.GetAutomateConfig(int64(client.DefaultClientTimeout))
	if err != nil {
		logrus.Error("failed to get elastic search configuration: ", err.Error())
		return false
	}
	return res.Config.GetGlobal().GetV1().GetExternal().GetElasticsearch().GetEnable().GetValue()
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
	pid, err := ioutil.ReadFile("/hab/svc/automate-elasticsearch/PID")
	if err != nil {
		return "", err
	}
	return string(pid), nil
}
func getOpenSearchPID() (string, error) {
	pid, err := ioutil.ReadFile("/hab/svc/automate-opensearch/PID")
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
		data, err := ioutil.ReadFile(processProcFile)
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
		fmt.Println(err)
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
		fmt.Println(err)
		clusterStats.Indices.Shards.Total = INDICES_TOTAL_SHARD_DEFAULT
	}
	clusterSettings, err := getCusterSetting()
	if err != nil {
		fmt.Println(err)
		clusterSettings.Defaults.Indices.Breaker.Total.Limit = INDICES_BREAKER_TOTAL_LIMIT_DEFAULT
	}
	runtimeSettings, err := getProcessRuntimeSettings(searchEnginePid)
	if err != nil {
		fmt.Println(err)
		runtimeSettings[MAX_OPEN_FILE_KEY] = MAX_OPEN_FILE_DEFAULT
		runtimeSettings[MAX_LOCKED_MEM_KEY] = MAX_LOCKED_MEM_DEFAULT
	}
	heapmemSettings, err := getHeapMemorySettings()
	if err != nil {
		fmt.Println(err)
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
