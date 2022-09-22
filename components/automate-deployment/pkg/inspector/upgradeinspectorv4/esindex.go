package upgradeinspectorv4

import (
	"encoding/json"
	"fmt"
	"strconv"
	"strings"
	"time"

	"github.com/briandowns/spinner"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/inspector"
	"github.com/fatih/color"
	"github.com/pkg/errors"
)

const (
	INDEX_BATCH_SIZE int    = 10
	MSG_ES_CHECKING  string = "Elasticsearch indices are in version 6"
)

// remove .saved-searches
var automateOldIndicesPattern []string = []string{".automate", ".locky", "saved-searches", ".tasks"}

type ESIndexInspection struct {
	writer             *cli.Writer
	upgradeUtils       UpgradeV4Utils
	esBasePath         string
	spinner            *spinner.Spinner
	automateOldIndices []string
	otherOldIndices    []string
}

func NewESIndexInspection(w *cli.Writer, utls UpgradeV4Utils, esBasePath string) *ESIndexInspection {
	return &ESIndexInspection{
		writer:       w,
		upgradeUtils: utls,
		esBasePath:   esBasePath,
	}
}

func (es *ESIndexInspection) ShowInfo(index *int) error {
	return nil
}

func (es *ESIndexInspection) Inspect() (err error) {
	es.showChecking()
	es.automateOldIndices, es.otherOldIndices, err = es.fetchOldIndicesInES()
	if err != nil {
		es.showError()
		return err
	}
	if !es.hasOldIndices() {
		es.showSuccess()
		return nil
	}
	es.showError()
	es.showErrorList()
	shouldDelete := es.promptForDeletion()
	if !shouldDelete {
		return errors.New(USER_TERMINATED)
	}
	err = es.deleteOldIndices()
	if err != nil {
		return err
	}
	es.deletedSuccessfully()
	return nil
}

func (es *ESIndexInspection) showSuccess() {
	es.spinner.FinalMSG = fmt.Sprintf(color.New(color.FgGreen).Sprint("✔") + "  [" + color.New(color.FgGreen).Sprint("Passed") +
		"]\t" + MSG_ES_CHECKING)
	es.spinner.Stop()
	es.writer.Println("")
}

func (es *ESIndexInspection) promptForDeletion() bool {
	msg := `                Please choose from options below:
                1. Delete these indices and proceed with upgrade.
                2. Exit the upgrade process, manually re-index the indices and upgrade Chef Automate later on.

                For more information on reindexing, visit: https://www. elastic.co/guide/en/elasticsearch/reference/6.8/docs-reindex.html
`
	es.writer.Println(msg)
	return es.shouldDelete()
}

func (es *ESIndexInspection) deleteOldIndices() error {
	allIndices := []string{}
	allIndices = append(allIndices, es.automateOldIndices...)
	allIndices = append(allIndices, es.otherOldIndices...)
	return es.batchDeleteOldIndices(allIndices, es.esBasePath)
}

func (es *ESIndexInspection) batchDeleteOldIndices(indexList []string, basePath string) error {
	additionalBatch := 0
	if len(indexList)%INDEX_BATCH_SIZE > 0 {
		additionalBatch = 1
	}
	numOfBatches := len(indexList)/INDEX_BATCH_SIZE + additionalBatch
	for i := 0; i < numOfBatches; i++ {
		upper := i*INDEX_BATCH_SIZE + INDEX_BATCH_SIZE
		if upper > len(indexList)-1 {
			upper = len(indexList)
		}
		indexCSL := strings.Join(indexList[i*INDEX_BATCH_SIZE:upper], ",")
		_, err := es.upgradeUtils.ExecRequest(fmt.Sprintf("%s%s", basePath, indexCSL)+"?pretty", "DELETE", nil)
		if err != nil {
			return err
		}
	}
	return nil
}

func (es *ESIndexInspection) deletedSuccessfully() {
	es.writer.Println("                " + color.New(color.FgGreen).Sprint("✔") + " Old Elasticsearch indices deleted successfully\n")
	es.writer.Println(color.New(color.FgGreen).Sprint("✔") + "  [" + color.New(color.FgGreen).Sprint("Passed") +
		"]\t" + MSG_ES_CHECKING)
}

func (es *ESIndexInspection) shouldDelete() bool {
	choice, err := es.writer.Prompt("                Enter your choice (1/2)")
	if err != nil {
		es.writer.Println(err.Error())
	}
	switch choice {
	case "1":
		return true
	case "2":
		return false
	default:
		es.writer.Printf("                I don't understand '%s'. Please type '1' or '2'\n", choice)
		return es.shouldDelete()
	}
}

func (es *ESIndexInspection) hasOldIndices() bool {
	if len(es.automateOldIndices) > 0 || len(es.otherOldIndices) > 0 {
		return true
	}
	return false
}

func (es *ESIndexInspection) showError() {
	es.spinner.FinalMSG = color.New(color.FgRed).Sprint("✖") + "  [" + color.New(color.FgRed).Sprint("Failed") + "]\t" + MSG_ES_CHECKING
	es.spinner.Stop()
	es.writer.Println("")
}

func (es *ESIndexInspection) showErrorList() {
	es.writer.Println("")
	if len(es.automateOldIndices) > 0 {
		es.showErrorListOldAutomateIndices()

	}
	if len(es.otherOldIndices) > 0 {
		es.showErrorListOldOtherIndices()
	}
}

func (es *ESIndexInspection) showErrorListOldAutomateIndices() {
	es.writer.Println("                [" + color.New(color.FgRed).Sprint("Error") + "] Below indices are from an older version of Elasticsearch from Chef Automate 1")
	for _, a1 := range es.automateOldIndices {
		es.writer.Println("                " + "        " + a1)
	}
	es.writer.Println("")
}

func (es *ESIndexInspection) showErrorListOldOtherIndices() {
	es.writer.Println("                [" + color.New(color.FgRed).Sprint("Error") + "] Below indices are from an older version of Elasticsearch")
	for _, oi := range es.otherOldIndices {
		es.writer.Println("                " + "        " + oi)
	}
	es.writer.Println("")
}

func (es *ESIndexInspection) showChecking() {
	es.spinner = es.writer.NewSpinner()
	es.spinner.Suffix = fmt.Sprintf("  [Checking]\tElasticsearch indices are in version 6")
	es.spinner.Start()
	time.Sleep(time.Second)
}

func (es *ESIndexInspection) fetchOldIndicesInES() (automateOldIndices, otherOldIndices []string, err error) {
	listIndexData, err := es.fetchOldIndexInfo()
	if err != nil {
		return nil, nil, errors.Wrap(err, "error while getting list of indices.")
	}
	automateOldIndices, otherOldIndices = es.categoriseIndices(listIndexData)
	return
}

func (es *ESIndexInspection) Skip() {
	es.writer.Println("⊖  [Skipped]\tElasticsearch indices are in version 6")
}

func (es *ESIndexInspection) GetShortInfo() []string {
	return []string{"Elasticsearch indices are in version 6"}
}

func (es *ESIndexInspection) getAllIndices() ([]byte, error) {
	return es.upgradeUtils.ExecRequest(es.esBasePath+"_cat/indices?h=index", "GET", nil)
}

func (es *ESIndexInspection) fetchOldIndexInfo() ([]IndexData, error) {
	allIndexList, err := es.getAllIndices()
	if err != nil {
		return nil, err
	}
	indexList := strings.Split(strings.TrimSuffix(string(allIndexList), "\n"), "\n")
	additionalBatch := 0
	if len(indexList)%INDEX_BATCH_SIZE > 0 {
		additionalBatch = 1
	}
	numOfBatches := len(indexList)/INDEX_BATCH_SIZE + additionalBatch
	var indexDataArr []IndexData
	for i := 0; i < numOfBatches; i++ {
		upper := i*INDEX_BATCH_SIZE + INDEX_BATCH_SIZE
		if upper > len(indexList)-1 {
			upper = len(indexList)
		}
		indexCSL := strings.Join(indexList[i*INDEX_BATCH_SIZE:upper], ",")
		versionData, err := es.upgradeUtils.ExecRequest(es.esBasePath+indexCSL+"/_settings/index.version.created*?&human", "GET", nil)
		if err != nil {
			return nil, errors.Wrap(err, "error while getting indices details.")
		}
		data, err := es.getDataForOldIndices(versionData)
		if err != nil {
			return nil, errors.Wrap(err, "error while parsing indices details.")
		}
		indexDataArr = append(indexDataArr, data...)
	}
	return indexDataArr, nil
}

func (es *ESIndexInspection) getDataForOldIndices(allIndexData []byte) ([]IndexData, error) {
	var indexDataArray []IndexData
	var parsed map[string]IndexInfo
	err := json.Unmarshal(allIndexData, &parsed)
	if err != nil {
		return nil, errors.Wrap(err, "error in unmarshalling the index data")
	}
	for key, data := range parsed {
		index, err := strconv.ParseInt(data.Settings.Index.Version.CreatedString[0:1], 10, 64)
		if err != nil {
			return nil, errors.Wrap(err, "failed to parse index version")
		}
		if index < 6 && key != ".watches" {
			indexDataArray = append(indexDataArray,
				IndexData{Name: key, MajorVersion: index,
					CreatedString: data.Settings.Index.Version.CreatedString})
		}
	}
	return indexDataArray, nil
}

func (es *ESIndexInspection) categoriseIndices(allIndices []IndexData) (automateIndices, otherIndices []string) {
	automateIndicesMap := make(map[string]bool)
	othersIndicesMap := make(map[string]bool)
	for _, item := range allIndices {
		isAutomateIndex := false
		for _, sourceItem := range automateOldIndicesPattern {
			if strings.Contains(item.Name, sourceItem) {
				automateIndicesMap[item.Name] = true
				isAutomateIndex = true
				break
			}
		}
		if !isAutomateIndex {
			othersIndicesMap[item.Name] = true
		}
	}

	for key := range automateIndicesMap {
		automateIndices = append(automateIndices, key)
	}
	for key := range othersIndicesMap {
		otherIndices = append(otherIndices, key)
	}
	return
}

func (es *ESIndexInspection) GetInstallationType() inspector.InstallationType {
	return inspector.EMBEDDED
}

type IndexData struct {
	Name          string
	MajorVersion  int64
	CreatedString string
	IsDeleted     bool
}

type IndexInfo struct {
	Settings struct {
		Index struct {
			Version struct {
				CreatedString string `json:"created_string"`
				Created       string `json:"created"`
			} `json:"version"`
		} `json:"index"`
	} `json:"settings"`
}
