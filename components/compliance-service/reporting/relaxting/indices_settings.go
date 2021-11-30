package relaxting

import (
	"context"
	"fmt"
	"strconv"
	"strings"

	"github.com/pkg/errors"
	elastic "gopkg.in/olivere/elastic.v6"
)

func GetMaxInnerResultWindow(backend ES2Backend) (int64, error) {
	esClient, err := backend.ES2Client()
	if err != nil {
		return 0, errors.Wrap(err, "failed in getting indices settings")
	}

	settingService := elastic.NewIndicesGetSettingsService(esClient)
	resp, err := settingService.Index("comp-*-r-*").Name("index.max_inner_result_window").Do(context.Background())
	if err != nil {
		return 0, errors.Wrap(err, "failed in getting indices settings")
	}
	// if the `max_inner_result_window` is not explicitly set, then empty response will be received
	if len(resp) == 0 {
		return 0, nil
	}
	//get the setting value from response
	// since the response has all indices info, process the first item result
	var resultMap map[string]interface{}
	for _, res := range resp {
		resultMap = res.Settings
		break
	}
	indexSettings, ok := resultMap["index"]
	if !ok {
		return 0, nil
	}
	converted := indexSettings.(map[string]interface{})
	interfaceVal, ok := converted["max_inner_result_window"]
	if !ok {
		return 0, nil
	}
	val, err := strconv.ParseInt(interfaceVal.(string), 10, 64)
	if err != nil {
		return 0, errors.Wrap(err, "error in converting setting value to int64")
	}
	return val, nil
}

func SetMaxInnerResultWindow(backend ES2Backend) error {
	esClient, err := backend.ES2Client()
	if err != nil {
		return errors.Wrap(err, "failed in setting indices settings")
	}

	settingService := elastic.NewIndicesPutSettingsService(esClient)
	body := `{
		"index":{
			"max_inner_result_window":"10000"
		}
	}`
	resp, err := settingService.Index("comp-*-r-*").BodyString(body).Do(context.Background())
	//if the error is index_not_found_exception, we can skip the error
	if err != nil && strings.Contains(err.Error(), "index_not_found_exception") && strings.Contains(err.Error(), "Error 404") {
		return nil
	}
	if err != nil {
		return errors.Wrap(err, "error in updating the indices settings")
	}
	if !resp.Acknowledged {
		return fmt.Errorf("updating the indices not successful")
	}
	return nil
}
