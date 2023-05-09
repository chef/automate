package trigger

import (
	"encoding/json"
	"fmt"
	"github.com/chef/automate/lib/logger"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
)

func RunCheck(config models.Config, log logger.Logger, port string, path string) map[string]models.CheckTriggerResponse {
	result := make(map[string]models.CheckTriggerResponse)
	count := config.Hardware.AutomateNodeCount +
		config.Hardware.ChefInfraServerNodeCount +
		config.Hardware.PostgresqlNodeCount +
		config.Hardware.OpenSearchNodeCount

	outputCh := make(chan models.CheckTriggerResponse, count)

	ipNodeMap := fiberutils.ConstructIpAndNodeTypeMap(config)

	for ip, nodeType := range ipNodeMap {
		go triggerCheckAPI(fiberutils.HostToEndPoint(ip, port, path, nodeType), host, outputCh)
	}

	for i := 0; i < count; i++ {
		select {
		case res := <-outputCh:
			result[res.Host] = res
		}
	}
	close(outputCh)
	fmt.Printf("result: %+v\n", result)
	return result
}

func triggerCheckAPI(endPoint, host string, output chan<- models.CheckTriggerResponse) { //Chan
	var ctr models.CheckTriggerResponse

	resp, err := http.Get(endPoint)
	if err != nil {
		errMsg := fmt.Sprintf("error triggering the API %s: %s", endPoint, err.Error())
		fiberutils.SendError(endPoint, errMsg, http.StatusBadRequest, output, ctr)
		return
	}

	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		errMsg := fmt.Sprintf("error triggering the API %s: status code %d ", endPoint, resp.StatusCode)
		fiberutils.SendError(endPoint, errMsg, resp.StatusCode, output, ctr)
		return
	}

	if err := json.NewDecoder(resp.Body).Decode(&ctr); err != nil {
		errMsg := fmt.Sprintf("error decoding response from the API %s: %s ", endPoint, err.Error())
		fiberutils.SendError(endPoint, errMsg, http.StatusInternalServerError, output, ctr)
		return
	}

	//host, _ := fiberutils.GetHostFormEndPoint(endPoint)
	ctr.Host = host
	output <- ctr
}
