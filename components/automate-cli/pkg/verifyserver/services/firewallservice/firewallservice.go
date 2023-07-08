package firewallservice

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"strconv"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/response"
	"github.com/chef/automate/lib/logger"
)

type IFirewallService interface {
	GetFirewallDetails(models.FirewallRequest) models.FirewallResponse
}

type FirewallService struct {
	log     logger.Logger
	timeout time.Duration
	port    string
}

func NewFirewallService(log logger.Logger, timeout time.Duration, port string) *FirewallService {
	return &FirewallService{
		log:     log,
		timeout: timeout,
		port:    port,
	}
}

func (fw *FirewallService) GetFirewallDetails(reqBody models.FirewallRequest) models.FirewallResponse {
	resp := models.FirewallResponse{}

	reachableCheck := fw.checkReachability(reqBody)
	resp.Checks = append(resp.Checks, reachableCheck)

	resp.Passed = true
	for _, check := range resp.Checks {
		if !check.Passed {
			resp.Passed = false
			break
		}
	}

	return resp
}

func (fw *FirewallService) checkReachability(req models.FirewallRequest) models.Checks {
	client := &http.Client{
		Timeout: fw.timeout * time.Second,
	}

	respBody, err := fw.triggerRequest(req, client)
	if err != nil || !respBody.Passed {
		// this means while triggering the request we encountered some error
		fw.log.Error("Error: ", err)
		// this means we successfully made the call. but the port-reachable api returns that the port is not reachable
		// hence from firewall api we are returning the same
		fw.log.Error("port-reachable api result: ", respBody.Passed)

		errorMsg := GetFirewallErrorMsg(req.DestinationServiceProtocol, req.DestinationNodeIP, req.DestinationServicePort, req.SourceNodeIP)
		resolutionMsg := GetFirewallResolutionMsg(req.DestinationNodeIP, req.DestinationServicePort, req.SourceNodeIP)
		return createFirewallCheck(false, "", errorMsg, resolutionMsg)
	}

	fw.log.Debug("Response From port-reachable API: ", respBody)
	return createFirewallCheck(true, GetFirewallSuccessMsg(req.DestinationServiceProtocol, req.DestinationNodeIP, req.DestinationServicePort, req.SourceNodeIP), "", "")
}

func (fw *FirewallService) triggerRequest(req models.FirewallRequest, client *http.Client) (models.Checks, error) {
	fw.log.Debug("triggerRequest Called...")
	url := fmt.Sprintf("http://%s:%s%s", req.SourceNodeIP, fw.port, constants.PORT_REACHABLE_API_PATH)
	fw.log.Debug("URL: ", url)

	destinationPort, err := strconv.Atoi(req.DestinationServicePort)
	if err != nil {
		return models.Checks{}, errors.New("Failed to convert string ip into int: " + err.Error())
	}

	httpReqBody := models.PortReachableRequest{
		DestinationNodeIp:              req.DestinationNodeIP,
		DestinationNodePort:            destinationPort,
		DestinationNodeServiceProtocol: req.DestinationServiceProtocol,
		RootCA:                         req.RootCert,
	}
	httpReqBodyJSON, err := json.Marshal(httpReqBody)
	if err != nil {
		return models.Checks{}, errors.New("Failed to Marshal the request for Port reachability API: " + err.Error())
	}

	httpReq, err := http.NewRequest(http.MethodPost, url, bytes.NewBuffer(httpReqBodyJSON))
	if err != nil {
		return models.Checks{}, errors.New("Error creating request for Port reachability API: " + err.Error())
	}
	httpReq.Header.Set(constants.CONTENT_TYPE, constants.TYPE_JSON)

	resp, err := client.Do(httpReq)
	if err != nil {
		return models.Checks{}, errors.New("Error sending request to Port reachability API: " + err.Error())
	}

	return fw.getResultStructFromRespBody(resp.Body)
}

func (fw *FirewallService) getResultStructFromRespBody(respBody io.Reader) (models.Checks, error) {
	fw.log.Debug("getResultStructFromRespBody called...")
	body, err := io.ReadAll(respBody)
	if err != nil {
		return models.Checks{}, errors.New("Cannot able to read data from response body: " + err.Error())
	}

	// Converting API Response Body into Generic Response Struct.
	apiRespStruct := response.ResponseBody{}
	err = json.Unmarshal(body, &apiRespStruct)
	if err != nil {
		return models.Checks{}, errors.New("Failed to Unmarshal the response from Port reachability API: " + err.Error())
	}

	// If API(/port-reachable) is itself failing.
	if apiRespStruct.Error != nil {
		return models.Checks{}, apiRespStruct.Error
	}

	// Converting interface into JSON encoding. apiResp.Result is a interface and for accessing the values we are converting that into json.
	resultByte, err := json.Marshal(apiRespStruct.Result)
	if err != nil {
		return models.Checks{}, errors.New("Failed to Marshal the response from Port reachability API: " + err.Error())
	}

	resultField := models.Checks{}
	// converting JSON into struct.
	err = json.Unmarshal(resultByte, &resultField)
	if err != nil {
		return models.Checks{}, errors.New("Failed to Unmarshal the result struct from Port reachability API response: " + err.Error())
	}

	return resultField, nil
}

func GetFirewallSuccessMsg(service, destinationIP, destinationPort, sourceIP string) string {
	return fmt.Sprintf("The %s service running on %s:%s is reachable from %s", service, destinationIP, destinationPort, sourceIP)
}

func GetFirewallErrorMsg(service, destinationIP, destinationPort, sourceIP string) string {
	return fmt.Sprintf("The %s service running on %s:%s is not reachable from %s", service, destinationIP, destinationPort, sourceIP)
}

func GetFirewallResolutionMsg(destinationIP, destinationPort, sourceIP string) string {
	return fmt.Sprintf("Check your firewall settings to provide access on %s port at %s from %s", destinationPort, destinationIP, sourceIP)
}

func createFirewallCheck(passed bool, successMsg, errorMsg, resolutionMsg string) models.Checks {
	return models.Checks{
		Title:         constants.FIREWALL_TITLE,
		Passed:        passed,
		SuccessMsg:    successMsg,
		ErrorMsg:      errorMsg,
		ResolutionMsg: resolutionMsg,
	}
}
