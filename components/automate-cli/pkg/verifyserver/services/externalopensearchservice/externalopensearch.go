package externalopensearchservice

import (
	"crypto/tls"
	"crypto/x509"
	"errors"
	"fmt"
	"io"
	"net/http"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
)

type IExternalOpensearchService interface {
	GetExternalOpensearchDetails(models.ExternalOSRequest) models.ExternalOpensearchResponse
}

type ExternalOpensearchService struct {
	log     logger.Logger
	timeout time.Duration
}

func NewExternalOpensearchService(log logger.Logger, timeout time.Duration) IExternalOpensearchService {
	return &ExternalOpensearchService{
		log:     log,
		timeout: timeout,
	}
}

func (eos *ExternalOpensearchService) checkReachability(reqBody models.ExternalOSRequest) models.ExternalOpensearchCheck {
	eos.log.Debug("Checking External Opensearch Reachability")
	caCertPool := x509.NewCertPool()
	caCertPool.AppendCertsFromPEM([]byte(reqBody.OSCert))
	// Created HTTP client with a root_ca and timeout
	client := &http.Client{
		Timeout: eos.timeout * time.Second,
		Transport: &http.Transport{
			TLSClientConfig: &tls.Config{
				RootCAs:    caCertPool,
				MinVersion: tls.VersionTLS12,
			},
		},
	}

	err := eos.triggerRequest(reqBody, client)
	if err != nil {
		eos.log.Error(err)
		return createExternalOpensearchCheck(false, constants.EXTERNAL_OPENSEARCH_FAILED_TITLE, constants.STATUS_FAIL, "", constants.EXTERNAL_OPENSEARCH_ERROR_MSG, constants.EXTERNAL_OPENSEARCH_RESOLUTION_MSG, err.Error())
	}

	eos.log.Debug("External Opensearch is reachable")
	return createExternalOpensearchCheck(true, constants.EXTERNAL_OPENSEARCH_SUCCESS_TITLE, constants.STATUS_PASS, constants.EXTERNAL_OPENSEARCH_SUCCESS_MSG, "", "", "")
}

func (eos *ExternalOpensearchService) triggerRequest(reqBody models.ExternalOSRequest, client *http.Client) error {
	eos.log.Debug("Triggering Request...")
	// Create a new request with basic authentication
	// hitting /_cat/indices API of opensearch just to make sure there is OS running or not
	url := fmt.Sprintf("https://%s/_cat/indices", reqBody.OSDomainURL)
	eos.log.Debug("URL: ", url)
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		return errors.New("Failed to create request: " + err.Error())
	}
	req.SetBasicAuth(reqBody.OSUsername, reqBody.OSUserPassword)
	eos.log.Debug("Basic Authentication Added")

	resp, err := client.Do(req)
	if err != nil {
		return errors.New("Failed to connect to OpenSearch: " + err.Error())
	}
	defer resp.Body.Close()

	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		return errors.New("Failed to read response body: " + err.Error())
	}

	eos.log.Debugf("Response Body: `%s` and Status code: %d", string(respBody), resp.StatusCode)
	if resp.StatusCode != http.StatusOK {
		return errors.New("external opensearch is not reachable")
	}

	return nil
}

func (eos *ExternalOpensearchService) GetExternalOpensearchDetails(reqBody models.ExternalOSRequest) models.ExternalOpensearchResponse {
	result := models.ExternalOpensearchResponse{}

	reachableCheck := eos.checkReachability(reqBody)
	result.Checks = append(result.Checks, reachableCheck)

	// We are setting initial value as true because if any check got failed then we are directly putting false.
	// Instead of traversing whole check list we are breaking the loop because if any check got failed that means our
	// overall result should be false
	result.Passed = true
	for _, check := range result.Checks {
		if !check.Passed {
			result.Passed = false
			break
		}
	}
	return result
}

func createExternalOpensearchCheck(passed bool, title, status, successMsg, errorMsg, resolutionMsg, debugMsg string) models.ExternalOpensearchCheck {
	return models.ExternalOpensearchCheck{
		Title:         title,
		Passed:        passed,
		Status:        status,
		SuccessMsg:    successMsg,
		ErrorMsg:      errorMsg,
		ResolutionMsg: resolutionMsg,
		DebugMsg:      debugMsg,
	}
}
