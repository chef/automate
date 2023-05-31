package externalopensearchservice

import (
	"crypto/tls"
	"crypto/x509"
	"fmt"
	"io"
	"net/http"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
)

type IExternalOpensearchService interface {
	GetExternalOpensearchDetails(models.ExternalOS, int) models.ExternalOpensearchResponse
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

func (eos *ExternalOpensearchService) checkReachability(reqBody models.ExternalOS, port int) models.ExternalOpensearchCheck {
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

	// Create a new request with basic authentication
	url := fmt.Sprintf("https://%s:%d", reqBody.OSDomainURL, port)
	eos.log.Debug("URL: ", url)
	req, err := http.NewRequest("GET", url, nil)
	if err != nil {
		eos.log.Debug("Failed to create request:", err)
		return createExternalOpensearchCheck(false, constants.EXTERNAL_OPENSEARCH_FAILED_TITLE, constants.STATUS_FAIL, "", constants.EXTERNAL_OPENSEARCH_ERROR_MSG, constants.EXTERNAL_OPENSEARCH_RESOLUTION_MSG, fmt.Sprintf("Failed to create request: %v", err))
	}
	req.SetBasicAuth(reqBody.OSUsername, reqBody.OSUserPassword)
	eos.log.Debug("Basic Authentication Added")

	resp, err := client.Do(req)
	if err != nil {
		eos.log.Debug("Failed to connect to OpenSearch:", err)
		return createExternalOpensearchCheck(false, constants.EXTERNAL_OPENSEARCH_FAILED_TITLE, constants.STATUS_FAIL, "", constants.EXTERNAL_OPENSEARCH_ERROR_MSG, constants.EXTERNAL_OPENSEARCH_RESOLUTION_MSG, fmt.Sprintf("Failed to connect to OpenSearch: %v", err))
	}
	defer resp.Body.Close()
	respBody, err := io.ReadAll(resp.Body)
	if err != nil {
		eos.log.Debug("Failed to read response body:", err)
		return createExternalOpensearchCheck(false, constants.EXTERNAL_OPENSEARCH_FAILED_TITLE, constants.STATUS_FAIL, "", constants.EXTERNAL_OPENSEARCH_ERROR_MSG, constants.EXTERNAL_OPENSEARCH_RESOLUTION_MSG, fmt.Sprintf("Failed to read response body: %v", err))
	}
	eos.log.Debugf("Response Body: `%s` and Status code: %d", string(respBody), resp.StatusCode)
	if resp.StatusCode == http.StatusOK {
		return createExternalOpensearchCheck(true, constants.EXTERNAL_OPENSEARCH_SUCCESS_TITLE, constants.STATUS_PASS, constants.EXTERNAL_OPENSEARCH_SUCCESS_MSG, "", "", "")
	}

	return createExternalOpensearchCheck(false, constants.EXTERNAL_OPENSEARCH_FAILED_TITLE, constants.STATUS_FAIL, "", constants.EXTERNAL_OPENSEARCH_ERROR_MSG, constants.EXTERNAL_OPENSEARCH_RESOLUTION_MSG, fmt.Sprintf("Response Body: `%s` and Status code: %d", string(respBody), resp.StatusCode))
}

func (eos *ExternalOpensearchService) GetExternalOpensearchDetails(reqBody models.ExternalOS, port int) models.ExternalOpensearchResponse {
	result := models.ExternalOpensearchResponse{}

	reachableCheck := eos.checkReachability(reqBody, port)
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

func createExternalOpensearchCheck(passed bool, title, status, success_msg, error_msg, resolution_msg, debug_msg string) models.ExternalOpensearchCheck {
	return models.ExternalOpensearchCheck{
		Title:         title,
		Passed:        passed,
		Status:        status,
		SuccessMsg:    success_msg,
		ErrorMsg:      error_msg,
		ResolutionMsg: resolution_msg,
		DebugMsg:      debug_msg,
	}
}
