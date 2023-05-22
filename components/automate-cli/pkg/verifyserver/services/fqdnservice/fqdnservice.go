package fqdnservice

import (
	"crypto/tls"
	"crypto/x509"
	"encoding/json"
	"encoding/pem"
	"fmt"
	"io"
	"net/http"
	"strings"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
)

type IFqdnService interface {
	CheckFqdnReachability(models.FqdnRequest) models.FqdnResponse
}

type FqdnService struct {
	log     logger.Logger
	timeout time.Duration
}

func NewFqdnService(log logger.Logger, timeout time.Duration) *FqdnService {
	return &FqdnService{
		log:     log,
		timeout: timeout,
	}
}

func (fq *FqdnService) createClient(rootCert string) *http.Client {
	caCertPool := x509.NewCertPool()
	caCertPool.AppendCertsFromPEM([]byte(rootCert))

	client := &http.Client{
		Transport: &http.Transport{
			TLSClientConfig: &tls.Config{
				RootCAs: caCertPool,
			},
		},
		Timeout: fq.timeout * time.Second,
	}

	return client
}

// createErrorMessage function is converting map into string to make more readable for end user.
func createErrorMessage(setNodes map[string]int) string {
	temp := "["
	for k := range setNodes {
		temp += k
		temp += ", "
	}
	temp = strings.TrimSuffix(temp, ", ")
	temp += "]"

	return temp
}

// fqdnReachable function will check that are we able to hit the load balancer fqdn or not.
func (fq *FqdnService) fqdnReachable(fqdn, rootCert string) (models.Checks, bool) {
	client := fq.createClient(rootCert)

	res, err := client.Get(fmt.Sprintf("https://%s", fqdn))
	if err != nil {
		fq.log.Error(err.Error())
		return createCheck(constants.FQDN_TITLE, false, "", constants.FQDN_ERROR_MESSAGE, constants.FQDN_RESOLUTION_MESSAGE), false
	}

	if res.StatusCode != 200 {
		return createCheck(constants.FQDN_TITLE, false, "", constants.FQDN_ERROR_MESSAGE, constants.FQDN_RESOLUTION_MESSAGE), false
	}
	return createCheck(constants.FQDN_TITLE, true, constants.FQDN_SUCCESS_MESSAGE, "", ""), true
}

// nodeReachable function will check that our load balancer will correctly redirecting to all the nodes or not.
func (fq *FqdnService) nodeReachable(fqdn, rootCert string, reqNodes []string) (models.Checks, bool) {
	// creating a map for storing all the nodes given in request.
	setNodes := make(map[string]int)
	for _, k := range reqNodes {
		setNodes[k] += 1
	}

	client := fq.createClient(rootCert)
	fqdnResultChan := make(chan string)

	for i := 0; i < 50; i++ {
		go func(fqdnResultChan chan string) {
			res, err := client.Get(fmt.Sprintf("https://%s", fqdn))
			if err != nil {
				fq.log.Error(err.Error())
				fqdnResultChan <- "got error"
				return
			}
			nodeIP := res.Header.Get("x-server-ip")
			fqdnResultChan <- nodeIP
		}(fqdnResultChan)
	}

	for i := 0; i < 50; i++ {
		chanResult := <-fqdnResultChan
		if chanResult == "got error" {
			break
		}
		delete(setNodes, chanResult)
		//if setNodes becomes empty, that means we are able to reach all the nodes given in the request body.
		if len(setNodes) == 0 {
			return createCheck(constants.NODE_TITLE, true, constants.NODE_SUCCESS_MESSAGE, "", ""), true
		}
	}

	temp := createErrorMessage(setNodes)
	return createCheck(constants.NODE_TITLE, false, "", fmt.Sprintf(constants.NODE_ERROR_MESSAGE, temp), constants.NODE_RESOLUTION_MESSAGE), false
}

// validateCertificate will check that if our root certificate is valid or not.
func (fq *FqdnService) validateCertificate(fqdn, rootCert string) (models.Checks, bool) {
	// Parse the PEM-encoded SSL certificate
	block, _ := pem.Decode([]byte(rootCert))
	if block == nil || block.Type != "CERTIFICATE" {
		fq.log.Error("Failed to decode certificate PEM")
		return createCheck(constants.CERTIFICATE_TITLE, false, "", constants.CERTIFICATE_ERROR_MESSAGE, constants.CERTIFICATE_RESOLUTION_MESSAGE), false
	}

	certificate, err := x509.ParseCertificate(block.Bytes)
	if err != nil {
		fq.log.Error(err.Error())
		return createCheck(constants.CERTIFICATE_TITLE, false, "", constants.CERTIFICATE_ERROR_MESSAGE, constants.CERTIFICATE_RESOLUTION_MESSAGE), false
	}

	currentTime := time.Now()
	if currentTime.After(certificate.NotAfter) || !certificate.IsCA || len(certificate.DNSNames) == 0 {
		return createCheck(constants.CERTIFICATE_TITLE, false, "", constants.CERTIFICATE_ERROR_MESSAGE, constants.CERTIFICATE_RESOLUTION_MESSAGE), false
	}
	return createCheck(constants.CERTIFICATE_TITLE, true, constants.CERTIFICATE_SUCCESS_MESSAGE, "", ""), true
}

// checkChefServerStatus function will check that all the services are in ok state or not.
func (fq *FqdnService) checkChefServerStatus(fqdn, rootCert string) (models.Checks, bool) {
	client := fq.createClient(rootCert)

	res, err := client.Get(fmt.Sprintf("https://%s/_status", fqdn))
	if err != nil {
		fq.log.Error(err.Error())
		return createCheck(constants.CHEF_SERVER_TITLE, false, "", constants.A2_CS_ERROR_MESSAGE, constants.A2_CS_RESOLUTION_MESSAGE), false
	}

	resBody, err := io.ReadAll(res.Body)
	if err != nil {
		fq.log.Error(err.Error())
		return createCheck(constants.CHEF_SERVER_TITLE, false, "", constants.A2_CS_ERROR_MESSAGE, constants.A2_CS_RESOLUTION_MESSAGE), false
	}

	var data map[string]interface{}
	json.Unmarshal(resBody, &data)

	if data["status"] != "pong" {
		return createCheck(constants.CHEF_SERVER_TITLE, false, "", constants.A2_CS_ERROR_MESSAGE, constants.A2_CS_RESOLUTION_MESSAGE), false
	}
	fq.log.Debug("Chef Server Status is all okay.")
	return createCheck(constants.CHEF_SERVER_TITLE, true, constants.A2_CS_SUCCESS_MESSAGE, "", ""), true
}

// checkAutomateStatus function will check that all the services are in ok state or not.
func (fq *FqdnService) checkAutomateStatus(fqdn, rootCert, apiToken string) (models.Checks, bool) {
	client := fq.createClient(rootCert)

	apiUrl := fmt.Sprintf("https://%s/api/v0/status", fqdn)
	// Create a new HTTP GET request
	req, err := http.NewRequest("GET", apiUrl, nil)
	if err != nil {
		fmt.Printf("Error creating request: %v", err)
		return createCheck(constants.AUTOMATE_TITLE, false, "", constants.A2_CS_ERROR_MESSAGE, constants.A2_CS_RESOLUTION_MESSAGE), false
	}

	// Set the API token in the Authorization header
	req.Header.Set("api-token", apiToken)
	resp, err := client.Do(req)
	if err != nil {
		fmt.Printf("Error making request: %v", err)
		return createCheck(constants.AUTOMATE_TITLE, false, "", constants.A2_CS_ERROR_MESSAGE, constants.A2_CS_RESOLUTION_MESSAGE), false
	}

	resBody, err := io.ReadAll(resp.Body)
	if err != nil {
		fq.log.Error(err.Error())
		return createCheck(constants.AUTOMATE_TITLE, false, "", constants.A2_CS_ERROR_MESSAGE, constants.A2_CS_RESOLUTION_MESSAGE), false
	}

	var data map[string]interface{}
	json.Unmarshal(resBody, &data)
	if data["ok"] != true {
		return createCheck(constants.AUTOMATE_TITLE, false, "", constants.A2_CS_ERROR_MESSAGE, constants.A2_CS_RESOLUTION_MESSAGE), false
	}
	fq.log.Debug("Automate Status is all okay")
	return createCheck(constants.AUTOMATE_TITLE, true, constants.A2_CS_SUCCESS_MESSAGE, "", ""), true
}

func createCheck(title string, passed bool, successMsg, errorMsg, resolutionMsg string) models.Checks {
	return models.Checks{
		Title:         title,
		Passed:        passed,
		SuccessMsg:    successMsg,
		ErrorMsg:      errorMsg,
		ResolutionMsg: resolutionMsg,
	}
}

func (fq *FqdnService) CheckFqdnReachability(req models.FqdnRequest) models.FqdnResponse {
	var response = models.FqdnResponse{}

	check, _ := fq.fqdnReachable(req.Fqdn, req.RootCert)
	response.Checks = append(response.Checks, check)

	if req.IsAfterDeployment {
		if req.NodeType == constants.CHEF_INFRA_SERVER {
			check, _ = fq.checkChefServerStatus(req.Fqdn, req.RootCert)
			response.Checks = append(response.Checks, check)
		} else if req.NodeType == constants.AUTOMATE {
			check, _ = fq.checkAutomateStatus(req.Fqdn, req.RootCert, req.ApiToken)
			response.Checks = append(response.Checks, check)
		}
	} else {
		check, _ = fq.nodeReachable(req.Fqdn, req.RootCert, req.Nodes)
		response.Checks = append(response.Checks, check)

		check, _ = fq.validateCertificate(req.Fqdn, req.RootCert)
		response.Checks = append(response.Checks, check)
	}

	flag := true
	for _, k := range response.Checks {
		if !k.Passed {
			flag = false
			break
		}
	}
	response.Passed = flag
	return response
}
