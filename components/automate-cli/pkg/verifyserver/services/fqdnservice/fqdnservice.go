package fqdnservice

import (
	"crypto/tls"
	"crypto/x509"
	"encoding/pem"
	"fmt"
	"net/http"
	"strings"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
)

type IFqdnService interface {
	CheckFqdnReachability(models.FqdnRequest, string) models.FqdnResponse
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
				RootCAs:    caCertPool,
				MinVersion: tls.VersionTLS12,
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
func (fq *FqdnService) fqdnReachable(fqdn, rootCert, nodeType string, IsAfterDeployment bool, port string) models.Checks {
	client := fq.createClient(rootCert)
	var url string

	if IsAfterDeployment && nodeType == constants.CHEF_INFRA_SERVER {
		url = fmt.Sprintf("https://%s:%s/_status", fqdn, port)
	} else {
		url = fmt.Sprintf("https://%s:%s", fqdn, port)
	}

	res, err := client.Get(url)
	if err != nil {
		fq.log.Error(err.Error())
		return createCheck(constants.FQDN_TITLE, false, "", constants.FQDN_ERROR_MESSAGE, constants.FQDN_RESOLUTION_MESSAGE)
	}

	if res.StatusCode != 200 {
		return createCheck(constants.FQDN_TITLE, false, "", constants.FQDN_ERROR_MESSAGE, constants.FQDN_RESOLUTION_MESSAGE)
	}
	return createCheck(constants.FQDN_TITLE, true, constants.FQDN_SUCCESS_MESSAGE, "", "")
}

// nodeReachable function will check that our load balancer will correctly redirecting to all the nodes or not.
func (fq *FqdnService) nodeReachable(fqdn, rootCert string, reqNodes []string, port string) models.Checks {
	// creating a map for storing all the nodes given in request.
	setNodes := make(map[string]int)
	for _, k := range reqNodes {
		setNodes[k] += 1
	}

	client := fq.createClient(rootCert)
	fqdnResultChan := make(chan string)

	for i := 0; i < 50; i++ {
		go func(fqdnResultChan chan string) {
			res, err := client.Get(fmt.Sprintf("https://%s:%s", fqdn, port))
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
			continue
		}
		delete(setNodes, chanResult)
		//if setNodes becomes empty, that means we are able to reach all the nodes given in the request body.
		if len(setNodes) == 0 {
			return createCheck(constants.NODE_TITLE, true, constants.NODE_SUCCESS_MESSAGE, "", "")
		}
	}

	temp := createErrorMessage(setNodes)
	return createCheck(constants.NODE_TITLE, false, "", fmt.Sprintf(constants.NODE_ERROR_MESSAGE, temp), constants.NODE_RESOLUTION_MESSAGE)
}

// validateCertificate will check that if our root certificate is valid or not.
func (fq *FqdnService) validateCertificate(rootCert string) models.Checks {
	// Parse the PEM-encoded SSL certificate
	block, _ := pem.Decode([]byte(rootCert))
	if block == nil || block.Type != "CERTIFICATE" {
		fq.log.Error("Failed to decode certificate PEM")
		return createCheck(constants.CERTIFICATE_TITLE, false, "", constants.CERTIFICATE_ERROR_MESSAGE, constants.CERTIFICATE_RESOLUTION_MESSAGE)
	}

	certificate, err := x509.ParseCertificate(block.Bytes)
	if err != nil {
		fq.log.Error(err.Error())
		return createCheck(constants.CERTIFICATE_TITLE, false, "", constants.CERTIFICATE_ERROR_MESSAGE, constants.CERTIFICATE_RESOLUTION_MESSAGE)
	}

	currentTime := time.Now()
	if currentTime.After(certificate.NotAfter) || !certificate.IsCA || len(certificate.DNSNames) == 0 {
		return createCheck(constants.CERTIFICATE_TITLE, false, "", constants.CERTIFICATE_ERROR_MESSAGE, constants.CERTIFICATE_RESOLUTION_MESSAGE)
	}
	return createCheck(constants.CERTIFICATE_TITLE, true, constants.CERTIFICATE_SUCCESS_MESSAGE, "", "")
}

// checkServiceStatus function will check that all the services are in ok state or not.
func (fq *FqdnService) checkServiceStatus(fqdn, rootCert string, reqNodes []string, port string) models.Checks {
	setNodes := make(map[string]int)
	for _, k := range reqNodes {
		setNodes[k] += 1
		// hasher := md5.New()
		// _, err := io.WriteString(hasher, k)
		// if err != nil {
		// 	panic(err)
		// }
		// setNodes[hex.EncodeToString(hasher.Sum(nil))] += 1
	}

	client := fq.createClient(rootCert)
	fqdnResultChan := make(chan string)
	for i := 0; i < 50; i++ {
		go func(fqdnResultChan chan string) {
			res, err := client.Get(fmt.Sprintf("https://%s:%s/check_status", fqdn, port))
			if err != nil {
				fq.log.Error(err.Error())
				fqdnResultChan <- "got error"
				return
			}
			nodeIP := res.Header.Get("X-Real-IP")
			fqdnResultChan <- nodeIP
		}(fqdnResultChan)
	}

	for i := 0; i < 50; i++ {
		chanResult := <-fqdnResultChan
		if chanResult == "got error" {
			continue
		}
		delete(setNodes, chanResult)
		//if setNodes becomes empty, that means we are able to reach all the nodes given in the request body.
		if len(setNodes) == 0 {
			return createCheck(constants.A2_CS_TITLE, true, constants.A2_CS_SUCCESS_MESSAGE, "", "")
		}
	}

	temp := createErrorMessage(setNodes)
	return createCheck(constants.A2_CS_TITLE, false, "", fmt.Sprintf(constants.A2_CS_ERROR_MESSAGE, temp), constants.A2_CS_RESOLUTION_MESSAGE)
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

func (fq *FqdnService) CheckFqdnReachability(req models.FqdnRequest, port string) models.FqdnResponse {
	var response = models.FqdnResponse{}

	check := fq.fqdnReachable(req.Fqdn, req.RootCert, req.NodeType, req.IsAfterDeployment, port)
	response.Checks = append(response.Checks, check)

	if req.IsAfterDeployment {
		check = fq.checkServiceStatus(req.Fqdn, req.RootCert, req.Nodes, port)
		response.Checks = append(response.Checks, check)
	} else {
		check = fq.nodeReachable(req.Fqdn, req.RootCert, req.Nodes, port)
		response.Checks = append(response.Checks, check)

		check = fq.validateCertificate(req.RootCert)
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
