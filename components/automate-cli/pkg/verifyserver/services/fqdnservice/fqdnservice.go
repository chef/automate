package fqdnservice

import (
	"crypto/md5"
	"crypto/tls"
	"crypto/x509"
	"encoding/hex"
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
	fq.log.Debug("Creating Client")
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
	fq.log.Debug("Client Created.")
	return client
}

func createErrorMessage(setNodes map[string]int, reqnodes []string, isAfterDeployment bool) string {
	temp := "["
	for _, k := range reqnodes {
		var val string
		if isAfterDeployment {
			hasher := md5.New() // nosemgrep
			_, err := io.WriteString(hasher, k)
			if err != nil {
				return constants.IP_TO_HASH_FAIL_MESSAGE
			}
			val = hex.EncodeToString(hasher.Sum(nil))
		} else {
			val = k
		}

		if setNodes[val] > 0 {
			temp += k
			temp += ", "
		}
	}
	temp = strings.TrimSuffix(temp, ", ")
	temp += "]"

	return fmt.Sprintf(constants.NODE_ERROR_MESSAGE, temp)
}

// makeSet function is storing all the request nodes into the map according to the deployment.
// if isAfterDeployment is true then it will store the hash of the ips of request nodes,
// otherwise it will directly store the ips.
func makeSet(reqNodes []string, isAfterDeployment bool) (map[string]int, error) {
	setNodes := make(map[string]int)
	for _, k := range reqNodes {
		if isAfterDeployment {
			hasher := md5.New() // nosemgrep
			_, err := io.WriteString(hasher, k)
			if err != nil {
				return nil, err
			}
			setNodes[hex.EncodeToString(hasher.Sum(nil))] += 1
		} else {
			setNodes[k] += 1
		}
	}

	return setNodes, nil
}

// fqdnReachable function will check that are we able to hit the load balancer fqdn or not.
func (fq *FqdnService) fqdnReachable(fqdn, rootCert, nodeType string, isAfterDeployment bool, port string) models.Checks {
	fq.log.Debug("Checking Fqdn Reachability...")
	client := fq.createClient(rootCert)
	var url string

	if isAfterDeployment && nodeType == constants.CHEF_INFRA_SERVER {
		url = fmt.Sprintf("https://%s:%s/_status", fqdn, port)
	} else {
		url = fmt.Sprintf("https://%s:%s", fqdn, port)
	}
	fq.log.Debug("URL: ", url)

	res, err := client.Get(url)
	if err != nil {
		fq.log.Error(err.Error())
		return createCheck(constants.FQDN_TITLE, false, "", constants.FQDN_ERROR_MESSAGE, constants.FQDN_RESOLUTION_MESSAGE)
	}

	fq.log.Debug("Status Code: ", res.StatusCode)
	if res.StatusCode != 200 {
		return createCheck(constants.FQDN_TITLE, false, "", constants.FQDN_ERROR_MESSAGE, constants.FQDN_RESOLUTION_MESSAGE)
	}
	fq.log.Debug("Fqdn is Reachable.")
	return createCheck(constants.FQDN_TITLE, true, constants.FQDN_SUCCESS_MESSAGE, "", "")
}

// nodeReachable function will check that our load balancer will correctly redirecting to all the nodes or not.
func (fq *FqdnService) nodeReachable(fqdn, rootCert string, reqNodes []string, isAfterDeployment bool, port string) models.Checks {
	fq.log.Debug("Checking Node Reachability...")
	setNodes, err := makeSet(reqNodes, isAfterDeployment)
	if err != nil {
		fq.log.Error(err.Error())
		return createCheck(constants.NODE_TITLE, false, "", constants.IP_TO_HASH_FAIL_MESSAGE, constants.NODE_RESOLUTION_MESSAGE)
	}

	var url string
	if isAfterDeployment {
		url = fmt.Sprintf("https://%s:%s/check_status", fqdn, port)
	} else {
		url = fmt.Sprintf("https://%s:%s", fqdn, port)
	}
	fq.log.Debug("URL: ", url)

	client := fq.createClient(rootCert)
	fqdnResultChan := make(chan string)

	fq.log.Debug("Making Concurrent Calls...")
	for i := 0; i < 50; i++ {
		go func(fqdnResultChan chan string) {
			res, err := client.Get(url)
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
			fq.log.Debug("All nodes are reachable.")
			return createCheck(constants.NODE_TITLE, true, constants.NODE_SUCCESS_MESSAGE, "", "")
		}
	}

	errorMessage := createErrorMessage(setNodes, reqNodes, isAfterDeployment)
	return createCheck(constants.NODE_TITLE, false, "", errorMessage, constants.NODE_RESOLUTION_MESSAGE)
}

// validateCertificate will check that if our root certificate is valid or not.
func (fq *FqdnService) validateCertificate(rootCert string) models.Checks {
	fq.log.Debug("Validating Certificate...")
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

	check = fq.nodeReachable(req.Fqdn, req.RootCert, req.Nodes, req.IsAfterDeployment, port)
	response.Checks = append(response.Checks, check)

	if !req.IsAfterDeployment {
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
