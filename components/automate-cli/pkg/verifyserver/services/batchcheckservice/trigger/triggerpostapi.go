package trigger

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber"
)

// RunCheckWithEndPointSpecified triggers the API on given endpoint with node ip, node type
func RunCheckWithEndPointSpecified(endPoint string, log logger.Logger, reqList []models.NodeIpRequest, method string) []models.CheckTriggerResponse {
	log.Debugf("Triggering the api call for specified nodes only")

	outputCh := make(chan models.CheckTriggerResponse)

	for _, req := range reqList {
		log.Debugf("Triggering api on enpoint %s for the node %s", endPoint, req.NodeIP)
		go triggerCheckAPIPost(endPoint, req.NodeIP, req.NodeType, outputCh, method, req.Request)
	}

	return getResultFromOutputChan(len(reqList), outputCh)
}

// RunCheckOnSpecifiedNodes triggers the API on gives node ips only, requires for various API's like S3/Minio backup config
func RunCheckOnSpecifiedNode(nodeIps []string, log logger.Logger, port string, path string, nodeType string, method string, reqBody interface{}) []models.CheckTriggerResponse {
	log.Debugf("Triggering the api call for specified nodes only")
	outputCh := make(chan models.CheckTriggerResponse)
	for _, ip := range nodeIps {
		log.Debugf("Triggering api %s for the node %s", path, ip)
		endpoint := prepareEndpoint(path, ip, port, nodeType, "")
		go triggerCheckAPIPost(endpoint, ip, nodeType, outputCh, method, reqBody)
	}

	return getResultFromOutputChan(len(nodeIps), outputCh)
}

// getResultFromOutputChan gets the result from output channel
func getResultFromOutputChan(reqList int, outputCh chan models.CheckTriggerResponse) []models.CheckTriggerResponse {
	var result []models.CheckTriggerResponse

	for i := 0; i < reqList; i++ {
		select {
		case res := <-outputCh:
			result = append(result, res)
		}
	}

	return result
}

// triggers the API with Post request and response
func triggerCheckAPIPost(endPoint, host, nodeType string, output chan<- models.CheckTriggerResponse, method string, reqBody interface{}) {
	var ctr models.CheckTriggerResponse

	reader, err := interfaceToIOReader(reqBody)
	if err != nil {
		output <- models.CheckTriggerResponse{
			Error: &fiber.Error{
				Code:    http.StatusBadRequest,
				Message: fmt.Sprintf("error while reading the request body: %s", err.Error()),
			},
			Host:     host,
			NodeType: nodeType,
		}
		return
	}

	req, err := http.NewRequest(method, endPoint, reader)
	if err != nil {
		output <- models.CheckTriggerResponse{
			Host: host,
			Error: &fiber.Error{
				Code:    http.StatusInternalServerError,
				Message: fmt.Sprintf("error while creating the request:%s", err.Error()),
			},
			NodeType: nodeType,
		}
		return
	}

	client := http.Client{
		Timeout: 5 * time.Second,
	}

	resp, err := client.Do(req)
	if err != nil {
		output <- models.CheckTriggerResponse{
			Host: host,
			Error: &fiber.Error{
				Code:    http.StatusInternalServerError,
				Message: fmt.Sprintf("error while connecting to the endpoint:%s", err.Error()),
			},
			NodeType: nodeType,
		}
		return
	}

	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		output <- models.CheckTriggerResponse{
			Host: host,
			Error: &fiber.Error{
				Code:    resp.StatusCode,
				Message: "error while connecting to the endpoint, received invalid status code",
			},
			NodeType: nodeType,
		}
		return
	}

	if err := json.NewDecoder(resp.Body).Decode(&ctr); err != nil {
		output <- models.CheckTriggerResponse{
			Host: host,
			Error: &fiber.Error{
				Code:    http.StatusInternalServerError,
				Message: fmt.Sprintf("error while parsing the response data:%s", err.Error()),
			},
			NodeType: nodeType,
		}
		return
	}

	ctr.Host = host
	ctr.NodeType = nodeType
	output <- ctr
}

func interfaceToIOReader(body interface{}) (io.Reader, error) {
	var reader io.Reader
	if body != nil {
		bx, err := json.Marshal(body)
		if err != nil {
			return nil, err
		}

		reader = bytes.NewBuffer(bx)

	}
	return reader, nil
}
