package httputils

import (
	"bytes"
	"encoding/json"
	"io"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
)

type HttpRequestClient struct{}

func NewHttpRequestClient() *HttpRequestClient {
	return &HttpRequestClient{}
}

type IHttpRequestClient interface {
	MakeRequest(requestMethod, url string, body interface{}) (*http.Response, error)
}

func (ht *HttpRequestClient) MakeRequest(requestMethod string, url string, body interface{}) (*http.Response, error) {
	return MakeRequest(requestMethod, url, body)
}

// Post - This function performs HTTP Post request to the given endpoint with the provided request body
func MakeRequest(requestMethod string, url string, body interface{}) (*http.Response, error) {
	var reader io.Reader
	if body != nil {
		requestBody, err := json.Marshal(body)
		if err != nil {
			return nil, err
		}
		reader = bytes.NewBuffer(requestBody)
	}
	req, err := http.NewRequest(requestMethod, url, reader)
	if err != nil {
		return nil, err
	}
	req.Header.Set(constants.CONTENT_TYPE, constants.TYPE_JSON)

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		return resp, err
	}
	return resp, nil
}
