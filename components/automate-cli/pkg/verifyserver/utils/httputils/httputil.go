package httputils

import (
	"bytes"
	"encoding/json"
	"errors"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
)

//Post - This function performs HTTP Post request to the given endpoint with the provided request body
func MakeRequest(requestMethod string, url string, body interface{}) (*http.Response, error) {
	requestBody, err := json.Marshal(body)
	if err != nil {
		return nil, err
	}
	req, err := http.NewRequest(requestMethod, url, bytes.NewBuffer(requestBody))
	if err != nil {
		return nil, err
	}
	req.Header.Set(constants.CONTENT_TYPE, constants.TYPE_JSON)

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		return nil, err
	}
	if err == nil && resp.StatusCode != 200 {
		return nil, errors.New(resp.Status)
	}
	return resp, nil
}
