package httputils

import (
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestMakeRequest(t *testing.T) {
	tests := []struct {
		description         string
		isInterfaceFunction bool
	}{
		{
			description:         "Test make request function",
			isInterfaceFunction: false,
		},
		{
			description:         "Test make request interface function ",
			isInterfaceFunction: true,
		},
	}
	for _, test := range tests {
		t.Run(test.description, func(t *testing.T) {
			// Create a test server to receive requests
			server := httptest.NewServer(http.HandlerFunc(func(rw http.ResponseWriter, req *http.Request) {
				// Check that the request body is correct
				expectedBody := "{\"org\":\"chef\"}"
				requestBody, err := ioutil.ReadAll(req.Body)
				if err != nil {
					t.Errorf("Unexpected error reading request body: %v", err)
				}
				if string(requestBody) != expectedBody {
					t.Errorf("Unexpected request body. Expected %v, got %v", expectedBody, string(requestBody))
				}
				// Write a response to the client
				rw.WriteHeader(http.StatusOK)
				rw.Write([]byte("{\"status\":\"OK\"}"))
			}))
			defer server.Close()

			// Call the Post function with a test URL and body
			url := server.URL
			body := map[string]string{"org": "chef"}

			var resp *http.Response
			var err error
			if test.isInterfaceFunction {
				httpUtils := NewHttpRequestClient()
				resp, err = httpUtils.MakeRequest(http.MethodPost, url, body)
			} else {
				resp, err = MakeRequest(http.MethodPost, url, body)
			}
			if err != nil {
				t.Errorf("Unexpected error from Post function: %v", err)
			}

			// Check that the response status code is correct
			if resp.StatusCode != http.StatusOK {
				t.Errorf("Unexpected response status code. Expected %v, got %v", http.StatusOK, resp.StatusCode)
			}
			// Check that the response body is correct
			expectedResponse := "{\"status\":\"OK\"}"
			responseBody, err := ioutil.ReadAll(resp.Body)
			if err != nil {
				t.Errorf("Unexpected error reading response body: %v", err)
			}
			if string(responseBody) != expectedResponse {
				t.Errorf("Unexpected response body. Expected %v, got %v", expectedResponse, string(responseBody))
			}
		})

	}
}
