package httputils

import (
	"net/http"
)

type MockHttpRequestClient struct {
	MakeRequestFunc func(requestMethod, url string, body interface{}) (*http.Response, error)
}

func (mss *MockHttpRequestClient) MakeRequest(requestMethod, url string, body interface{}) (*http.Response, error) {
	return mss.MakeRequestFunc(requestMethod, url, body)
}
