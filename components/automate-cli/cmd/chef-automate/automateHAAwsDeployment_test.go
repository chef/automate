package main

import (
	"net/http"
	"testing"

	"github.com/chef/automate/lib/httputils"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

func TestGetBastionIamRole(t *testing.T) {
	ser := newAwsDeployemnt("")
	tests := []struct {
		description        string
		MockhttputilsToken httputils.HTTPClient
		MockhttputilsIam   httputils.HTTPClient
		wantError          error
		testurl            string
	}{
		{
			description: "Failed to get token value from the request",
			MockhttputilsToken: &httputils.MockHTTPClient{
				MakeRequestWithHeadersfunc: func(requestMethod, url string, body interface{}, headerkey, headerValue string) (*http.Response, []byte, error) {
					return nil, nil, errors.New("failed to marshal request body")
				},
			},
			wantError: errors.New("error while getting the token value: failed to marshal request body"),
			testurl:   TOKEN_URL,
		},
		{
			description: "Failed to get token value from the request (error in creation of request)",
			MockhttputilsToken: &httputils.MockHTTPClient{
				MakeRequestWithHeadersfunc: func(requestMethod, url string, body interface{}, headerkey, headerValue string) (*http.Response, []byte, error) {
					return nil, nil, errors.New("failed to create HTTP request")
				},
			},
			wantError: errors.New("error while getting the token value: failed to create HTTP request"),
			testurl:   TOKEN_URL,
		},
		{
			description: "Bastion IAM role is not attached",
			MockhttputilsToken: &httputils.MockHTTPClient{
				MakeRequestWithHeadersfunc: func(requestMethod, url string, body interface{}, headerkey, headerValue string) (*http.Response, []byte, error) {
					return &http.Response{}, nil, nil
				},
			},
			MockhttputilsIam: &httputils.MockHTTPClient{
				MakeRequestWithHeadersfunc: func(requestMethod, url string, body interface{}, headerkey, headerValue string) (*http.Response, []byte, error) {
					return &http.Response{}, []byte("Success"), nil
				},
			},
			wantError: errors.New("Please check if Bastion has attached an IAM Role to it"),
			testurl:   METADATA_URL,
		},
		{
			description: "Bastion IAM role is not attached (Error)",
			MockhttputilsToken: &httputils.MockHTTPClient{
				MakeRequestWithHeadersfunc: func(requestMethod, url string, body interface{}, headerkey, headerValue string) (*http.Response, []byte, error) {
					return &http.Response{}, nil, nil
				},
			},
			MockhttputilsIam: &httputils.MockHTTPClient{
				MakeRequestWithHeadersfunc: func(requestMethod, url string, body interface{}, headerkey, headerValue string) (*http.Response, []byte, error) {
					if requestMethod == http.MethodPut {
						return nil, nil, nil
					}
					return nil, nil, errors.New("failed to Make HTTP Request")
				},
			},
			wantError: errors.New("error while getting the response for IAM role: failed to Make HTTP Request"),
			testurl:   METADATA_URL,
		},
		{
			description: "Bastion IAM role is attached",
			MockhttputilsToken: &httputils.MockHTTPClient{
				MakeRequestWithHeadersfunc: func(requestMethod, url string, body interface{}, headerkey, headerValue string) (*http.Response, []byte, error) {
					return &http.Response{}, nil, nil
				},
			},
			MockhttputilsIam: &httputils.MockHTTPClient{
				MakeRequestWithHeadersfunc: func(requestMethod, url string, body interface{}, headerkey, headerValue string) (*http.Response, []byte, error) {
					return &http.Response{
						StatusCode: 200,
					}, []byte("Success"), nil
				},
			},
			wantError: nil,
			testurl:   METADATA_URL,
		},
	}

	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			if tt.testurl == TOKEN_URL {
				ser.httpRequestClient = tt.MockhttputilsToken
			} else if tt.testurl == METADATA_URL {
				ser.httpRequestClient = tt.MockhttputilsIam
			}
			err := ser.isIamRolePresent()
			if err != nil {
				assert.Equal(t, err.Error(), tt.wantError.Error())
			}
		})
	}
}
