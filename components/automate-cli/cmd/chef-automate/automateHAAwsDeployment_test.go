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
		description string
		MockhttputilsToken httputils.HTTPClient
		MockhttputilsIam httputils.HTTPClient
		wantError error
		want bool
		testurl string
	}{
		{
			description: "Failed to get Token Value from the Request",
			MockhttputilsToken: &httputils.MockHTTPClient{
				MakeRequestWithHeadersfunc: func(requestMethod, url string, body interface{}, headerkey, headerValue string) (*http.Response, []byte, error) {
					return nil, nil, errors.New("failed to marshal request body")
				},
			},
			wantError: errors.New("error while getting the token value= failed to marshal request body"),
			want: false,
			testurl: tokenurl,
		},
		{
			description: "Failed to get Token Value from the Request (Error in creation of Request)",
			MockhttputilsToken: &httputils.MockHTTPClient{
				MakeRequestWithHeadersfunc: func(requestMethod, url string, body interface{}, headerkey, headerValue string) (*http.Response, []byte, error) {
					return nil, nil, errors.New("failed to create HTTP request")
				},
			},
			wantError: errors.New("error while getting the token value= failed to create HTTP request"),
			want: false,
			testurl: tokenurl,
		},
		{
			description: "Response Code is not 200",
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
			want: false,
			wantError: errors.New("Please check if Bastion has attached IAM Role onto it"),
			testurl: metaDataurl,
		},
		{
			description: "Response Code is 200",
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
			want: true,
			wantError: nil,
			testurl: metaDataurl,
		},
	}

	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			if tt.testurl == tokenurl {
				ser.httpRequestClient = tt.MockhttputilsToken
			} else if tt.testurl == metaDataurl {
				ser.httpRequestClient = tt.MockhttputilsIam
			}
			got, err := ser.getBastionIamRole()
			assert.Equal(t, tt.want, got)
			if err != nil {
				assert.Equal(t, err.Error(), tt.wantError.Error())
			}
		})
	}
}