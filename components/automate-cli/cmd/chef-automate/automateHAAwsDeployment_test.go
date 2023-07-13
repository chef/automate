package main

import (
	"net/http"
	"log"
	"testing"

	"github.com/chef/automate/lib/httputils"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

func TestGetBastionIamRole(t *testing.T) {
	ser := newAwsDeployemnt("")
	tests := []struct {
		description string
		Mockhttputils httputils.HTTPClient
		wantError error
		want bool
	}{
		{
			description: "Failed to get Token Value from the Request",
			Mockhttputils: &httputils.MockHTTPClient{
				MakeRequestWithHeadersfunc: func(requestMethod, url string, body interface{}, headerkey, headerValue string) (*http.Response, []byte, error) {
					return nil, nil, errors.New("failed to marshal request body")
				},
			},
			wantError: errors.New("error while getting the token value= failed to marshal request body"),
			want: false,
		},
		{
			description: "Failed to get Token Value from the Request (Error in creation of Request)",
			Mockhttputils: &httputils.MockHTTPClient{
				MakeRequestWithHeadersfunc: func(requestMethod, url string, body interface{}, headerkey, headerValue string) (*http.Response, []byte, error) {
					return nil, nil, errors.New("failed to create HTTP request")
				},
			},
			wantError: errors.New("error while getting the token value= failed to create HTTP request"),
			want: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			ser.httpRequestClient = tt.Mockhttputils
			got, err := ser.getBastionIamRole()
			log.Println("got=", got)
			log.Println("err=", err)
			assert.Equal(t, got, tt.want)
			assert.Equal(t, err.Error(), tt.wantError.Error())
		})
	}
}