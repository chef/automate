package mock_test

import (
	"io"
	"net/http"
	"testing"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/mock"
	"github.com/stretchr/testify/assert"
)

func TestKeysAPI(t *testing.T) {

	tests := []struct {
		description  string
		resBody      string
		expectedCode int
		expectedBody string
		wait         time.Duration
	}{
		{
			description:  "200:success route",
			expectedCode: 200,
			resBody:      "{\"data\": \"test\"}",
			expectedBody: "{\"data\": \"test\"}",
		},
	}
	keyEndpoint := "/api/v1/test"

	for _, test := range tests {
		t.Run(test.description, func(t *testing.T) {

			ts := mock.NewAPI(t).HttpReqMock(keyEndpoint, test.expectedCode, []byte(test.resBody), test.wait).Build()

			//ts.URL gives the mock server endpoint
			resp, err := http.Get(ts.URL + keyEndpoint)
			if err != nil {
				t.Fatalf("Failed to make GET request: %v", err)
			}
			defer resp.Body.Close()

			assert.NoError(t, err)

			// Read the response body
			body, err := io.ReadAll(resp.Body)

			// Reading the response body should work everytime, such that
			// the err variable should be nil
			assert.NoError(t, err, test.description)

			// Verify, that the reponse body equals the expected body
			assert.Equalf(t, test.expectedBody, string(body), test.description)
		})
	}
}
