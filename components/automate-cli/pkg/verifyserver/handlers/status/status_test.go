package status_test

import (
	"io"
	"net/http/httptest"
	"os"
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/mock"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
)

func TestStatusAPI(t *testing.T) {
	var log = &logrus.Logger{
		Out:       os.Stderr,
		Formatter: &logrus.TextFormatter{TimestampFormat: "2006-01-02 15:04:05.000", FullTimestamp: true},
		Hooks:     make(logrus.LevelHooks),
		Level:     logrus.DebugLevel,
	}
	tests := []struct {
		description  string
		expectedCode int
		expectedBody string
	}{
		{
			description:  "200:success status route",
			expectedCode: 200,
			expectedBody: "\"status\":\"ok\"",
		},
	}
	statusEndpoint := "/status"
	// Setup the app as it is done in the main function
	app := mock.SetupWithDefaultHandlers(log)

	for _, test := range tests {
		t.Run(test.description, func(t *testing.T) {
			req := httptest.NewRequest("GET", statusEndpoint, nil)
			req.Header.Add("Content-Type", "application/json")
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := io.ReadAll(res.Body)
			assert.NoError(t, err, test.description)
			assert.Contains(t, string(body), test.expectedBody)
			assert.Equal(t, res.StatusCode, test.expectedCode)
		})
	}
}
