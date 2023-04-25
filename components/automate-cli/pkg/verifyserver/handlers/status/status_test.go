package statusHandler_test

import (
	"io"
	"net/http/httptest"
	"os"
	"testing"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/mock"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
)

func TestStatusAPI(t *testing.T) {
	var log = &logrus.Logger{
		Out:       os.Stdout,
		Formatter: &logrus.TextFormatter{TimestampFormat: "2006-01-02 15:04:05.000", FullTimestamp: true},
		Hooks:     make(logrus.LevelHooks),
		Level:     logrus.DebugLevel,
	}
	tests := []struct {
		description string

		// Expected output
		expectedCode int
		expectedBody string
		wait         time.Duration
	}{
		{
			description:  "200:success keys route",
			expectedCode: 200,
			expectedBody: "{\"status\":\"ok\",\"services\":[]}",
		},
	}
	statusEndpoint := "/status"
	// Setup the app as it is done in the main function
	app := mock.Setup(log)

	for _, test := range tests {
		t.Run(test.description, func(t *testing.T) {
			req := httptest.NewRequest("GET", statusEndpoint, nil)
			req.Header.Add("Content-Type", "application/json")
			res, err := app.Test(req, -1)
			assert.NoError(t, err)
			body, err := io.ReadAll(res.Body)
			assert.NoError(t, err, test.description)
			assert.Equal(t, string(body), test.expectedBody)
			assert.Equal(t, res.StatusCode, test.expectedCode)
		})
	}
}
