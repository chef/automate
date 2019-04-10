package mock

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"go.uber.org/zap"
)

var logger *zap.Logger

func init() {
	cfg := zap.NewProductionConfig()
	cfg.Level.SetLevel(zap.ErrorLevel)
	logger, _ = cfg.Build()
}
func TestAuthenticate(t *testing.T) {
	tests := map[string]struct {
		issuer, audience                 string
		groups                           []string
		expectedIssuer, expectedAudience string
		expectedError                    bool
		expectedTeams                    []string
	}{
		"matching issuer and audience": {
			issuer:           "foo",
			audience:         "client",
			expectedIssuer:   "foo",
			expectedAudience: "client",
		},
		"matching issuer and audience, and configured groups": {
			issuer:           "foo",
			audience:         "client",
			groups:           []string{"users"},
			expectedIssuer:   "foo",
			expectedAudience: "client",
			expectedTeams:    []string{"team:mockmock:users"},
		},
		"mismatching issuer": {
			issuer:           "bar",
			audience:         "client",
			expectedIssuer:   "foo",
			expectedAudience: "client",
			expectedError:    true,
		},
		"mismatching audience": {
			issuer:           "foo",
			audience:         "bar",
			expectedIssuer:   "foo",
			expectedAudience: "client",
			expectedError:    true,
		},
		"mismatching issuer and audience": {
			issuer:           "foo",
			audience:         "bar",
			expectedIssuer:   "baz",
			expectedAudience: "client",
			expectedError:    true,
		},
	}

	for name, d := range tests {
		t.Run(name, func(t *testing.T) {
			subject := "alice"
			authn := NewAuthenticator(d.expectedIssuer, d.expectedAudience, d.groups, "mockmock", "mockuser", "", logger)
			request := newRequest(GenerateMockJWT(subject, d.issuer, d.audience))
			requestor, err := authn.Authenticate(request)

			if d.expectedError {
				assert.NotNil(t, err)
			} else {
				require.NotNil(t, requestor)
				assert.Equal(t, "user:mockmock:mockuser", requestor.Subject())
				if d.expectedTeams != nil {
					assert.ElementsMatch(t, d.expectedTeams, requestor.Teams())
				}
			}
		})
	}
}

func newRequest(token string) *http.Request {
	req := httptest.NewRequest("", "/thisDoesNotMatter", nil)
	req.Header.Add("Authorization", fmt.Sprintf("bearer %s", token))
	return req
}
