package server

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	jwt "github.com/dgrijalva/jwt-go"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/trial-license-service/pkg/license"
	"github.com/chef/automate/components/trial-license-service/pkg/recorder"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/version"
)

func TestStatusHandler(t *testing.T) {
	mux := setupTestMux(nil, recorder.NoopRecorder())
	// Create request to handler we want to test
	rr := httptest.NewRecorder()
	req := httptest.NewRequest("GET", "/status", nil)
	// Sends request to tested handler
	mux.ServeHTTP(rr, req)
	// Checks ResponseRecorder for results
	assert.Equal(t, http.StatusOK, rr.Code)
	assert.ElementsMatch(t, []string{"application/json; charset=utf-8"}, rr.Header()["Content-Type"])

	m := map[string]string{}
	err := json.NewDecoder(rr.Body).Decode(&m)
	require.NoError(t, err)
	expected := map[string]string{
		"status": "OK",
	}
	assert.Equal(t, expected, m)
}

func TestVersionHandler(t *testing.T) {
	version.Version = "pkg_version/pkg_release"
	mux := setupTestMux(nil, recorder.NoopRecorder())
	// Create request to handler we want to test
	rr := httptest.NewRecorder()
	req := httptest.NewRequest("GET", "/version", nil)
	// Sends request to tested handler
	mux.ServeHTTP(rr, req)
	// Checks ResponseRecorder for results
	assert.Equal(t, http.StatusOK, rr.Code)
	assert.ElementsMatch(t, []string{"application/json; charset=utf-8"}, rr.Header()["Content-Type"])

	m := map[string]string{}
	err := json.NewDecoder(rr.Body).Decode(&m)
	require.NoError(t, err)
	expected := map[string]string{
		"version": "pkg_version/pkg_release",
	}
	assert.Equal(t, expected, m)
}

func TestCreateTrialHandler(t *testing.T) {
	// This is consumed when processing the request, so each valid payload needs
	// to be "fresh" for its test case.
	validPayload := func() io.Reader {
		return trialReqBody(t, sampleTrialRequest())
	}
	noRecordings := func(t *testing.T, rs []entry) {
		assert.Zero(t, len(rs))
	}
	oneRecording := func(gdpr bool) func(*testing.T, []entry) {
		return func(t *testing.T, rs []entry) {
			require.Equal(t, 1, len(rs))
			assert.Equal(t, "Sherry Gess", rs[0].name)
			assert.Equal(t, "SherryLGess@clickright.com", rs[0].email)
			assert.Equal(t, "fd0e259d-aa76-4e76-88e6-11cefd0a723a", rs[0].customerID)
			assert.Equal(t, "9916b10f-9ce9-4e94-91f8-0d782c3a6836", rs[0].licenseID)
			assert.Equal(t, "b36621fe-4089-42fd-8a4f-f17023cf82a1", rs[0].deploymentID)
			assert.Equal(t, "20180426151108", rs[0].automateVersion)
			assert.Equal(t, gdpr, rs[0].gdprAgree)
		}
	}
	cases := map[string]struct {
		fetcher        license.Fetcher
		payload        io.Reader
		checkFuncs     []checkFunc
		recordCheckFun func(*testing.T, []entry)
	}{
		"happy path": {
			&successFetcher{},
			validPayload(),
			checks(
				hasStatus(http.StatusOK),
				hasResponseWithKey("license"),
			),
			oneRecording(true),
		},
		"happy path without GDPR agreement": {
			&successFetcher{},
			trialReqBody(t, func() trialRequest {
				req := sampleTrialRequest()
				req.GDPRAgree = false
				return req
			}()),
			checks(hasStatus(http.StatusOK)),
			oneRecording(false),
		},
		"fetcher fails (client error)": {
			&failFetcher{&license.ClientError{}},
			validPayload(),
			checks(hasStatus(http.StatusBadRequest)),
			noRecordings,
		},
		"fetcher fails (server error)": {
			&failFetcher{&license.ServerError{}},
			validPayload(),
			checks(hasStatus(http.StatusInternalServerError)),
			noRecordings,
		},
		"bad request payload": {
			&successFetcher{},
			strings.NewReader("bahbahbah"),
			checks(hasStatus(http.StatusBadRequest)),
			noRecordings,
		},
		"missing field payload (firstname)": {
			&successFetcher{},
			trialReqBody(t, func() trialRequest {
				req := sampleTrialRequest()
				req.FirstName = ""
				return req
			}()),
			checks(hasStatus(http.StatusBadRequest)),
			noRecordings,
		},
		"missing field payload (lastname)": {
			&successFetcher{},
			trialReqBody(t, func() trialRequest {
				req := sampleTrialRequest()
				req.LastName = ""
				return req
			}()),
			checks(hasStatus(http.StatusBadRequest)),
			noRecordings,
		},
		"missing field payload (email)": {
			&successFetcher{},
			trialReqBody(t, func() trialRequest {
				req := sampleTrialRequest()
				req.Email = ""
				return req
			}()),
			checks(hasStatus(http.StatusBadRequest)),
			noRecordings,
		},
		"missing field payload (deployment ID)": {
			&successFetcher{},
			trialReqBody(t, func() trialRequest {
				req := sampleTrialRequest()
				req.DeploymentID = ""
				return req
			}()),
			checks(hasStatus(http.StatusBadRequest)),
			noRecordings,
		},
		"missing field payload (automate version)": {
			&successFetcher{},
			trialReqBody(t, func() trialRequest {
				req := sampleTrialRequest()
				req.ChefAutomateVersion = ""
				return req
			}()),
			checks(hasStatus(http.StatusBadRequest)),
			noRecordings,
		},
	}

	for name, tc := range cases {
		t.Run(name, func(t *testing.T) {
			r := &rec{}
			mux := setupTestMux(tc.fetcher, r)
			rr := httptest.NewRecorder()

			req := httptest.NewRequest("POST", "/create-trial", tc.payload)
			mux.ServeHTTP(rr, req)

			for _, check := range tc.checkFuncs {
				check(t, rr)
			}

			tc.recordCheckFun(t, r.entries)
		})
	}
}

// helpers

func sampleTrialRequest() trialRequest {
	return trialRequest{
		FirstName:           "Sherry",
		LastName:            "Gess",
		Email:               "SherryLGess@clickright.com",
		GDPRAgree:           true,
		DeploymentID:        "b36621fe-4089-42fd-8a4f-f17023cf82a1",
		ChefAutomateVersion: "20180426151108",
	}
}

func trialReqBody(t *testing.T, r trialRequest) io.Reader {
	t.Helper()
	buf := bytes.Buffer{}
	if err := json.NewEncoder(&buf).Encode(r); err != nil {
		t.Fatal(err)
	}

	return &buf
}

func setupTestMux(fetcher license.Fetcher, rec recorder.Client) *http.ServeMux {
	return NewServeMux(fetcher, rec, logger.NewTestLogger())
}

type checkFunc func(*testing.T, *httptest.ResponseRecorder)

func checks(f ...checkFunc) []checkFunc {
	return f
}

func hasStatus(code int) checkFunc {
	return func(t *testing.T, rr *httptest.ResponseRecorder) {
		require.Equal(t, code, rr.Code)
	}
}

func hasResponseWithKey(expected string) checkFunc {
	return func(t *testing.T, rr *httptest.ResponseRecorder) {
		actual := map[string]interface{}{}
		err := json.NewDecoder(rr.Body).Decode(&actual)
		require.NoError(t, err)
		assert.NotZero(t, actual[expected])
	}
}

type successFetcher struct{}

func testLicense() string {
	payload := struct {
		ID string `json:"id"`
	}{"9916b10f-9ce9-4e94-91f8-0d782c3a6836"}

	buf := bytes.Buffer{}
	if err := json.NewEncoder(&buf).Encode(payload); err != nil {
		panic(err)
	}
	return fmt.Sprintf("hdr.%s.sign", jwt.EncodeSegment(buf.Bytes()))
}

func (*successFetcher) RequestLicense(context.Context, string) (*license.TrialLicense, error) {
	return &license.TrialLicense{
		License:    testLicense(),
		CustomerID: "fd0e259d-aa76-4e76-88e6-11cefd0a723a",
	}, nil
}

type failFetcher struct {
	err error
}

func (f *failFetcher) RequestLicense(context.Context, string) (*license.TrialLicense, error) {
	return nil, f.err
}

type rec struct {
	entries []entry
}
type entry struct {
	licenseID, customerID, name, email string
	automateVersion, deploymentID      string
	gdprAgree                          bool
}

func (rs *rec) Record(licenseID, customerID, name, email,
	automateVersion, deploymentID string,
	gdpr bool) error {
	rs.entries = append(rs.entries, entry{licenseID, customerID, name, email,
		automateVersion, deploymentID, gdpr})
	return nil
}
