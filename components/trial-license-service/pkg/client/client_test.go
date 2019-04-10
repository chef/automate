package client_test

import (
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"net/url"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/trial-license-service/pkg/client"
)

func TestRequestTrialLicense(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	cases := map[string]struct {
		firstname, lastname, email string
		gdpr                       bool
		deploymentID, automateVsn  string
		reqChecks                  []checkReqFun
		response                   responseFun
		respChecks                 []checkRespFun
	}{
		"happy path": {"alice", "schmidt", "alice@aol.com", true,
			"b36621fe-4089-42fd-8a4f-f17023cf82a1",
			"20180426151108",
			checkReq(
				hasMethod("POST"),
				hasBodyJSON(map[string]interface{}{
					"first_name":            "alice",
					"last_name":             "schmidt",
					"email":                 "alice@aol.com",
					"gdpr_agree":            true,
					"deployment_id":         "b36621fe-4089-42fd-8a4f-f17023cf82a1",
					"chef_automate_version": "20180426151108",
				}),
			),
			licenseResponse("LICENSE!"),
			checkResp(
				noError(),
				hasLicenseString("LICENSE!"),
			)},
		"server error": {"alice", "schmidt", "alice@aol.com", true,
			"b36621fe-4089-42fd-8a4f-f17023cf82a1",
			"20180426151108",
			checkReq(), // we don't bother checking stuff here
			errorResponse(),
			checkResp(
				hasStatusCodeError(),
			)},
	}

	for name, tc := range cases {
		t.Run(name, func(t *testing.T) {
			endpoint := http.NewServeMux()
			s := httptest.NewServer(endpoint)
			defer s.Close()
			u, err := url.Parse(s.URL)
			require.NoError(t, err)
			e, err := u.Parse("/create-trial")
			require.NoError(t, err)

			endpoint.HandleFunc("/create-trial", func(w http.ResponseWriter, r *http.Request) {
				for _, check := range tc.reqChecks {
					check(t, r)
				}
				tc.response(w)
			})

			cl := client.New(e)
			resp, err := cl.RequestTrialLicense(ctx,
				tc.firstname, tc.lastname, tc.email, tc.gdpr,
				tc.deploymentID, tc.automateVsn)
			for _, check := range tc.respChecks {
				check(t, resp, err)
			}
		})
	}
}

func noError() checkRespFun {
	return func(t *testing.T, _ string, err error) {
		require.NoError(t, err)
	}
}

func hasStatusCodeError() checkRespFun {
	return func(t *testing.T, _ string, err error) {
		_, ok := err.(*client.UnexpectedStatusError)
		require.True(t, ok, "expected unexpected status code error")
	}
}

func hasLicenseString(expected string) checkRespFun {
	return func(t *testing.T, actual string, _ error) {
		assert.Equal(t, expected, actual)
	}
}

func hasMethod(meth string) checkReqFun {
	return func(t *testing.T, req *http.Request) {
		assert.Equal(t, meth, req.Method)
	}
}

func hasBodyJSON(expected map[string]interface{}) checkReqFun {
	return func(t *testing.T, req *http.Request) {
		body, err := ioutil.ReadAll(req.Body)
		require.NoError(t, err)
		m := map[string]interface{}{}
		err = json.Unmarshal(body, &m)
		require.NoError(t, err)
		assert.Equal(t, expected, m)
	}
}

type checkReqFun func(*testing.T, *http.Request)
type checkRespFun func(*testing.T, string, error)

func checkReq(c ...checkReqFun) []checkReqFun {
	return c
}

func checkResp(c ...checkRespFun) []checkRespFun {
	return c
}

type responseFun func(http.ResponseWriter)

func licenseResponse(lic string) responseFun {
	return func(w http.ResponseWriter) {
		fmt.Fprintf(w, `{"license":%q}`, lic)
	}
}

func errorResponse() responseFun {
	return func(w http.ResponseWriter) {
		http.Error(w, "something went wrong", 500)
	}
}
