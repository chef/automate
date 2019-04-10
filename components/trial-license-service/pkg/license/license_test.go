package license_test

import (
	"context"
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"net/url"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/trial-license-service/pkg/license"
)

func TestTrialLicenseIDValidLicense(t *testing.T) {
	// this is dev/license.jwt (as of commit 4806ce7f60f)
	// nolint: lll
	jwt := `eyJhbGciOiJFUzUxMiIsInR5cCI6IkpXVCJ9.eyJpZCI6ImE5NTNjNWJiLTgyYTUtNDFiZS1iN2RkLWE1ZGUxZWE1M2FkYSIsInZlcnNpb24iOiIxIiwidHlwZSI6ImNvbW1lcmNpYWwiLCJnZW5lcmF0b3IiOiJjaGVmL2xpY2Vuc2UtMi4wLjAiLCJrZXlfc2hhMjU2IjoiZTBkZjI4YzhiYzY4MTUwZWRiZmVmOThjZDZiN2RjNDM5YzFmODBjN2U3ZWY3NDc4OTNhNjg5M2EyZjdiNjBmNyIsImdlbmVyYXRpb25fZGF0ZSI6eyJzZWNvbmRzIjoxNTE4NjMzMTc2fSwiY3VzdG9tZXIiOiJDaGVmIERldiIsImN1c3RvbWVyX2lkIjoiMDAwMDAwMDAwMDAwMDAwMCIsImN1c3RvbWVyX2lkX3ZlcnNpb24iOiIxIiwiZW50aXRsZW1lbnRzIjpbeyJuYW1lIjoiYmFzZSIsIm1lYXN1cmUiOiJub2RlcyIsImxpbWl0IjoxMDAsInN0YXJ0Ijp7InNlY29uZHMiOjE1MTg1NjY0MDB9LCJlbmQiOnsic2Vjb25kcyI6MTU4MTcyNDc5OX19XX0.AQ7MlTtGhsYYt9PV2G3oSNvGCRdNX_Itsj-uTLnbF43MwZlP4ZFCYK-v2rtcTQfjBx91ZeFgY7-Y0i6V9hPvFmmqAANMFRmbbupEgy6cxOJUzQblxxLF2JgbrOKc6mQprskerNSKA6wKNJqkvh0mQF-42WTOOZAKekoRHDsHCYHweFOO`

	tl := license.TrialLicense{License: jwt}
	actual, err := tl.ID()
	require.NoError(t, err)
	expected := "a953c5bb-82a5-41be-b7dd-a5de1ea53ada"
	assert.Equal(t, expected, actual)
}

func TestTrialLicenseIDInvalidLicenses(t *testing.T) {
	cases := []string{
		`foo`,
		`foo.bar.baz`,
		// valid JWT payload, but not a license payload
		// nolint: lll
		`HEADER.eyJpc3MiOiJodHRwczovL2EyLWRldi50ZXN0L2RleCIsInN1YiI6IkNpUmlaVGcyWldRMU9DMHdNR1kzTFRRMk1qSXRPR000WmkwMk0yRm1ZVEJoTWprNU9UUVNCV3h2WTJGcyIsImF1ZCI6ImF1dG9tYXRlLXNlc3Npb24iLCJleHAiOjE1MjYzNzI1MTAsImlhdCI6MTUyNjM3MjMzMCwiYXRfaGFzaCI6InV6a0w0XzBwZjdaNThJQnNabmpkZHciLCJlbWFpbCI6ImFkbWluIiwiZW1haWxfdmVyaWZpZWQiOnRydWUsIm5hbWUiOiJMb2NhbCBBZG1pbmlzdHJhdG9yIiwiZmVkZXJhdGVkX2NsYWltcyI6eyJjb25uZWN0b3JfaWQiOiJsb2NhbCIsInVzZXJfaWQiOiJiZTg2ZWQ1OC0wMGY3LTQ2MjItOGM4Zi02M2FmYTBhMjk5OTQifX0.SIGNATURE`,
	}

	for _, tc := range cases {
		t.Run(fmt.Sprintf("%.10s", tc) /* abbreviate */, func(t *testing.T) {
			tl := license.TrialLicense{License: tc}
			_, err := tl.ID()
			assert.Error(t, err)
			t.Log(err)
		})
	}
}

func TestFetcherRequestLicense(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	apiKey := "opensesamestreet"
	trialDays := 34
	customerName := "Gonzo's Royal Flush"

	cases := map[string]struct {
		apiKey       string
		customerName string
		trialDays    int
		reqChecks    []checkReqFun
		response     responseFun
		respChecks   []checkRespFun
	}{
		"happy path": {
			apiKey,
			customerName,
			trialDays,
			checkReq(
				hasMethod("POST"),
				hasHeader("content-type", "application/json"),
				hasHeader("Auth-Token", apiKey),
				hasFields(
					"customer", customerName,
					"customer_id_version", "2",
					"version", "1",
					"type", "trial",
					// NOTE: we're not checking the entitlement's start/end here
					"entitlements", containsEntitlements(map[string]interface{}{
						"name":    "Chef Automate Trial",
						"measure": "days",
						"limit":   float64(trialDays), // float?! well, doesn't matter, I guess.
					}),
					"customer_id", nonEmpty,
				),
			),
			licenseResponse("LICENSE124!"),
			checkResp(
				noError(),
				hasLicenseString("LICENSE124!"),
			)},
		"server error (4xx)": {
			apiKey,
			customerName,
			trialDays,
			checkReq(), // not checking here
			errorResponse(http.StatusBadRequest),
			checkResp(
				hasError(&license.ClientError{}),
			)},
		"server error (5xx)": {
			apiKey,
			customerName,
			trialDays,
			checkReq(), // not checking here
			errorResponse(http.StatusInternalServerError),
			checkResp(
				hasError(&license.ServerError{}),
			)},
	}

	for name, tc := range cases {
		t.Run(name, func(t *testing.T) {
			mux := http.NewServeMux()
			lgs := httptest.NewServer(mux)
			defer lgs.Close()
			lgsURL, err := url.Parse(lgs.URL)
			require.NoError(t, err)

			f := license.NewFetcher(lgsURL, tc.apiKey, tc.trialDays)

			mux.HandleFunc("/generate", func(w http.ResponseWriter, r *http.Request) {
				for _, check := range tc.reqChecks {
					check(t, r)
				}
				tc.response(w)
			})

			resp, err := f.RequestLicense(ctx, tc.customerName)
			for _, check := range tc.respChecks {
				check(t, resp, err)
			}
		})
	}
}

type checkReqFun func(*testing.T, *http.Request)
type checkRespFun func(*testing.T, *license.TrialLicense, error)

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

func errorResponse(code int) responseFun {
	return func(w http.ResponseWriter) {
		http.Error(w, "something went wrong", code)
	}
}

func noError() checkRespFun {
	return func(t *testing.T, _ *license.TrialLicense, err error) {
		require.NoError(t, err)
	}
}

func hasError(expected interface{}) checkRespFun {
	return func(t *testing.T, _ *license.TrialLicense, err error) {
		require.IsType(t, expected, err)
	}
}

func hasLicenseString(expected string) checkRespFun {
	return func(t *testing.T, actual *license.TrialLicense, _ error) {
		assert.Equal(t, expected, actual.License)
	}
}

func hasMethod(meth string) checkReqFun {
	return func(t *testing.T, req *http.Request) {
		assert.Equal(t, meth, req.Method)
	}
}

func hasHeader(key, val string) checkReqFun {
	return func(t *testing.T, req *http.Request) {
		assert.Equal(t, val, req.Header.Get(key))
	}
}

// We go through the hassle of passing all expectations into ONE helper to avoid
// having to (deep-)copy the request body -- which can only be read once.
func hasFields(expectations ...interface{}) checkReqFun {
	return func(t *testing.T, req *http.Request) {
		require.Equalf(t, 0, len(expectations)%2, "expectations needs to come in pairs!")
		m := map[string]interface{}{}
		err := json.NewDecoder(req.Body).Decode(&m)
		require.NoError(t, err)

		for i := 0; i < len(expectations)/2; i++ {
			field := expectations[2*i].(string)
			actual, ok := m[field]
			switch expected := expectations[2*i+1].(type) {
			case string:
				require.Truef(t, ok, "field %q not found", field)
				assert.Equal(t, expected, actual)
			case func(*testing.T, interface{}):
				require.Truef(t, ok, "field %q not found", field)
				expected(t, actual)
			default:
				t.Errorf("bad expectation for field %q", field)
			}
		}
	}
}

func nonEmpty(t *testing.T, str interface{}) {
	s, ok := str.(string)
	require.True(t, ok)
	assert.NotZero(t, s)
}

func containsEntitlements(expected map[string]interface{}) func(*testing.T, interface{}) {
	return func(t *testing.T, x interface{}) {
		es, ok := x.([]interface{})
		require.True(t, ok)
		assert.Equal(t, 1, len(es))
		m, ok := es[0].(map[string]interface{})
		require.True(t, ok)
		for k, v := range expected {
			assert.Equal(t, v, m[k])
		}
	}
}
