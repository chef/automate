package integration_test

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
	"github.com/chef/automate/components/trial-license-service/pkg/license"
	"github.com/chef/automate/components/trial-license-service/pkg/recorder"
	"github.com/chef/automate/components/trial-license-service/pkg/server"
	"github.com/chef/automate/lib/logger"
)

func TestClientAndServer(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	licenseByte, err := ioutil.ReadFile("../../../../dev/license.jwt")
	require.NoError(t, err, "could not open license file")
	licenseData := string(licenseByte)

	// unsafe for concurrent access; but we don't do that here
	m := map[string]interface{}{}

	mux := http.NewServeMux()
	mux.HandleFunc("/generate", func(w http.ResponseWriter, r *http.Request) {
		if err := json.NewDecoder(r.Body).Decode(&m); err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
		fmt.Fprintf(w, `{"license":%q}`, licenseData)
	})

	lgs := httptest.NewServer(mux)
	defer lgs.Close()
	lgsURL, err := url.Parse(lgs.URL)
	require.NoError(t, err)

	apiKey := "opensesamestreet"
	trialDays := 1234
	first, last := "Gonzo", "the Great"
	email := "gonzo@royalflu.sh"
	gdprAgree := true
	deploymentID := "9e263d13-1f0d-430c-804b-9515e7a368ef"
	automateVersion := "20180503125136"

	f := license.NewFetcher(lgsURL, apiKey, trialDays)
	l := logger.NewTestLogger()

	servMux := server.NewServeMux(f, recorder.NoopRecorder(), l)
	serv := httptest.NewServer(servMux)
	defer serv.Close()
	u, err := url.Parse(serv.URL + "/create-trial")
	require.NoError(t, err)

	cl := client.New(u)
	lic, err := cl.RequestTrialLicense(ctx, first, last, email, gdprAgree, deploymentID, automateVersion)
	require.NoError(t, err)
	assert.Equal(t, licenseData, lic)

	assert.Equal(t, "Gonzo the Great <gonzo@royalflu.sh> - TRIAL", m["customer"])
}
