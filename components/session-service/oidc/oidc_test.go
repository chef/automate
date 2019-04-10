package oidc

import (
	"crypto/tls"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/http/httptest"
	"net/url"
	"testing"

	"github.com/gorilla/mux"
	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/lib/tls/certs"
	"github.com/chef/automate/lib/tls/test/helpers"
)

func TestHTTPClientForIssuer(t *testing.T) {
	dexServer := mux.NewRouter()
	dexServer.HandleFunc("/dex", func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprint(w, "piñata")
	})
	p := httptest.NewUnstartedServer(dexServer)
	dexCerts := devDexCerts(t)
	p.TLS = &tls.Config{
		Certificates: []tls.Certificate{*dexCerts.ServiceKeyPair},
	}
	p.StartTLS()
	defer p.Close()

	dexURL, _ := url.Parse(p.URL)
	hc := httpClientForIssuer("automate-dex", dexURL, devSessionCerts(t))

	issuers := []string{"https://localhost/dex", "https://some-real-fqdn/dex"}
	for _, iss := range issuers {
		t.Run(fmt.Sprintf("issuer is %#v", iss), func(t *testing.T) {
			resp, err := hc.Get(iss)
			if assert.Nil(t, err) {
				body, _ := ioutil.ReadAll(resp.Body)
				assert.Equal(t, "piñata", string(body), "answer from lb")
			}
		})
	}
}

func devDexCerts(t *testing.T) *certs.ServiceCerts {
	return devCerts(t, "automate-dex")
}

func devSessionCerts(t *testing.T) *certs.ServiceCerts {
	return devCerts(t, "session-service")
}

func devCerts(t *testing.T, name string) *certs.ServiceCerts {
	return helpers.LoadDevCerts(t, name)
}
