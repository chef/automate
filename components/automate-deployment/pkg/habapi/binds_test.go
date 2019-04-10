package habapi_test

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/habapi"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
)

func TestAllConsumingServicesReturnsConsumingServices(t *testing.T) {
	responseData := `[
{"binds": ["automate-dex:automate-dex.default"],
 "pkg": { "name": "acme", "origin": "core", "release": "20170101010101", "version": "1.2.3", "channel": "dev"}},
{"binds": [], "pkg": { "name": "acme2", "origin": "core", "release": "20170101010101", "version": "1.2.3", "channel": "dev"}}]`

	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintln(w, responseData)
	}))
	defer ts.Close()

	c := habapi.New(ts.URL)
	pkg := habpkg.New("chef", "automate-dex")
	response, err := habapi.AllConsumingServices(c, &pkg)
	require.NoError(t, err)
	require.Equal(t, 1, len(response))
	assert.Equal(t, habpkg.NewFQ("core", "acme", "1.2.3", "20170101010101"), response[0])
}

func TestAllConsumingServicesReturnsErrorIfBindsAreMalformed(t *testing.T) {
	responseData := `[
{"binds": ["automate-dex:automate-dex.default"],
 "pkg": { "name": "acme", "origin": "core", "release": "20170101010101", "version": "1.2.3", "channel": "dev"}},
{"binds": ["wat"], "pkg": { "name": "acme2", "origin": "core", "release": "20170101010101", "version": "1.2.3", "channel": "dev"}}]`

	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintln(w, responseData)
	}))
	defer ts.Close()

	c := habapi.New(ts.URL)
	pkg := habpkg.New("chef", "automate-dex")
	_, err := habapi.AllConsumingServices(c, &pkg)
	require.Error(t, err)
}
