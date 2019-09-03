package license

import (
	"bytes"
	"context"
	"encoding/json"
	"io/ioutil"
	"net/url"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestProcessLicGenResult(t *testing.T) {
	inBytes := []byte(`{"license":"abc123", "foo":"", "bar":12}`)
	expect := "abc123"
	got, err := processLicGenResult(ioutil.NopCloser(bytes.NewReader(inBytes)))
	assert.NoError(t, err)
	assert.Equal(t, expect, got)
}

func TestMakeHTTPRequest(t *testing.T) {
	genURL, err := url.Parse("/generate")
	require.NoError(t, err)

	f := &fetcher{
		licGenURL:         genURL,
		trialDays:         34,
		customerIDVersion: "1",
		licenseVersion:    "2",
		apiKey:            "sesame5",
	}
	licReq, err := f.makeHTTPRequest(context.Background(), licenseRequest{Customer: "ACME Inc", Type: "trial"})
	assert.NoError(t, err)

	assert.Equal(t, "application/json", licReq.Header.Get("content-type"))
	assert.Equal(t, "sesame5", licReq.Header.Get("auth-token"))
	rawBody, err := ioutil.ReadAll(licReq.Body)
	data := &licenseRequest{}
	err = json.Unmarshal(rawBody, data)
	assert.NoError(t, err)
	assert.Equal(t, "trial", data.Type)
}
