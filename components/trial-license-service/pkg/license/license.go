// Package license makes HTTP requests to a license generation service to obtain trial licenses
package license

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strings"
	"time"

	jwt "github.com/dgrijalva/jwt-go"
	"github.com/gofrs/uuid"
	"github.com/pkg/errors"
	"golang.org/x/net/context/ctxhttp"
)

const (
	trialLicenseType  = "trial"
	licenseVersion    = "1"
	customerIDVersion = "2"
)

// ServerError is returned when the license-generation-server returns an error
// code in the 5xx range
type ServerError struct {
	httpError
}

// ClientError is returned when the license-generation-server returns an error
// code in the 4xx range
type ClientError struct {
	httpError
}

type httpError struct {
	code int
}

func (h *httpError) Error() string {
	return fmt.Sprintf("unexpected return code: %d", h.code)
}

// Fetcher requests licenses from a license-generation-service.
type Fetcher interface {
	RequestLicense(ctx context.Context, customerName string) (license *TrialLicense, err error)
}

type fetcher struct {
	licGenURL         *url.URL
	trialDays         int
	customerIDVersion string
	licenseVersion    string
	apiKey            string
}

// NewFetcher returns a new trial license fetcher configured to talk to the
// license-generation-service at licGenURL. The fetcher will request license
// that expire trialDays from now.
func NewFetcher(licGenURL *url.URL, apiKey string, trialDays int) Fetcher {
	return &fetcher{
		licGenURL:         licGenURL,
		trialDays:         trialDays,
		customerIDVersion: customerIDVersion,
		licenseVersion:    licenseVersion,
		apiKey:            apiKey,
	}
}

// licenseRequest represents the request we need to send to a license-generation-service
type licenseRequest struct {
	Version           string        `json:"version"`
	Type              string        `json:"type"`
	Customer          string        `json:"customer"`
	CustomerID        string        `json:"customer_id"`
	CustomerIDVersion string        `json:"customer_id_version"`
	Entitlements      []entitlement `json:"entitlements"`
}

type entitlement struct {
	Name    string `json:"name"`
	Measure string `json:"measure"`
	Limit   int    `json:"limit"`
	Start   string `json:"start"`
	End     string `json:"end"`
}

// TrialLicense is returned by RequestLicense and contains the generated License
// and the assigned CustomerID.
type TrialLicense struct {
	License    string
	CustomerID string
}

// ID returns the TrialLicense's ID (decoded from the license JWT)
func (tl *TrialLicense) ID() (string, error) {
	parts := strings.Split(tl.License, ".")
	if len(parts) != 3 {
		return "", errors.New("invalid JWT")
	}

	data, err := jwt.DecodeSegment(parts[1])
	if err != nil {
		return "", errors.Wrap(err, "base64url-decode JWT payload")
	}

	payload := struct {
		ID string `json:"id"`
	}{}

	if err := json.NewDecoder(bytes.NewReader(data)).Decode(&payload); err != nil {
		return "", errors.Wrap(err, "json-decode JWT payload")
	}
	if payload.ID == "" {
		return "", errors.New("no id in payload")
	}

	return payload.ID, nil
}

// RequestLicense requests a new trial license from the license generation
// service for the provided customerName.
func (f *fetcher) RequestLicense(ctx context.Context, customerName string) (*TrialLicense, error) {
	licReq := newLicenseRequest(customerName, f.trialDays)
	postReq, err := f.makeHTTPRequest(licReq)
	if err != nil {
		return nil, err
	}

	resp, err := ctxhttp.Do(ctx, http.DefaultClient, postReq)
	if err != nil {
		return nil, err
	}
	defer resp.Body.Close() // nolint: errcheck

	code := resp.StatusCode
	switch { // Do we care? I'm not sure. Seems like 5xx might be retryable...
	case code >= 400 && code < 500:
		return nil, &ClientError{httpError: httpError{code: code}}
	case code >= 500 && code < 600:
		return nil, &ServerError{httpError: httpError{code: code}}
	}

	lic, err := processLicGenResult(resp.Body)
	if err != nil {
		return nil, err
	}

	return &TrialLicense{
		License:    lic,
		CustomerID: licReq.CustomerID,
	}, nil
}

func (f *fetcher) makeHTTPRequest(licReq licenseRequest) (*http.Request, error) {
	genPath, err := f.licGenURL.Parse("/generate")
	if err != nil {
		return nil, errors.Wrap(err, "construct endpoint")
	}

	buf := bytes.Buffer{}
	err = json.NewEncoder(&buf).Encode(licReq)
	if err != nil {
		return nil, err
	}

	postReq, err := http.NewRequest("POST", genPath.String(), &buf)
	if err != nil {
		return nil, err
	}
	postReq.Header.Set("content-type", "application/json")
	postReq.Header.Set("auth-token", f.apiKey)
	return postReq, nil
}

func processLicGenResult(bs io.ReadCloser) (string, error) {
	licRes := struct {
		License string `json:"license"`
	}{}
	err := json.NewDecoder(bs).Decode(&licRes)
	if err != nil {
		return "", err
	}

	return licRes.License, nil
}

func newLicenseRequest(customerName string, days int) licenseRequest {
	return licenseRequest{
		// These two are constants
		Version: licenseVersion,
		Type:    trialLicenseType,

		Customer:          customerName,
		CustomerID:        uuid.Must(uuid.NewV4()).String(),
		CustomerIDVersion: customerIDVersion,
		Entitlements:      []entitlement{newTrialEntitlement(time.Now(), days)},
	}
}

func newTrialEntitlement(startTime time.Time, days int) entitlement {
	endTime := startTime.AddDate(0, 0, days)
	return entitlement{
		Name:    "Chef Automate Trial",
		Measure: "days",
		Limit:   days,
		Start:   formatTime(startTime),
		End:     formatTime(endTime),
	}
}

func formatTime(t time.Time) string {
	return t.Format("2006-01-02T15:04:05Z")
}
