package client

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/http"
	"net/url"

	"github.com/pkg/errors"
)

// Client is the interface fulfilled by a trial-license-service client
type Client interface {
	RequestTrialLicense(ctx context.Context,
		firstname, lastname, email string, gdprAgree bool,
		deploymentID, automateVersion string) (string, error)
}

// UnexpectedStatusError is returned when the trial-license-service responds
// with a non-200 status code
type UnexpectedStatusError struct {
	code    int
	message string
}

func (e *UnexpectedStatusError) Error() string {
	if e.message != "" {
		return fmt.Sprintf("unexpected response %q (code %d)", e.message, e.code)
	}
	return fmt.Sprintf("unexpected response status code: %d", e.code)
}

// Code returns the status code returned by the upstream service
func (e *UnexpectedStatusError) Code() int {
	return e.code
}

// client holds a trial-license-service client's state
type client struct {
	endpoint *url.URL
}

// New returns a Client struct pointing at the passed endpoint for
// trial-license-service
func New(endpoint *url.URL) Client {
	return &client{endpoint: endpoint}
}

func (c *client) RequestTrialLicense(ctx context.Context,
	firstname, lastname, email string, gdprAgree bool,
	deploymentID, automateVersion string) (string, error) {

	body := struct {
		FirstName           string `json:"first_name"`
		LastName            string `json:"last_name"`
		Email               string `json:"email"`
		GDPRAgree           bool   `json:"gdpr_agree"`
		DeploymentID        string `json:"deployment_id"`
		ChefAutomateVersion string `json:"chef_automate_version"`
	}{
		firstname,
		lastname,
		email,
		gdprAgree,
		deploymentID,
		automateVersion,
	}
	reqBody, err := json.Marshal(body)
	if err != nil {
		return "", errors.Wrap(err, "marshal request body")
	}

	req, err := http.NewRequest("POST", c.endpoint.String(), bytes.NewReader(reqBody))
	if err != nil {
		return "", errors.Wrap(err, "send trial license request")
	}
	req.WithContext(ctx)
	req.Header.Set("Content-Type", "application/json")

	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return "", errors.Wrap(err, "send trial license request")
	}
	defer resp.Body.Close() // nolint: errcheck

	if resp.StatusCode != http.StatusOK {
		var body []byte
		body, err = ioutil.ReadAll(resp.Body)
		if err != nil {
			return "", &UnexpectedStatusError{code: resp.StatusCode}
		}
		return "", &UnexpectedStatusError{code: resp.StatusCode, message: string(body)}
	}

	r := struct {
		License string `json:"license"`
	}{}
	err = json.NewDecoder(resp.Body).Decode(&r)
	if err != nil {
		return "", errors.Wrap(err, "decode trial license response")
	}
	return r.License, nil
}
