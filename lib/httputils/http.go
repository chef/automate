package httputils

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"time"

	"github.com/chef/automate/lib/logger"
)

// HTTPClient interface represents an HTTP client.
type HTTPClient interface {
	MakeRequest(requestMethod, url string, body interface{}) (*http.Response, error)
}

// Client is a wrapper around http.Client that adds some convenience methods for making requests and handling responses.
type Client struct {
	Client *http.Client
	Logger logger.Logger
}

// NewClient returns a new Client with sane defaults.
func NewClient(logger logger.Logger) *Client {
	return &Client{
		Client: http.DefaultClient,
		Logger: logger,
	}
}

// NewClientWithTimeout returns a new Client with the specified timeout.
func NewClientWithTimeout(timeout time.Duration, logger logger.Logger) *Client {
	return &Client{
		Client: &http.Client{
			Timeout: timeout,
		},
		Logger: logger,
	}
}

// MakeRequest sends an HTTP request with the specified request method, URL, and body.
func (c *Client) MakeRequest(requestMethod, url string, body interface{}) (*http.Response, error) {
	var reader io.Reader

	// Marshal the request body to JSON if it's not nil
	if body != nil {
		requestBody, err := json.Marshal(body)
		if err != nil {
			return nil, fmt.Errorf("failed to marshal request body: %w", err)
		}
		reader = bytes.NewReader(requestBody)
	}

	req, err := http.NewRequest(requestMethod, url, reader)
	if err != nil {
		return nil, fmt.Errorf("failed to create HTTP request: %w", err)
	}

	req.Header.Set("Content-Type", "application/json")

	resp, err := c.Client.Do(req)
	if err != nil {
		return nil, fmt.Errorf("failed to make HTTP request: %w", err)
	}

	if resp.StatusCode != http.StatusOK {
		return nil, fmt.Errorf("unexpected response status: %s", resp.Status)
	}

	return resp, nil
}
