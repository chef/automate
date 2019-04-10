// Package habapi provides access to the Habitat supervisor's HTTP API
//
// The Habitat supervisor's API contains information about
// the running services.
//
// This package is currently incomplete and contains only those API
// functions used in other parts of automate-deployment.
package habapi

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"net/url"
	"path"
	"time"

	"github.com/sirupsen/logrus"
)

// A Client is a Habitat API client.
type Client struct {
	// BaseURI is the Habitat API endpoint that this client should
	// talk to.  For example: "http://127.0.0.1:9631"
	BaseURI *url.URL
	// HTTPClient is the underlying http.Client used to
	// communicate with the Habitat API.
	//
	// NOTE(ssd) 2018-01-24: We use a concrete type here since
	// there is no standard http client interface. The standard
	// advice is that if you need to swap something out, you can
	// swap out the Transport field in the http.Client. We don't
	// currently use this for testing since we start a test http
	// server instead.
	HTTPClient http.Client
}

// HabServiceInfoAPIClient interface describes things that can get the ServiceInfo
type HabServiceInfoAPIClient interface {
	ServiceInfo(ctx context.Context, serviceName string, serviceGroup string) (ServiceInfo, error)
}

// ServiceInfo contains information about a service according to the
// Habitat supervisor. Note that this structure currently only
// contains a small portion of the data Habitat actually knows about.
type ServiceInfo struct {
	Binds          []string               `json:"binds"`
	Pkg            ServicePkg             `json:"pkg"`
	StartStyle     string                 `json:"start_style"`
	UpdateStrategy string                 `json:"update_strategy"`
	Process        ServiceProcess         `json:"process"`
	Sys            SysInfo                `json:"sys"`
	DesiredState   string                 `json:"desired_state"`
	Cfg            map[string]interface{} `json:"cfg"`
}

type SysInfo struct {
	GossipIP        string `json:"gossip_ip"`
	GossipPort      uint32 `json:"gossip_port"`
	Hostname        string `json:"hostname"`
	HTTPGatewayIP   string `json:"http_gateway_ip"`
	HTTPGatewayPort uint32 `json:"http_gateway_port"`
	IP              string `json:"ip"`
	MemberID        string `json:"member_id"`
	Permanent       bool   `json:"permanent"`
	Version         string `json:"version"`
}

type ServiceProcess struct {
	PID              uint64 `json:"pid"`
	State            string `json:"state"`
	TimeStateEntered uint64 `json:"state_entered"`
}

// ServicePkg represents package information for a service. Note
// that this structure is currently incomplete when compared to the
// API return.
type ServicePkg struct {
	Origin  string `json:"origin"`
	Name    string `json:"name"`
	Version string `json:"version"`
	Release string `json:"release"`
	Channel string `json:"channel"`
}

// ServiceStatus represents the current health of a service according
// to the Habitat supervisor. This is returned by ServiceHealth.
type ServiceStatus int

const (
	// StatusOk indicates that the health endpoint has
	// returned 200 OK.
	StatusOk ServiceStatus = iota
	// StatusWarning is included for completeness but is not
	// currently returned because the health API does not
	// distinguish between OK and WARNING (both return 200 OK) and
	// the other API that may provide insight appears to have a
	// bug:
	//
	//     https://github.com/habitat-sh/core-plans/issues/1474
	StatusWarning
	// StatusCritical indicates that the health endpoint returned
	// 503
	StatusCritical
	// StatusUnknown indicates that the health endpoint returned
	// 500 or another unrecognized status code.
	StatusUnknown
	// StatusDown indicates that the habitat supervisor doesn't
	// know about this service and it is likely not started.
	StatusDown
)

var (
	ErrServiceNotFound = errors.New("service not found")
)

// ServiceHealth returns the health of a service according to the
// Habitat supervisor.
//
// Uses the /service/SERVICE_NAME/SERVICE_GROUP/health endpoint.
func (c *Client) ServiceHealth(ctx context.Context, serviceName string, serviceGroup string) (ServiceStatus, error) {
	path := fmt.Sprintf("/services/%s/%s/health", serviceName, serviceGroup)
	response, err := c.apiGet(ctx, path)
	if err != nil {
		return StatusUnknown, err
	}

	defer response.Body.Close()

	switch response.StatusCode {
	case http.StatusOK:
		return StatusOk, nil
	case http.StatusServiceUnavailable:
		return StatusCritical, nil
	case http.StatusInternalServerError:
		return StatusUnknown, nil
	case http.StatusNotFound:
		return StatusDown, nil
	default:
		return StatusUnknown, nil
	}
}

// ServiceInfo returns information about a service running under the Habitat supervisor.
//
// Uses the /service/SERVICE_NAME/SERVICE_GROUP endpoint
//
func (c *Client) ServiceInfo(ctx context.Context, serviceName string, serviceGroup string) (ServiceInfo, error) {
	var info ServiceInfo
	path := fmt.Sprintf("/services/%s/%s", serviceName, serviceGroup)
	response, err := c.apiGet(ctx, path)
	if err != nil {
		return info, err
	}

	defer response.Body.Close()

	switch response.StatusCode {
	case 404:
		return info, ErrServiceNotFound
	case 200:
		// Don't log in the success case
	default:
		logrus.WithField("status_code", response.StatusCode).Debug("Unexpected response from Habitat ServiceInfo API call")
	}

	err = json.NewDecoder(response.Body).Decode(&info)
	return info, err
}

// ListServices returns a list of services currently running under the Habitat supervisor
func (c *Client) ListServices(ctx context.Context) ([]ServiceInfo, error) {
	services := []ServiceInfo{}
	path := "/services"

	response, err := c.apiGet(ctx, path)

	if response != nil {
		defer response.Body.Close()
	}

	if err != nil {
		return nil, err
	}

	err = json.NewDecoder(response.Body).Decode(&services)

	return services, err
}

func mustParse(uri string) *url.URL {
	u, err := url.Parse(uri)
	if err != nil {
		logrus.WithError(err).Fatal("failed to initialize url")
	}
	return u
}

// New returns a Client with all passed options applied to it.
func New(baseURI string, options ...func(*Client)) *Client {
	c := &Client{}
	c.BaseURI = mustParse(baseURI)

	c.HTTPClient = http.Client{}

	for _, option := range options {
		option(c)
	}

	return c
}

// Timeout returns an "option func" suitable for passing to New which
// will set to Client's timeout to the specified duration.
func Timeout(timeout time.Duration) func(*Client) {
	return func(c *Client) {
		c.HTTPClient.Timeout = timeout
	}
}

func (c *Client) apiGet(ctx context.Context, p string) (*http.Response, error) {
	u := *c.BaseURI
	u.Path = path.Join(u.Path, p)
	req, err := http.NewRequest("GET", u.String(), nil)
	if err != nil {
		return nil, err
	}

	req = req.WithContext(ctx)
	return c.HTTPClient.Do(req)
}
