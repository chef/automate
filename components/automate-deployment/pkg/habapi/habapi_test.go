package habapi

import (
	"context"
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

func TestListServicesReturnsParsedData(t *testing.T) {
	responseData := `[{"binds": ["foo"], "pkg": { "name": "acme", "origin": "core", "release": "20170101010101", "version": "1.2.3", "channel": "dev"}},{"binds": ["foo"], "pkg": { "name": "acme2", "origin": "core", "release": "20170101010101", "version": "1.2.3", "channel": "dev"}}]`

	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintln(w, responseData)
	}))
	defer ts.Close()

	c := New(ts.URL)
	response, err := c.ListServices(context.TODO())
	assert.NoError(t, err)
	assert.Equal(t, []ServiceInfo{
		{
			Binds: []string{"foo"},
			Pkg: ServicePkg{
				Origin:  "core",
				Name:    "acme",
				Version: "1.2.3",
				Release: "20170101010101",
				Channel: "dev",
			},
		},
		{
			Binds: []string{"foo"},
			Pkg: ServicePkg{
				Origin:  "core",
				Name:    "acme2",
				Version: "1.2.3",
				Release: "20170101010101",
				Channel: "dev",
			},
		},
	}, response)

}

func TestServiceInfoReturnsParsedData(t *testing.T) {
	responseData := `{"binds": ["foo"], "pkg": { "name": "acme", "origin": "core", "release": "20170101010101", "version": "1.2.3", "channel": "dev"}}`

	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintln(w, responseData)
	}))
	defer ts.Close()

	c := New(ts.URL)
	response, err := c.ServiceInfo(context.TODO(), "some-service", "default")
	assert.NoError(t, err)
	assert.Equal(t, ServiceInfo{
		Binds: []string{"foo"},
		Pkg: ServicePkg{
			Origin:  "core",
			Name:    "acme",
			Version: "1.2.3",
			Release: "20170101010101",
			Channel: "dev",
		},
	}, response)

}

func TestServiceHealthReturnsStatusOkOnOk(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		assert.Equal(t, "/services/some-service/default/health", r.URL.Path)
	}))
	defer ts.Close()

	c := New(ts.URL)
	response, err := c.ServiceHealth(context.TODO(), "some-service", "default")
	assert.NoError(t, err)
	assert.Equal(t, StatusOk, response)
}

func TestServiceHealthReturnsDownOnNotFound(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		assert.Equal(t, "/services/some-service/default/health", r.URL.Path)
		w.WriteHeader(404)
	}))
	defer ts.Close()
	c := New(ts.URL)
	response, err := c.ServiceHealth(context.TODO(), "some-service", "default")
	assert.NoError(t, err)
	assert.Equal(t, StatusDown, response)
}

func TestServiceHealthReturnsUnknownOnInternalServiceError(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		assert.Equal(t, "/services/some-service/default/health", r.URL.Path)
		w.WriteHeader(http.StatusInternalServerError)
	}))
	defer ts.Close()

	c := New(ts.URL)
	response, err := c.ServiceHealth(context.TODO(), "some-service", "default")
	assert.NoError(t, err)
	assert.Equal(t, StatusUnknown, response)
}

func TestServiceHealthReturnsCriticalOnServiceUnavailable(t *testing.T) {
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		assert.Equal(t, "/services/some-service/default/health", r.URL.Path)
		w.WriteHeader(http.StatusServiceUnavailable)
	}))
	defer ts.Close()

	c := New(ts.URL)
	response, err := c.ServiceHealth(context.TODO(), "some-service", "default")
	assert.NoError(t, err)
	assert.Equal(t, StatusCritical, response)
}

func TestTimeout(t *testing.T) {
	waitChan := make(chan struct{}, 1)
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		assert.Equal(t, "/services/some-service/default/health", r.URL.Path)
		<-waitChan
	}))
	defer ts.Close()

	c := New(ts.URL, Timeout(10*time.Millisecond))
	_, err := c.ServiceHealth(context.TODO(), "some-service", "default")
	assert.Error(t, err)
	waitChan <- struct{}{}
}

func TestContext(t *testing.T) {
	waitChan := make(chan struct{}, 1)
	ts := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		assert.Equal(t, "/services/some-service/default/health", r.URL.Path)
		<-waitChan
	}))
	defer ts.Close()

	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Millisecond)
	defer cancel()

	c := New(ts.URL)
	_, err := c.ServiceHealth(ctx, "some-service", "default")
	assert.Error(t, err)
	waitChan <- struct{}{}
}
