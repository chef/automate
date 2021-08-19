package integration_test

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"testing"
)

type Suite struct {
	ctx           context.Context
	server        http.Server
	mux           *http.ServeMux
	destinationId string
	secretId      string
}

// Just returns a new struct. You have to call GlobalSetup() to setup the
// backend connections and such.
func NewSuite() *Suite {
	suite := new(Suite)
	mux := http.NewServeMux()
	server := http.Server{Addr: ":38080", Handler: mux}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	mux.HandleFunc("/success", func(w http.ResponseWriter, r *http.Request) {
		w.Write([]byte("OK"))
	})
	mux.Handle("/fails", http.NotFoundHandler())
	suite.ctx = ctx
	suite.server = server
	suite.mux = mux
	return suite
}

// GlobalSetup makes backend connections to elastic and postgres. It also sets
// global vars to usable values.
func (s *Suite) GlobalSetup() error {
	var err error
	suite.secretId, err = addSecretRequest(secretData, 200)
	if err != nil {
		log.Fatal(err)
	}
	destination := []byte(`{"url":"http://localhost:38080/asset", "name":"integration_test_suite", "secret":"` + suite.secretId + `", "services":"Service Now", "integration_types": "Webhook"}`)
	response, err := addDestinationRequest(destination, 200)
	if err != nil {
		log.Fatal(err)
	}
	body, err := parseHttpBody(response.Body)
	if err != nil {
		log.Fatal(err)
	}
	suite.destinationId = body["id"].(string)
	go func() {
		if err := s.server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
			log.Fatal(err)
		}
	}()
	return nil
}

// GlobalTeardown is the place where you tear everything down after we have finished
// executing all our test suite, at the moment we are just deleting ES Indices
func (s *Suite) GlobalTeardown() {
	deleteDestinationRequest(suite.destinationId, 200)
	deleteSecretRequest(suite.secretId, 200)
	s.server.Shutdown(s.ctx)
}

func parseHttpBody(body io.ReadCloser) (map[string]interface{}, error) {
	m := make(map[string]interface{})
	err := json.NewDecoder(body).Decode(&m)
	if err != nil {
		return nil, err
	}
	return m, err
}

type AssetHandler struct {
	t *testing.T
}

func (suite Suite) registerAssetHandler(t *testing.T) {
	suite.mux.Handle("/asset", &AssetHandler{t: t})
}

func testStringValue(body map[string]interface{}, key string, expected string) (bool, string) {
	if value, ok := body[key]; ok {
		if value == expected {
			return true, ""
		} else {
			return false, fmt.Sprintf("Expected %s for key: %s, got %s", expected, key, value)
		}
	} else {
		return false, fmt.Sprintf("Key not found: %s", key)
	}
}
