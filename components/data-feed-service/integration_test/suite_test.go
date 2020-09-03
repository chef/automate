package integration_test

import (
	"context"
	"log"
	"net/http"
)

type Suite struct {
	ctx    context.Context
	server http.Server
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
	return suite
}

// GlobalSetup makes backend connections to elastic and postgres. It also sets
// global vars to usable values.
func (s *Suite) GlobalSetup() error {

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
	s.server.Shutdown(s.ctx)
}
