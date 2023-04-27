package mockserver

import (
	"fmt"
	"net/http"
	"net/http/httptest"
	"testing"
	"time"
)

type APIBuilder struct {
	mux *http.ServeMux
	t   *testing.T

	server *httptest.Server
}

// NewAPI creates new APIBuilder
func NewAPI(t *testing.T) *APIBuilder {
	result := &APIBuilder{
		mux: http.NewServeMux(),
		t:   t,
	}
	// fallback to show not implemented routes
	result.mux.Handle("/", http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(404)
		fmt.Fprintf(w, `{ "error": "Not supported route %q"}`, r.URL.Path)
		result.t.Errorf("Not Supported route %q", r.URL.Path)
	}))
	return result
}

func (b *APIBuilder) WithHandler(pattern string, handler http.HandlerFunc) *APIBuilder {
	b.mux.HandleFunc(pattern, handler)
	return b
}

// Build returns new http.Server hosting API mocks
func (b *APIBuilder) Build() *httptest.Server {
	b.server = httptest.NewServer(b.mux)
	return b.server
}

func (b *APIBuilder) HttpReqMock(endpoint string, code int, response []byte, wait time.Duration) *APIBuilder {
	b.mux.Handle(endpoint,
		http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			time.Sleep(wait)
			w.WriteHeader(code)
			w.Header().Set("Content-Type", "application/json")
			w.Write(response)
		}))
	return b
}
