package gateway

import (
	"net/http/httptest"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc/metadata"
)

func TestMetadataFromRequest(t *testing.T) {
	cases := map[string]struct {
		headers  map[string][]string
		metadata metadata.MD
	}{
		"no headers": {
			map[string][]string{},
			metadata.Pairs([]string{}...),
		},
		"authorization header": {
			map[string][]string{"Authorization": {"bearer TOKEN"}},
			metadata.Pairs("grpcgateway-authorization", "bearer TOKEN"),
		},
		"two authorization header": {
			map[string][]string{"Authorization": {"bearer TOKEN", "bearer TOKEN2"}},
			metadata.Pairs(
				"grpcgateway-authorization", "bearer TOKEN",
				"grpcgateway-authorization", "bearer TOKEN2",
			),
		},
		"x-data-collector-token header": {
			map[string][]string{"x-data-collector-token": {"DCTOKEN"}},
			metadata.Pairs("grpcgateway-x-data-collector-token", "DCTOKEN"),
		},
		"api-token header": {
			map[string][]string{"api-token": {"DCTOKEN"}},
			metadata.Pairs("grpcgateway-api-token", "DCTOKEN"),
		},
		"projects header": {
			map[string][]string{"projects": {"these,are,some,projects"}},
			metadata.Pairs("grpcgateway-projects", "these,are,some,projects"),
		},
		"x-tls-client-subject-dn header": {
			map[string][]string{"x-client-cert": {"URLENCODEDCERT"}},
			metadata.Pairs("grpcgateway-x-client-cert", "URLENCODEDCERT"),
		},
		"some standard header (content-type)": {
			map[string][]string{"content-type": {"application/banana"}},
			metadata.Pairs("grpcgateway-content-type", "application/banana"),
		},
	}

	for desc, tc := range cases {
		t.Run(desc, func(t *testing.T) {
			req := httptest.NewRequest("GET", "/foo", nil)
			req.Header = tc.headers
			actual := metadataFromRequest(req)
			assert.Equal(t, tc.metadata, actual)
		})
	}
}

func TestHeaderMatcherFunc(t *testing.T) {
	t.Parallel()
	t.Run("authorization", matchPrefix("grpcgateway-Authorization"))
	t.Run("Authorization", matchPrefix("grpcgateway-Authorization"))
	t.Run("x-data-collector-token", matchPrefix("grpcgateway-x-data-collector-token"))
	t.Run("something-else", func(t *testing.T) {
		input := "something-else"
		actual, ok := headerMatcher(input)
		if ok {
			t.Fatal("expected NOT ok")
		}
		if actual != "" {
			t.Fatalf("expected \"\", got %v", actual)
		}
	})
}

func matchPrefix(expected string) func(*testing.T) {
	return func(t *testing.T) {
		t.Helper()
		input := strings.Split(t.Name(), "/")[1]
		actual, ok := headerMatcher(input)
		if !ok {
			t.Fatal("expected ok")
		}
		if actual != expected {
			t.Fatalf("expected %v, got %v", expected, actual)
		}
	}
}
