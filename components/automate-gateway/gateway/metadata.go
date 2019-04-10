package gateway

import (
	"net/http"
	"net/textproto"

	"github.com/grpc-ecosystem/grpc-gateway/runtime"
	"google.golang.org/grpc/metadata"
)

// headerMatcher extends runtime.DefaultHeaderMatcher by also injecting headers
// that are not-standard, but in use in Automate.
func headerMatcher(key string) (string, bool) {
	// default behavior
	if key, ok := runtime.DefaultHeaderMatcher(key); ok {
		return key, ok
	}
	if isAutomateHeader(key) {
		return runtime.MetadataPrefix + key, true
	}

	return "", false
}

func isAutomateHeader(hdr string) bool {
	switch textproto.CanonicalMIMEHeaderKey(hdr) {
	case "X-Data-Collector-Token", "Api-Token", "X-Client-Cert", "Projects":
		return true
	}
	return false
}

// metadataFromRequest constructs a metadata object to be passed into the
// context of a grpc call (via `metadata.NewOutgoingContext`). It mimics
// what grpc-gateway does and is only used for the custom, handcrafted
// endpoints defined in `gateway/services.go`.
func metadataFromRequest(r *http.Request) metadata.MD {
	var pairs []string
	for key, vals := range r.Header {
		for _, val := range vals {
			if h, ok := headerMatcher(key); ok {
				pairs = append(pairs, h, val)
			}
		}
	}
	return metadata.Pairs(pairs...)
}
