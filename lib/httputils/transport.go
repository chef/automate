package httputils

import (
	"net"
	"net/http"
	"time"
)

// NewDefaultTransport constructs an http.Transport with settings that
// should match http.DefaultTransport. Use this when you want to
// customize your transport options without modifying the global
// http.DefaultTransport.
//
// This is copied from the golang source source:
//
// https://github.com/golang/go/blob/5fae09b7386de26db59a1184f62fc7b22ec7667b/src/net/http/transport.go#L42-L53
//
// See also: https://github.com/golang/go/issues/26013
func NewDefaultTransport() *http.Transport {
	return &http.Transport{
		DialContext: (&net.Dialer{
			Timeout:   30 * time.Second,
			KeepAlive: 30 * time.Second,
			DualStack: true,
		}).DialContext,
		MaxIdleConns:          100,
		IdleConnTimeout:       90 * time.Second,
		TLSHandshakeTimeout:   10 * time.Second,
		ExpectContinueTimeout: 1 * time.Second,
	}
}
