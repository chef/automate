package httputils

import (
	"net/http"
	"strconv"
	"strings"
	"time"
)

//Add HSTS header to response
//See: https://github.com/chef/automate/issues/5698
type HSTS struct {
	next http.Handler
	// MaxAge sets the duration that the HSTS is valid for.
	MaxAge time.Duration
	// HostOverride provides a host to the redirection URL in the case that the system is behind a load balancer
	// which doesn't provide the X-Forwarded-Host HTTP header (e.g. an Amazon ELB).
	HostOverride string
	// Decides whether to accept the X-Forwarded-Proto header as proof of SSL.
	AcceptXForwardedProtoHeader bool
	// SendPreloadDirective sets whether the preload directive should be set. The directive allows browsers to
	// confirm that the site should be added to a preload list. (see https://hstspreload.appspot.com/)
	SendPreloadDirective bool
}

func isHTTPS(r *http.Request, acceptXForwardedProtoHeader bool) bool {
	// Added by common load balancers which do TLS offloading.
	if acceptXForwardedProtoHeader && r.Header.Get("X-Forwarded-Proto") == "https" {
		return true
	}
	// If the X-Forwarded-Proto was set upstream as HTTP, then the request came in without TLS.
	if acceptXForwardedProtoHeader && r.Header.Get("X-Forwarded-Proto") == "http" {
		return false
	}
	// Set by some middleware.
	if r.URL.Scheme == "https" {
		return true
	}
	// Set when the Go server is running HTTPS itself.
	if r.TLS != nil && r.TLS.HandshakeComplete {
		return true
	}
	return false
}

func createHeaderValue(maxAge time.Duration, sendPreloadDirective bool) string {
	builder := strings.Builder{}
	builder.WriteString("max-age=")
	builder.WriteString(strconv.Itoa(int(maxAge.Seconds())))
	builder.WriteString("; includeSubDomains")
	if sendPreloadDirective {
		builder.WriteString("; preload")
	}
	return builder.String()
}

func HSTSHandler(next http.Handler) *HSTS {
	return &HSTS{
		next:                        next,
		MaxAge:                      time.Hour * 24 * 730, //2 years; ie. 730 days; ie. 63072000 seconds
		AcceptXForwardedProtoHeader: true,
		SendPreloadDirective:        false,
	}
}

//Add HSTS response header for https calls and redirect if call is over http
func (h *HSTS) addHSTSHeaderAndRedirect(w http.ResponseWriter, r *http.Request) {
	if isHTTPS(r, h.AcceptXForwardedProtoHeader) {
		w.Header().Add("Strict-Transport-Security", createHeaderValue(h.MaxAge, h.SendPreloadDirective))

		h.next.ServeHTTP(w, r)
	} else {
		if h.HostOverride != "" {
			r.URL.Host = h.HostOverride
		} else if !r.URL.IsAbs() {
			r.URL.Host = r.Host
		}

		r.URL.Scheme = "https"

		http.Redirect(w, r, r.URL.String(), http.StatusMovedPermanently)
	}
}

//Only add HSTS response header for https calls and doesn't redirect if call is over http
func (h *HSTS) addHSTSHeader(w http.ResponseWriter, r *http.Request) {
	w.Header().Add("Strict-Transport-Security", createHeaderValue(h.MaxAge, h.SendPreloadDirective))
	h.next.ServeHTTP(w, r)
}

func (h *HSTS) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	h.addHSTSHeader(w, r)
}

//HSTS middleware
func HSTSMiddleware(next http.Handler) http.Handler {
	h := HSTSHandler(next)
	return http.HandlerFunc(h.ServeHTTP)
}
