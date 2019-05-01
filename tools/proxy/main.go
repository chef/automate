package main

import (
	"crypto/tls"
	"fmt"
	"net/http"
	"net/http/httputil"
	"net/url"
	"os"
	"strings"
	"sync/atomic"

	"github.com/sirupsen/logrus"
)

func main() {
	if len(os.Args) < 3 {
		fmt.Println("proxy CERT KEY URL...")
		os.Exit(1)
	}
	certFile := os.Args[1]
	keyFile := os.Args[2]
	logrus.Info("starting jaym's proxy on port 443")
	urls := []*url.URL{}
	for _, urlStr := range os.Args[3:] {
		if !strings.HasPrefix(urlStr, "https://") && !strings.HasPrefix(urlStr, "http://") {
			urlStr = fmt.Sprintf("https://%s", urlStr)
		}
		u, err := url.Parse(urlStr)
		if err != nil {
			logrus.Fatalf("could not parse given upstream url (%s): %s", urlStr, err.Error())
		}
		logrus.Infof("upstream %s added", u)
		urls = append(urls, u)
	}
	proxy := newReverseProxy(urls)
	err := http.ListenAndServeTLS("0.0.0.0:443", certFile, keyFile, proxy)
	if err != nil {
		logrus.Fatalf("listen: %s", err.Error())
	}
}

func newReverseProxy(urls []*url.URL) *httputil.ReverseProxy {
	var counter uint64
	director := func(req *http.Request) {
		v := atomic.AddUint64(&counter, 1)
		u := urls[int(v)%len(urls)]
		req.URL.Scheme = u.Scheme
		req.URL.Host = u.Host
		logrus.Infof("%s %s upstream: %s", req.Method, req.URL.Path, req.URL.String())
	}

	return &httputil.ReverseProxy{
		Director: director,
		Transport: &http.Transport{
			TLSClientConfig: &tls.Config{
				InsecureSkipVerify: true,
			},
		},
	}
}
