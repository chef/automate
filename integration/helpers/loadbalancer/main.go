package main

import (
	"crypto/tls"
	"fmt"
	"math/rand"
	"net"
	"net/http"
	"net/http/httputil"
	"net/url"
	"os"
	"strings"

	"github.com/chef/automate/lib/tls/certs"

	"github.com/chef/toml"

	"github.com/sirupsen/logrus"
)

type Config struct {
	Global struct {
		V1 struct {
			FrontendTLS []struct {
				Cert string `toml:"cert"`
				Key  string `toml:"key"`
			} `toml:"frontend_tls"`
		} `toml:"v1"`
	} `toml:"global"`
}

func main() {
	if len(os.Args) < 2 {
		fmt.Println("proxy a2config.toml URL...")
		os.Exit(1)
	}
	config := Config{}
	_, err := toml.DecodeFile(os.Args[1], &config)
	if err != nil {
		logrus.WithError(err).Fatal("Could not read config file")
	}
	if len(config.Global.V1.FrontendTLS) == 0 {
		logrus.Fatal("missing certs")
	}
	if len(config.Global.V1.FrontendTLS[0].Cert) == 0 {
		logrus.Fatal("cert missing")
	}
	if len(config.Global.V1.FrontendTLS[0].Key) == 0 {
		logrus.Fatal("key missing")
	}

	logrus.Info("starting jaym's proxy on port 443")
	urls := []*url.URL{}
	for _, urlStr := range os.Args[2:] {
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
	svcCerts, err := certs.ServiceCertsFromBytes(
		[]byte(config.Global.V1.FrontendTLS[0].Cert),
		[]byte(config.Global.V1.FrontendTLS[0].Key),
		[]byte(config.Global.V1.FrontendTLS[0].Cert),
	)
	if err != nil {
		logrus.Fatal(err)
	}

	addr := "0.0.0.0:443"
	server := &http.Server{Addr: addr, Handler: proxy}
	tlsConfig := &tls.Config{
		Certificates: []tls.Certificate{*svcCerts.ServiceKeyPair},
		NextProtos:   []string{"http/1.1"},
	}
	ln, err := net.Listen("tcp", addr)
	if err != nil {
		logrus.Fatal(err)
	}

	tlsListener := tls.NewListener(ln, tlsConfig)

	if err := server.Serve(tlsListener); err != nil {
		logrus.Fatalf("listen: %s", err.Error())
	}
}

func newReverseProxy(urls []*url.URL) *httputil.ReverseProxy {
	director := func(req *http.Request) {
		v := rand.Intn(len(urls))
		u := urls[v%len(urls)]
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
