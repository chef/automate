package esgateway

import (
	"encoding/base64"
	"fmt"
	"net"
	"net/url"
	"strings"

	wrappers "github.com/golang/protobuf/ptypes/wrappers"

	ac "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/chef/automate/lib/config"
)

// NewConfigRequest returns a new instance of ConfigRequest with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Mlsa:    &ac.Mlsa{},
				Service: &ConfigRequest_V1_System_Service{},
				Log:     &ConfigRequest_V1_System_Log{},
				Ngx: &ConfigRequest_V1_System_Nginx{
					Main: &ConfigRequest_V1_System_Nginx_Main{
						Resolvers: &ConfigRequest_V1_System_Nginx_Main_Resolvers{},
					},
					Events: &ConfigRequest_V1_System_Nginx_Events{},
					Http:   &ConfigRequest_V1_System_Nginx_Http{},
				},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()

	c.V1.Sys.Service.Host = w.String("127.0.0.1")
	c.V1.Sys.Service.Port = w.Int32(10144)

	c.V1.Sys.Log.Level = w.String("error")

	c.V1.Sys.Ngx.Main.WorkerProcesses = w.Int32(4)
	c.V1.Sys.Ngx.Main.MaxFails = w.Int32(10)
	// TODO(ssd) 2020-12-08: Enable automatic discovery of system
	// resolvers once we are a bit more confident in the nginx
	// module we are using to do dynamic resolution.
	//

	c.V1.Sys.Ngx.Main.Resolvers.EnableSystemNameservers = w.Bool(false)
	c.V1.Sys.Ngx.Events.WorkerConnections = w.Int32(1024)

	c.V1.Sys.Ngx.Http.ServerNamesHashBucketSize = w.Int32(128)
	c.V1.Sys.Ngx.Http.ClientMaxBodySize = w.String("250m")
	c.V1.Sys.Ngx.Http.ClientBodyBufferSize = w.String("128k")
	c.V1.Sys.Ngx.Http.ProxyConnectTimeout = w.Int32(1)
	c.V1.Sys.Ngx.Http.KeepaliveTimeout = w.Int32(64)
	c.V1.Sys.Ngx.Http.Gzip = w.String("on")
	c.V1.Sys.Ngx.Http.GzipHttpVersion = w.String("1.0")
	c.V1.Sys.Ngx.Http.GzipCompLevel = w.String("2")
	c.V1.Sys.Ngx.Http.GzipProxied = w.String("any")
	c.V1.Sys.Ngx.Http.GzipTypes = w.String("text/plain text/css application/x-javascript text/xml application/xml application/xml+rss text/javascript application/json") //nolint: lll

	c.V1.Sys.Ngx.Http.Sendfile = w.String("on")
	c.V1.Sys.Ngx.Http.TcpNodelay = w.String("on")
	c.V1.Sys.Ngx.Http.TcpNopush = w.String("on")

	c.V1.Sys.Ngx.Http.SslProtocols = w.String("TLSv1.2 TLSv1.3")
	c.V1.Sys.Ngx.Http.SslCiphers = w.String(ac.InternalCipherSuite)
	c.V1.Sys.Ngx.Http.SslVerifyDepth = w.Int32(2)
	c.V1.Sys.Ngx.Http.ProxyReadTimeout = w.Int32(7200)
	c.V1.Sys.Ngx.Http.ProxySendTimeout = w.Int32(600)
	c.V1.Sys.Ngx.Http.ProxySetHeaderHost = w.String("$http_host")

	return c
}

// Validate validates that the config is sufficient to start the Service. If
// validation succeeds it will return nil, if it fails it will return a new
// instance of config.InvalidConfigError that has the missing keys and invalid
// fields populated.
func (c *ConfigRequest) Validate() error {
	return nil
}

// PrepareSystemConfig returns a system configuration that can be used
// to start the service.
func (c *ConfigRequest) PrepareSystemConfig(creds *ac.TLSCredentials) (ac.PreparedSystemConfig, error) {
	c.V1.Sys.Tls = creds

	enableSystemNameServer := c.GetV1().GetSys().GetNgx().GetMain().GetResolvers().GetEnableSystemNameservers().GetValue()
	nameServers := c.GetV1().GetSys().GetNgx().GetMain().GetResolvers().GetNameservers()
	if len(nameServers) == 0 && enableSystemNameServer {
		c.V1.Sys.Ngx.Main.Resolvers.NameserversString = getSystemResolvers()
	} else {
		if len(nameServers) > 0 {
			ns := make([]string, 0, len(nameServers))
			for _, n := range nameServers {
				if n != nil {
					ns = append(ns, n.GetValue())
				}
			}
			c.V1.Sys.Ngx.Main.Resolvers.NameserversString = w.String(strings.Join(ns, " "))
		}
	}
	return c.V1.Sys, nil
}

func (c *ConfigRequest) SetGlobalConfig(g *ac.GlobalConfig) {
	c.V1.Sys.Mlsa = g.V1.Mlsa

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level.Value = ac.GlobalLogLevelToNginxLevel(logLevel)
	}

	if externalES := g.GetV1().GetExternal().GetElasticsearch(); externalES.GetEnable().GetValue() {
		nodes := externalES.GetNodes()
		c.V1.Sys.External = &ConfigRequest_V1_System_External{
			Enable: w.Bool(true),
		}
		endpoints := make([]*ConfigRequest_V1_System_Endpoint, 0, len(nodes))
		if len(nodes) > 0 {
			isSSL := false

			for _, n := range nodes {
				endpoint, ssl := uriToEndpoint(n.GetValue())
				endpoints = append(endpoints, endpoint)
				isSSL = ssl
			}
			c.V1.Sys.External.SslUpstream = w.Bool(isSSL)
			c.V1.Sys.External.ParsedEndpoints = endpoints

			if isSSL {
				if serverName := externalES.GetSsl().GetServerName().GetValue(); serverName != "" {
					c.V1.Sys.External.ServerName = w.String(serverName)
				} else {
					endpoint := endpoints[0].Address.GetValue()
					c.V1.Sys.External.ServerName = w.String(strings.Split(endpoint, ":")[0])
				}
			}
		}

		switch auth := g.GetV1().GetExternal().GetElasticsearch().GetAuth(); auth.GetScheme().GetValue() {
		case "basic_auth":
			c.V1.Sys.External.BasicAuthCredentials = w.String(base64.StdEncoding.EncodeToString([]byte(
				fmt.Sprintf(
					"%s:%s",
					auth.GetBasicAuth().GetUsername().GetValue(),
					auth.GetBasicAuth().GetPassword().GetValue(),
				),
			)))
		case "aws_es":
			// If we only have 1 AWS Elasticsearch Service endpoint specified, we can assume that
			// the host header should be the name of that endpoint.
			if c.V1.Sys.Ngx.Http.ProxySetHeaderHost.Value == "$http_host" && len(endpoints) == 1 {
				c.V1.Sys.Ngx.Http.ProxySetHeaderHost = endpoints[0].Address
			}
			c.V1.Sys.External.BasicAuthCredentials = w.String(base64.StdEncoding.EncodeToString([]byte(
				fmt.Sprintf(
					"%s:%s",
					auth.GetAwsEs().GetUsername().GetValue(),
					auth.GetAwsEs().GetPassword().GetValue(),
				),
			)))
		default:
		}

		c.V1.Sys.External.RootCert = g.GetV1().GetExternal().GetElasticsearch().GetSsl().GetRootCert()
		c.V1.Sys.External.RootCertFile = g.GetV1().GetExternal().GetElasticsearch().GetSsl().GetRootCertFile()
	}
}

func getSystemResolvers() *wrappers.StringValue {
	ns := config.GetSystemResolvers()
	r := strings.Join(ns, " ")

	return w.String(r)
}

var (
	defaultHTTPPort  = w.String("80")
	defaultHTTPSPort = w.String("443")
)

func uriToEndpoint(uri string) (*ConfigRequest_V1_System_Endpoint, bool) {
	ssl := false
	ret := &ConfigRequest_V1_System_Endpoint{
		Address:  w.String(uri),
		IsDomain: w.Bool(false),
	}

	url, err := url.Parse(uri)
	if err != nil {
		ret.Port = defaultHTTPPort
		return ret, ssl
	}

	host, port, err := net.SplitHostPort(url.Host)
	if err == nil {
		ret.Port = w.String(port)
		ret.Address = w.String(host)
	} else {
		ret.Address = w.String(url.Host)
	}

	ret.IsDomain = w.Bool(!isIPAddress(ret.Address.GetValue()))

	if url.Scheme == "https" {
		ssl = true
		if ret.Port == nil {
			ret.Port = defaultHTTPSPort
		}
	} else {
		if ret.Port == nil {
			ret.Port = defaultHTTPPort
		}
	}

	return ret, ssl
}

func isIPAddress(addr string) bool {
	return net.ParseIP(addr) != nil
}
