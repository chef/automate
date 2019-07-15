package esgateway

import (
	"encoding/base64"
	"fmt"
	"strings"

	wrappers "github.com/golang/protobuf/ptypes/wrappers"

	ac "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
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
					Main:   &ConfigRequest_V1_System_Nginx_Main{},
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

	c.V1.Sys.Service.Port = w.Int32(10144)
	c.V1.Sys.Service.Host = w.String("0.0.0.0")

	c.V1.Sys.Log.Level = w.String("error")

	c.V1.Sys.Ngx.Main.WorkerProcesses = w.Int32(4)

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
		if len(nodes) > 0 {
			isSSL := false

			endpoints := make([]*wrappers.StringValue, 0, len(nodes))
			for _, n := range nodes {
				endpoint, ssl := uriToEndpoint(n.GetValue())
				endpoints = append(endpoints, w.String(endpoint))
				isSSL = ssl
			}
			c.V1.Sys.External.SslUpstream = w.Bool(isSSL)
			c.V1.Sys.External.Endpoints = endpoints
			if isSSL {
				if serverName := externalES.GetSsl().GetServerName().GetValue(); serverName != "" {
					c.V1.Sys.External.ServerName = w.String(serverName)
				} else {
					endpoint := endpoints[0].GetValue()
					c.V1.Sys.External.ServerName = w.String(strings.Split(endpoint, ":")[0])
				}
			}
		}

		if auth := g.GetV1().GetExternal().GetElasticsearch().GetAuth(); auth.GetScheme().GetValue() == "basic_auth" {
			c.V1.Sys.External.BasicAuthCredentials = w.String(base64.StdEncoding.EncodeToString([]byte(
				fmt.Sprintf(
					"%s:%s",
					auth.GetBasicAuth().GetUsername().GetValue(),
					auth.GetBasicAuth().GetPassword().GetValue(),
				),
			)))
		}

		c.V1.Sys.External.RootCert = g.GetV1().GetExternal().GetElasticsearch().GetSsl().GetRootCert()
		c.V1.Sys.External.RootCertFile = g.GetV1().GetExternal().GetElasticsearch().GetSsl().GetRootCertFile()
	}

}

func uriToEndpoint(uri string) (endpoint string, ssl bool) {
	if strings.HasPrefix(uri, "http://") {
		return strings.TrimPrefix(uri, "http://"), false
	} else if strings.HasPrefix(uri, "https://") {
		return strings.TrimPrefix(uri, "https://"), true
	}
	return uri, false
}
