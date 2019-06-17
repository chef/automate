package cs_nginx

import (
	"crypto/md5"
	"encoding/base64"
	fmt "fmt"
	"io/ioutil"
	"net/url"

	"github.com/sirupsen/logrus"

	"github.com/pkg/errors"

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
				RequiredRecipe: &ConfigRequest_V1_System_RequiredRecipe{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()

	c.V1.Sys.Service.Port = w.Int32(10200)
	c.V1.Sys.Service.Host = w.String("0.0.0.0")
	c.V1.Sys.Service.StatusPort = w.Int32(10204)

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

	c.V1.Sys.Ngx.Http.SslProtocols = w.String("TLSv1.2")
	c.V1.Sys.Ngx.Http.SslCiphers = w.String(ac.InternalCipherSuite)
	c.V1.Sys.Ngx.Http.SslVerifyDepth = w.Int32(2)

	c.V1.Sys.RequiredRecipe.Enabled = w.Bool(false)

	return c
}

// Validate validates that the config is sufficient to start the Service. If
// validation succeeds it will return nil, if it fails it will return a new
// instance of config.InvalidConfigError that has the missing keys and invalid
// fields populated.
func (c *ConfigRequest) Validate() error {
	cfgErr := ac.NewInvalidConfigError()
	rrEnabled := c.GetV1().GetSys().GetRequiredRecipe().GetEnabled().GetValue()
	if rrEnabled {
		// NOTE(ssd) 2018-08-02: We don't check if the file
		// actually exists here only if it has been
		// set. Practically, we could check for existence, but
		// technically, this function runs both client-side
		// and server-side.
		if c.GetV1().GetSys().GetRequiredRecipe().GetPath().GetValue() == "" {
			cfgErr.AddMissingKey("cs_nginx.v1.sys.required_recipe.path")
		}
	}

	logLevel := c.GetV1().GetSys().GetLog().GetLevel().GetValue()
	if logLevel != "" {
		err := ac.ValidateNginxLogLevel(logLevel)
		if err != nil {
			cfgErr.AddInvalidValue("cs_nginx.v1.sys.log.level", err.Error())
		}
	}

	if cfgErr.IsEmpty() {
		return nil
	}
	return cfgErr
}

// SetGlobalConfig is a callback that allows us to populate the configuration
// of an individual service with global automate config.
func (c *ConfigRequest) SetGlobalConfig(g *ac.GlobalConfig) {
	c.V1.Sys.Mlsa = g.V1.Mlsa

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level.Value = ac.GlobalLogLevelToNginxLevel(logLevel)
	}

	if gExternalAutomate := g.GetV1().GetExternal().GetAutomate(); gExternalAutomate.GetEnable().GetValue() {
		externalAutoamteURL, err := url.Parse(gExternalAutomate.GetNode().GetValue())
		if err != nil {
			logrus.WithError(err).Error("failed to parse external automate url")
			return
		}
		host := externalAutoamteURL.Host
		port := externalAutoamteURL.Port()
		scheme := externalAutoamteURL.Scheme
		if scheme == "" {
			scheme = "https"
		}
		if port == "" {
			switch scheme {
			case "https":
				port = "443"
			case "http":
				port = "80"
			}
		}

		endpoint := fmt.Sprintf("%s:%s", host, port)

		c.V1.Sys.ExternalAutomate = &ConfigRequest_V1_System_ExternalAutomate{
			Enable:      w.Bool(true),
			SslUpstream: w.Bool(scheme == "https"),
			Endpoint:    w.String(endpoint),
			RootCert:    gExternalAutomate.GetSsl().GetRootCert(),
			ServerName:  gExternalAutomate.GetSsl().GetServerName(),
			Token:       gExternalAutomate.GetAuth().GetToken(),
		}
	}
}

// PrepareSystemConfig returns a system configuration that can be used
// to start the service.
func (c *ConfigRequest) PrepareSystemConfig(creds *ac.TLSCredentials) (ac.PreparedSystemConfig, error) {
	c.V1.Sys.Tls = creds

	if c.V1.Sys.GetRequiredRecipe().GetEnabled().GetValue() {
		rrPath := c.V1.Sys.RequiredRecipe.Path.GetValue()
		rrContent, err := ioutil.ReadFile(rrPath)
		if err != nil {
			return nil, errors.Wrapf(err, "could not read required recipe at %q", rrPath)
		}
		c.V1.Sys.RequiredRecipe.ContentMd5 = w.String(CalculateContentMD5(rrContent))
	}

	return c.V1.Sys, nil
}

// CalculateContentMD5 returns a base-64 encoded MD5 sum of the given
// bytes.
//
// To ensure compatibility with the upstream chef-server this should
// be the same as the value returned by Ruby's
//
//  Digest::MD5.base64digest
//
func CalculateContentMD5(data []byte) string {
	md5sum := md5.Sum(data)
	return base64.StdEncoding.EncodeToString(md5sum[:])
}
