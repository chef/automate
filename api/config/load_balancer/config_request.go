package load_balancer

import (
	"net"
	"os"
	"sync"
	"syscall"

	"github.com/sirupsen/logrus"

	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

// NewConfigRequest returns a new ConfigRequests instance with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Mlsa:    &config.Mlsa{},
				Service: &ConfigRequest_V1_System_Service{},
				Log:     &ConfigRequest_V1_System_Logger{},
				Ngx:     NewNginxConfig(),
				Proxy:   &config.Proxy{},
				// TODO: do we need to reintroduce the empty ssl cert here?
				FrontendTls: []*config.FrontendTLSCredential{},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new ConfigRequest instance with default values.
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.HttpsPort = w.Int32(443)
	c.V1.Sys.Service.HttpPort = w.Int32(80)
	c.V1.Sys.Service.MaintenanceMode = w.Bool(false)
	c.V1.Sys.Log.Level = w.String("error")
	c.V1.Sys.Ngx.Main.WorkerProcesses = w.Int32(4)
	c.V1.Sys.Ngx.Main.ErrorLog = w.String("/dev/stderr")
	c.V1.Sys.Ngx.Events.WorkerConnections = w.Int32(1024)
	c.V1.Sys.Ngx.Events.WorkerProcessorMethod = w.String("epoll")
	c.V1.Sys.Ngx.Events.MultiAccept = w.String("on")
	c.V1.Sys.Ngx.Http.AccessLog = w.String("/dev/stdout")
	c.V1.Sys.Ngx.Http.DefaultType = w.String("application/octet-stream")
	c.V1.Sys.Ngx.Http.ClientBodyBufferSize = w.String("128k")
	c.V1.Sys.Ngx.Http.ClientMaxBodySize = w.String("250m")
	c.V1.Sys.Ngx.Http.KeepaliveTimeout = w.Int32(60)
	c.V1.Sys.Ngx.Http.KeepaliveRequests = w.Int32(10000)
	c.V1.Sys.Ngx.Http.Gzip = w.String("on")
	c.V1.Sys.Ngx.Http.GzipCompLevel = w.String("2")
	c.V1.Sys.Ngx.Http.GzipDisable = w.String("MSIE [1-6]\\.")
	c.V1.Sys.Ngx.Http.GzipHttpVersion = w.String("1.0")
	c.V1.Sys.Ngx.Http.GzipMinLength = w.Int32(10240)
	c.V1.Sys.Ngx.Http.GzipProxied = w.String("expired no-cache no-store private auth")
	c.V1.Sys.Ngx.Http.GzipTypes = w.String("text/plain text/css text/xml text/javascript application/x-javascript application/xml")
	c.V1.Sys.Ngx.Http.GzipVary = w.String("on")
	c.V1.Sys.Ngx.Http.LargeClientHeaderBuffersNumber = w.Int32(4)
	c.V1.Sys.Ngx.Http.LargeClientHeaderBuffersSize = w.String("8k")
	c.V1.Sys.Ngx.Http.Sendfile = w.String("on")
	c.V1.Sys.Ngx.Http.SslCiphers = w.String(config.ExternalCipherSuite)
	c.V1.Sys.Ngx.Http.SslProtocols = w.String("TLSv1.2")
	c.V1.Sys.Ngx.Http.TcpNodelay = w.String("on")
	c.V1.Sys.Ngx.Http.TcpNopush = w.String("on")
	c.V1.Sys.Ngx.Http.Ipv6Supported = w.Bool(ipV6Supported())

	return c
}

// NewNginxConfig returns a new ConfigRequest_V1_System_Nginx instance with
// zero values.
func NewNginxConfig() *ConfigRequest_V1_System_Nginx {
	return &ConfigRequest_V1_System_Nginx{
		Main:   &ConfigRequest_V1_System_Nginx_Main{},
		Events: &ConfigRequest_V1_System_Nginx_Events{},
		Http:   &ConfigRequest_V1_System_Nginx_Http{},
		Mail:   &ConfigRequest_V1_System_Nginx_Mail{},
	}
}

// ValidateConfigRequest validates that the config is sufficient to start the
// Service and returns true.
func (c *ConfigRequest) Validate() error {
	cfgErr := config.NewInvalidConfigError()

	if c.V1.Sys.Service.ExternalFqdn == nil {
		cfgErr.AddMissingKey("load_balancer.v1.sys.service.external_fqdn")
	}

	logLevel := c.GetV1().GetSys().GetLog().GetLevel().GetValue()
	if logLevel != "" {
		err := config.ValidateNginxLogLevel(logLevel)
		if err != nil {
			cfgErr.AddInvalidValue("load_balancer.v1.sys.log.level", err.Error())
		}
	}

	if len(c.V1.Sys.FrontendTls) < 1 {
		cfgErr.AddMissingKey("load_balancer.v1.sys.frontend_tls")
	} else {
		for _, tls := range c.V1.Sys.FrontendTls {
			if tls.Cert == "" {
				cfgErr.AddMissingKey("load_balancer.v1.sys.frontend_tls.cert")
			}
			if tls.Key == "" {
				cfgErr.AddMissingKey("load_balancer.v1.sys.frontend_tls.key")
			}
			if tls.ServerName == "" {
				cfgErr.AddInvalidValue("load_balancer.v1.sys.frontend_tls.server_name", "server_name must be a valid FQDN")
			}
		}
	}

	if cfgErr.IsEmpty() {
		return nil
	}
	return cfgErr
}

// SetGlobalConfig takes a pointer to a config.GlobalConfig and applies any
// global configuration that the service requires.
func (c *ConfigRequest) SetGlobalConfig(g *config.GlobalConfig) {
	c.V1.Sys.Mlsa = g.V1.Mlsa

	// If frontend TLS certs are set on global, they will take priority. This
	// might be a backwards but it's consistent with how we use global config
	// elsewhere.
	if g.GetV1().GetFrontendTls() != nil {
		if len(g.GetV1().GetFrontendTls()) > 0 {
			c.V1.Sys.FrontendTls = g.GetV1().GetFrontendTls()
		}
	}

	// Fixup any certs that don't have the FQDN. This has to come after we copy
	// the certs over from global.
	if g.V1.Fqdn != nil {
		c.V1.Sys.Service.ExternalFqdn = g.V1.Fqdn

		for _, cred := range c.V1.Sys.FrontendTls {
			// Override any missing or default server_name's with our global FQDN.
			if cred.ServerName == "localhost" || cred.ServerName == "" {
				cred.ServerName = g.V1.Fqdn.Value
			}
		}
	}

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Log.Level.Value = config.GlobalLogLevelToNginxLevel(logLevel)
	}
}

// PrepareSystemConfig returns a system configuration that can be used
// to start the service.
func (c *ConfigRequest) PrepareSystemConfig(certificate *config.TLSCredentials) (config.PreparedSystemConfig, error) {
	c.V1.Sys.Tls = certificate
	return c.V1.Sys, nil
}

var (
	capIPv6Supported bool = true
	capProbed        sync.Once
)

func ipV6Supported() bool {
	capProbed.Do(func() {
		l, err := net.Listen("tcp6", "")
		if err == nil {
			l.Close()
			return
		}

		// PRO-GRAMMING
		if opErr, ok := err.(*net.OpError); ok && opErr.Err != nil {
			if syscallErr, ok := opErr.Err.(*os.SyscallError); ok {
				if syscallErr.Err == syscall.EAFNOSUPPORT {
					logrus.Warn("No IPv6 support detected on host")
					capIPv6Supported = false
				}
			}
		}
	})
	return capIPv6Supported
}
