package shared

import (
	"fmt"
	"net/url"
	"strings"

	gw "github.com/golang/protobuf/ptypes/wrappers"
	"github.com/sirupsen/logrus"

	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/proxy"
	"github.com/chef/automate/lib/stringutils"
)

// NewGlobalConfig returns a new GlobalConfig instance with zero values.
func NewGlobalConfig() *GlobalConfig {
	return &GlobalConfig{
		V1: &V1{},
	}
}

// DefaultGlobalConfig returns a new GlobalConfig instance with default values.
func DefaultGlobalConfig() *GlobalConfig {
	return &GlobalConfig{
		V1: &V1{
			Backups: &Backups{
				Location: w.String("filesystem"),
				Filesystem: &Backups_Filesystem{
					Path: w.String("/var/opt/chef-automate/backups"),
				},
			},
			Mlsa: &Mlsa{
				Accept: w.Bool(true),
			},
		},
	}
}

// Validate validates that the config is valid. If validation succeeds it will
// return nil, if it fails it will return a new instance of config.InvalidConfigError
// that has the missing keys and invalid fields populated.
func (c *GlobalConfig) Validate() error { // nolint gocyclo
	cfgErr := NewInvalidConfigError()

	if c.GetV1() == nil {
		cfgErr.AddMissingKey("global.v1")
		return cfgErr
	}

	if c.GetV1().GetFqdn() == nil {
		cfgErr.AddMissingKey("global.v1.fqdn")
	}

	if len(c.GetV1().GetFrontendTls()) < 1 {
		// It's not currently mandatory to configure frontend_tls certs via
		// the global config. They can be set on the load_balancer config instead.
		//cfgErr.AddMissingKey("global.v1.frontend_tls")
	} else {
		for _, tls := range c.V1.FrontendTls {
			if tls.Cert == "" {
				cfgErr.AddMissingKey("global.v1.frontend_tls.cert")
			}
			if tls.Key == "" {
				cfgErr.AddMissingKey("global.v1.frontend_tls.key")
			}
			// TODO: The load balancer code will copy the FQDN (above) over
			// the server_name setting if the server name is set to "" or
			// "localhost" It feels wrong to do that in a validation function.
			// Maybe we need to add a method on GlobalConfig to return a
			// computed fixed up version (?)
			//  if tls.ServerName == "" {
			//  	cfgErr.AddInvalidValue("global.v1.frontend_tls.server_name", "server_name must be a valid FQDN")
			//  }
		}
	}

	bu := c.GetV1().GetBackups()
	location := bu.GetLocation().GetValue()
	switch location {
	case "filesystem":
		p := bu.GetFilesystem().GetPath().GetValue()
		if p == "" {
			cfgErr.AddMissingKey("global.v1.backups.filesystem.path")
		}

		// NOTE: We don't manage the permissions of the backup directory but
		// we'd like to prevent the user from setting the backup directory to
		// an invalid location. Because we do validation at deploy time,
		// restore time, and config patch/set time, we cannot guarantee that
		// the backup directory will exist yet or that the hab user exists.
		// Therefore, we'll only validate on the happy path: raise a
		// validation error if the hab user exists, the backup directory exists,
		// and the hab user doesn't have read/write/exec permissions on the
		// directory.
		ok, err := fileutils.ReadWriteExecutable("hab", p)
		if err != nil {
			logrus.WithError(err).WithFields(logrus.Fields{
				"user": "hab",
				"path": p,
			}).Debug("failed checking for read/write/exec on path")
		}
		if err == nil && !ok {
			cfgErr.AddInvalidValue(
				"global.v1.backups.filesystem.path",
				fmt.Sprintf("the 'hab' user must have read/write/exec permissions to path: %s", p),
			)
		}
	case "s3":
		if bu.GetS3().GetBucket().GetEndpoint().GetValue() == "" {
			cfgErr.AddMissingKey("global.v1.backups.s3.bucket.endpoint")
		}

		if bu.GetS3().GetBucket().GetName().GetValue() == "" {
			cfgErr.AddMissingKey("global.v1.backups.s3.bucket.name")
		}

		// The user might be relying on IAM for S3 credentials. Here we'll
		// make sure that if credentials are provided that both an access_key
		// and secret_key have been provided.
		if bu.GetS3().GetCredentials() != nil {
			access_key := bu.GetS3().GetCredentials().GetAccessKey().GetValue()
			secret_key := bu.GetS3().GetCredentials().GetSecretKey().GetValue()

			if secret_key != "" || access_key != "" {
				if secret_key == "" {
					cfgErr.AddMissingKey("global.v1.backups.s3.credentials.secret_key")
				}

				if access_key == "" {
					cfgErr.AddMissingKey("global.v1.backups.s3.credentials.access_key")
				}
			}
		}
	case "gcs":
		if bu.GetGcs().GetBucket().GetProjectid().GetValue() == "" {
			cfgErr.AddMissingKey("global.v1.backups.gcs.bucket.projectid")
		}

		if bu.GetGcs().GetBucket().GetName().GetValue() == "" {
			cfgErr.AddMissingKey("global.v1.backups.gcs.bucket.name")
		}

		// The user might be relying on IAM for GCP credentials. Here we'll
		// make sure that if credentials are provided
		if bu.GetGcs().GetCredentials() != nil {
			json := bu.GetGcs().GetCredentials().GetJson().GetValue()

			if json == "" {
				cfgErr.AddMissingKey("global.v1.backups.gcs.credentials.json")
			}
		}
	default:
		// Make sure that if a backup location is specified that is valid. If
		// none is given the default configuration "filesystem" location will
		// be used.
		if location != "" {
			cfgErr.AddInvalidValue("global.v1.backups.location", "Must be either 'filesystem', 's3', or 'gcs'")
		}
	}

	if log := c.GetV1().Log; log != nil {
		if level := log.GetLevel().GetValue(); level != "" {
			switch level {
			case "debug", "info", "warning", "error", "fatal", "panic":
			default:
				cfgErr.AddInvalidValue("global.v1.log.level",
					fmt.Sprintf("'%s' must be one of 'debug, 'info', 'warning', 'error', 'fatal', 'panic'", level))
			}
		}
		if format := log.GetFormat().GetValue(); format != "" {
			// logrus does support custom formatters. For now we'll only
			// support the built-in text and json formatters at the global
			// level.
			if format != "text" && format != "json" {
				cfgErr.AddInvalidValue("global.v1.log.format",
					fmt.Sprintf("'%s' must be 'text' or 'json'", format))
			}
		}
	}

	if rawURL := c.GetV1().GetExternal().GetAutomate().GetNode().GetValue(); rawURL != "" {
		externalAutomateURL, err := url.Parse(rawURL)
		if err != nil {
			cfgErr.AddInvalidValue("global.v1.external.automate.node", "must be url")
		} else {
			scheme := externalAutomateURL.Scheme
			switch scheme {
			case "", "http", "https":
			default:
				cfgErr.AddInvalidValue("global.v1.external.automate.node", "only https and http are supported")
			}
		}
	}
	switch c.GetV1().GetExternal().GetAutomate().GetAuth().GetScheme().GetValue() {
	case "", "token":
		break
	default:
		cfgErr.AddInvalidValue("global.v1.external.data_collector.auth.scheme", "scheme must be one of '', 'token'")
	}

	if externalES := c.GetV1().GetExternal().GetElasticsearch(); externalES.GetEnable().GetValue() {
		// External ES nodes all either have https urls or https urls
		nodes := externalES.GetNodes()
		httpsNodes := make([]string, 0)
		for _, n := range nodes {
			ns := n.GetValue()
			if strings.HasPrefix(ns, "https") {
				httpsNodes = append(httpsNodes, ns)
			}
		}
		if len(httpsNodes) > 0 && len(httpsNodes) < len(nodes) {
			cfgErr.AddInvalidValue("global.v1.external.elasticsearch.nodes", "Cannot mix http and https nodes")
		}

		// Only one of root_cert or root_cert_file has been specified
		rc := c.GetV1().GetExternal().GetElasticsearch().GetSsl().GetRootCert().GetValue()
		rcf := c.GetV1().GetExternal().GetElasticsearch().GetSsl().GetRootCertFile().GetValue()
		if rc != "" && rcf != "" {
			cfgErr.AddInvalidValue("global.v1.external.elasticsearch.ssl", "Specify either global.v1.external.elasticsearch.ssl.root_cert or global.v1.external.elasticsearch.ssl.root_cert_file, but not both.")
		}

		auth := c.GetV1().GetExternal().GetElasticsearch().GetAuth()
		scheme := auth.GetScheme().GetValue()
		if scheme != "" {
			// External ES uses a supported auth scheme
			if scheme != "basic_auth" {
				cfgErr.AddInvalidValue("global.v1.external.elasticsearch.auth.scheme", "Scheme should be 'basic_auth'.")
			}

			// Username and password specified in config if using basic auth
			if scheme == "basic_auth" {
				u := auth.GetBasicAuth().GetUsername().GetValue()
				p := auth.GetBasicAuth().GetPassword().GetValue()
				if u == "" {
					cfgErr.AddMissingKey("global.v1.external.elasticsearch.basic_auth.username")
				}
				if p == "" {
					cfgErr.AddMissingKey("global.v1.external.elasticsearch.basic_auth.password")
				}
			}
		}
	}

	if externalPG := c.GetV1().GetExternal().GetPostgresql(); externalPG.GetEnable().GetValue() {
		if auth := c.GetV1().GetExternal().GetPostgresql().GetAuth(); auth.GetScheme().GetValue() != "password" {
			// use supported auth scheme (currently only password auth is
			// supported for postgres)
			cfgErr.AddInvalidValue("global.v1.external.postgresql.auth.scheme", "Scheme should be 'password'.")
		} else {
			// superuser username and password
			su := auth.GetPassword().GetSuperuser().GetUsername().GetValue()
			sp := auth.GetPassword().GetSuperuser().GetPassword().GetValue()
			if su == "" {
				cfgErr.AddMissingKey("global.v1.external.postgresql.auth.password.superuser.username")
			}
			if sp == "" {
				cfgErr.AddMissingKey("global.v1.external.postgresql.auth.password.superuser.password")
			}

			// dbuser username and password
			du := auth.GetPassword().GetDbuser().GetUsername().GetValue()
			dp := auth.GetPassword().GetDbuser().GetPassword().GetValue()
			if du == "" {
				cfgErr.AddMissingKey("global.v1.external.postgresql.auth.password.dbuser.username")
			}
			if dp == "" {
				cfgErr.AddMissingKey("global.v1.external.postgresql.auth.password.dbuser.password")
			}
		}
	}

	if cfgErr.IsEmpty() {
		return nil
	}

	return cfgErr
}

// ProxyString returns the proxy configuration formatted into the canonical
// HTTP_PROXY style formatting.
func (c *GlobalConfig) ProxyString() *gw.StringValue {
	if c.V1.Proxy == nil {
		return nil
	}

	proxy := c.V1.Proxy
	if proxy.Host == nil {
		return nil
	}

	b := strings.Builder{}
	// NOTE: from testing, it appears that Rust (hab) requires "http://" to be
	// at the head of the proxy URLs
	b.WriteString("http://") // nolint: errcheck

	if proxy.User != nil {
		authPart := fmt.Sprintf("%s:%s", proxy.User.Value, proxy.Password.Value)
		b.WriteString(url.PathEscape(authPart)) // nolint: errcheck
		b.WriteString("@")                      // nolint: errcheck
	}

	hostPortPart := fmt.Sprintf("%s:%d", proxy.Host.Value, proxy.Port.Value)
	b.WriteString(hostPortPart) // nolint: errcheck
	return w.String(b.String())
}

// NoProxyString turns a non-empty NoProxy whitelist into a string of comma-separated
// entries for easier consumption by the hab config.
func (c *GlobalConfig) NoProxyString() *gw.StringValue {
	// If no proxy is set at all, move along.
	if c.V1.Proxy == nil {
		return nil
	}

	// Just return the default list
	if c.V1.Proxy.NoProxy == nil {
		return w.String(strings.Join(proxy.DefaultNoProxyEntries, ","))
	}

	noProxy := c.V1.Proxy.NoProxy
	for _, noProxyEntry := range proxy.DefaultNoProxyEntries {
		if !stringutils.SliceContains(noProxy, noProxyEntry) {
			noProxy = append(noProxy, noProxyEntry)
		}
	}

	return w.String(strings.Join(noProxy, ","))
}
