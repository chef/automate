package a1upgrade

import (
	"fmt"
	"io/ioutil"
	"path/filepath"
	"reflect"
	"strings"

	"github.com/pkg/errors"

	"github.com/chef/automate/api/config/authn"
	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/dex"
	es "github.com/chef/automate/api/config/elasticsearch"
	lb "github.com/chef/automate/api/config/load_balancer"
	ns "github.com/chef/automate/api/config/notifications"
	pg "github.com/chef/automate/api/config/postgresql"
	ac "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

/*

Within the a1upgrade application there are several operations that do I/O
which gives us ample room for failure and errors cases. To sanely handle
error printing, each each function of the A1Upgrade class should format and
return a go error if a failure occurred. It is the responsibility of the caller
to handle and wrap an error if necessary. Printing is done by the root caller,
which is most cases should be the Cobra command that executed the entire migration
or the step being run. There are cases where the A1Upgrade will need to write
information to a TTY, in which the embedded cli.Writer should be appropriate.

*/

const (
	// NOTE: (for future feature planning): workflow goes in this bucket also.
	// Detect if workflow is in use via config heuristic or presence of runners
	// or whatever.
	successMsg = `Generated Chef Automate v2 configuration based on your existing config settings.

You can view your Chef Automate v2 config file at /path/to/config.toml
`

	anotherMigrationExistsMsg = `--------------------------------------------------------------------------------
WARNING: Existing Chef Automate v1 data snapshot renamed
--------------------------------------------------------------------------------

A Chef Automate v1 data snapshot from a previous upgrade attempt was found. It
has been renamed to:

/home/bobotownFC/chefautomatev1export.tgz.1
`
)

// Silence linters for unused messages until we actually use them.
var _ string = successMsg
var _ string = anotherMigrationExistsMsg

//func getBackupRepos(r *DeliveryRunning,
//	s *DeliverySecrets) (*DataLifecycleServiceConfig_BackupRepo, *DataLifecycleServiceConfig_BackupRepo) {
//	// Default S3 backup config
//	dS3Cfg := DataLifecycleServiceConfig_S3Config{
//		Name:                 "default",
//		AccessKeyId:          r.Delivery.Backup.S3AccessKeyID,
//		Bucket:               r.Delivery.Backup.S3Bucket,
//		SecretAccessKey:      r.Delivery.Backup.S3SecretAccessKey,
//		Region:               r.Delivery.Backup.S3Region,
//		ServerSideEncryption: r.Delivery.Backup.S3ServerSideEncryption,
//		SseCustomerAlgorithm: r.Delivery.Backup.S3SSECustomerAlgorithm,
//		SseCustomerKey:       r.Delivery.Backup.S3SSECustomerKey,
//		SseCustomerKeyMd5:    r.Delivery.Backup.S3SSECustomerKeyMD5,
//		SseKmsKeyId:          r.Delivery.Backup.S3SSEKMSKeyID,
//	}
//
//	// Default filesystem backup config
//	dFsCfg := DataLifecycleServiceConfig_LocalConfig{
//		Name:      "default",
//		Directory: r.Delivery.Backup.Location,
//	}
//
//	// Elasticsearch S3 backup config
//	esS3Cfg := DataLifecycleServiceConfig_S3Config{
//		Name:                 "elasticsearch",
//		AccessKeyId:          r.Delivery.Backup.Elasticsearch.S3AccessKeyID,
//		Bucket:               r.Delivery.Backup.Elasticsearch.S3Bucket,
//		SecretAccessKey:      r.Delivery.Backup.Elasticsearch.S3SecretAccessKey,
//		Region:               r.Delivery.Backup.Elasticsearch.S3Region,
//		ServerSideEncryption: r.Delivery.Backup.Elasticsearch.S3ServerSideEncryption,
//	}
//
//	// Elasticsearch filesystem backup config
//	esFsCfg := DataLifecycleServiceConfig_LocalConfig{
//		Name:      "elasticsearch",
//		Directory: r.Delivery.Backup.Elasticsearch.Location,
//	}
//
//	dRetPol := DataLifecycleServiceConfig_RetentionPolicy{
//		Name:      "default",
//		Enabled:   r.Delivery.Reaper.Enabled,
//		MaxCount:  r.Delivery.Backup.Retention.MaxArchives,
//		MinDays:   r.Delivery.Reaper.RetentionPeriod,
//		RepoType:  r.Delivery.Backup.Type,
//		Mode:      r.Delivery.Reaper.Mode,
//		Threshold: r.Delivery.Reaper.Threshold,
//		S3:        &dS3Cfg,
//		Local:     &dFsCfg,
//	}
//
//	esRetPol := DataLifecycleServiceConfig_RetentionPolicy{
//		Name:      "elasticsearch",
//		Enabled:   r.Delivery.Reaper.Enabled,
//		MaxCount:  r.Delivery.Backup.Retention.MaxSnapshots,
//		MinDays:   r.Delivery.Reaper.RetentionPeriod,
//		RepoType:  r.Delivery.Backup.Type,
//		Mode:      r.Delivery.Reaper.Mode,
//		Threshold: r.Delivery.Reaper.Threshold,
//		S3:        &esS3Cfg,
//		Local:     &esFsCfg,
//	}
//
//	dBRepo := DataLifecycleServiceConfig_BackupRepo{
//		Name:            "default",
//		Type:            r.Delivery.Backup.Type,
//		RetentionPolicy: &dRetPol,
//		S3:              &dS3Cfg,
//		Local:           &dFsCfg,
//	}
//
//	esBRepo := DataLifecycleServiceConfig_BackupRepo{
//		Name:            "elasticsearch",
//		Type:            r.Delivery.Backup.Elasticsearch.Type,
//		RetentionPolicy: &esRetPol,
//		S3:              &esS3Cfg,
//		Local:           &esFsCfg,
//	}
//
//	return &dBRepo, &esBRepo
//}

func getPostgresSettings(r *DeliveryRunning) (*pg.ConfigRequest_V1_System_PGConfig, error) {
	var err error

	c := pg.NewPGConfig()

	checkpointCompletionTarget, err := r.Delivery.PostgreSQL.CheckpointCompletionTarget.Float64()
	if err != nil {
		return nil, err
	}

	maxConnections, err := r.Delivery.PostgreSQL.MaxConnections.Int64()
	if err != nil {
		return nil, err
	}

	// If we have the default A1 CIDR addresses we'll replace them with "samehost"
	// so that MD5 Auth is allowed by all local services on any address.
	if reflect.DeepEqual(r.Delivery.PostgreSQL.MD5AuthCIDRAddresses, []string{"127.0.0.1/32", "::1/128"}) {
		c.Md5AuthCidrAddresses = []string{"samehost"}
	} else {
		// Otherwise, make sure that our local services are allowed to connect on
		// any address.
		c.Md5AuthCidrAddresses = append(r.Delivery.PostgreSQL.MD5AuthCIDRAddresses, "samehost")
	}

	c.CheckpointTimeout = w.String(r.Delivery.PostgreSQL.CheckpointTimeout)
	c.CheckpointCompletionTarget = w.Float(float32(checkpointCompletionTarget))
	c.CheckpointWarning = w.String(r.Delivery.PostgreSQL.CheckpointWarning)
	c.EffectiveCacheSize = w.String(r.Delivery.PostgreSQL.EffectiveCacheSize)
	c.MaxConnections = w.Int32(int32(maxConnections))
	c.SharedBuffers = w.String(r.Delivery.PostgreSQL.SharedBuffers)
	c.WorkMem = w.String(r.Delivery.PostgreSQL.WorkMem)

	return c, nil
}

func getLBServiceSettings(r *DeliveryRunning, _ *DeliverySecrets) (*lb.ConfigRequest_V1_System_Service, error) {
	httpsPort, err := r.Delivery.Nginx.HTTPSPort.Int64()
	if err != nil {
		return nil, err
	}

	httpPort, err := r.Delivery.Nginx.HTTPPort.Int64()
	if err != nil {
		return nil, err
	}

	return &lb.ConfigRequest_V1_System_Service{
		HttpsPort: w.Int32(int32(httpsPort)),
		HttpPort:  w.Int32(int32(httpPort)),
	}, nil
}

func getLBNginxSettings(r *DeliveryRunning, _ *DeliverySecrets) (*lb.ConfigRequest_V1_System_Nginx, error) {
	workerProcesses, err := r.Delivery.Nginx.WorkerProcesses.Int64()
	if err != nil {
		return nil, err
	}

	workerConnections, err := r.Delivery.Nginx.WorkerConnections.Int64()
	if err != nil {
		return nil, err
	}

	keepaliveTimeout, err := r.Delivery.Nginx.KeepaliveTimeout.Int64()
	if err != nil {
		return nil, err
	}

	keepaliveRequests, err := r.Delivery.Nginx.KeepaliveRequests.Int64()
	if err != nil {
		return nil, err
	}

	largeClientHeaderBuffersNumber, err := r.Delivery.Nginx.LargeClientHeaderBuffers.Number.Int64()
	if err != nil {
		return nil, err
	}

	return &lb.ConfigRequest_V1_System_Nginx{
		Main: &lb.ConfigRequest_V1_System_Nginx_Main{
			WorkerProcesses: w.Int32(int32(workerProcesses)),
		},
		Events: &lb.ConfigRequest_V1_System_Nginx_Events{
			WorkerConnections:     w.Int32(int32(workerConnections)),
			WorkerProcessorMethod: w.String(r.Delivery.Nginx.WorkerProcessorMethod),
			MultiAccept:           w.String(r.Delivery.Nginx.MultiAccept),
		},
		Http: &lb.ConfigRequest_V1_System_Nginx_Http{
			AccessLogBufferSize:            w.String(r.Delivery.Nginx.AccessLog.BufferSize),
			AccessLogFlushTime:             w.String(r.Delivery.Nginx.AccessLog.FlushTime),
			ClientMaxBodySize:              w.String(r.Delivery.Nginx.ClientMaxBodySize),
			KeepaliveTimeout:               w.Int32(int32(keepaliveTimeout)),
			KeepaliveRequests:              w.Int32(int32(keepaliveRequests)),
			Gzip:                           w.String(r.Delivery.Nginx.GZip),
			GzipCompLevel:                  w.String(r.Delivery.Nginx.GZipCompLevel),
			GzipHttpVersion:                w.String(r.Delivery.Nginx.GZipHTTPVersion),
			GzipProxied:                    w.String(r.Delivery.Nginx.GZipProxied),
			GzipTypes:                      w.String(strings.Join(r.Delivery.Nginx.GZipTypes, " ")),
			LargeClientHeaderBuffersNumber: w.Int32(int32(largeClientHeaderBuffersNumber)),
			LargeClientHeaderBuffersSize:   w.String(r.Delivery.Nginx.LargeClientHeaderBuffers.Size),
			Sendfile:                       w.String(r.Delivery.Nginx.Sendfile),
			SslCiphers:                     w.String(r.Delivery.Nginx.SSLCiphers),
			SslProtocols:                   w.String(r.Delivery.Nginx.SSLProtocols),
			TcpNodelay:                     w.String(r.Delivery.Nginx.TCPNoDelay),
			TcpNopush:                      w.String(r.Delivery.Nginx.TCPNoPush),
		},
	}, nil
}

func getFrontendTLSCreds(r *DeliveryRunning) ([]*ac.FrontendTLSCredential, error) {
	var frontendTLSCreds []*ac.FrontendTLSCredential

	// If the user provided their own SSLCertificates we'll need to read them
	// off of the disk and populate our configuration with them.
	if len(r.Delivery.Delivery.SSLCertificates) > 0 {
		for name, v := range r.Delivery.Delivery.SSLCertificates {
			c := &ac.FrontendTLSCredential{}
			c.ServerName = name
			certContent, err := ReadURI(v["crt"])
			if err != nil {
				return nil, errors.Wrap(err, "Failed to load SSL Certificate")
			}
			c.Cert = string(certContent)

			keyContent, err := ReadURI(v["key"])
			if err != nil {
				return nil, errors.Wrap(err, "Failed to load SSL Certificate Key")
			}
			c.Key = string(keyContent)

			frontendTLSCreds = append(frontendTLSCreds, c)
		}
	} else {
		// Try and get the self-signed certificate
		c := &ac.FrontendTLSCredential{}
		c.ServerName = r.Delivery.FQDN

		certPath := filepath.Join(r.Delivery.Nginx.Dir, "ca", fmt.Sprintf("%s.crt", c.ServerName))
		certContent, err := ioutil.ReadFile(certPath)
		if err != nil {
			return nil, errors.Wrap(err, "Failed to load self-signed SSL Certificate")
		}
		c.Cert = string(certContent)

		keyPath := filepath.Join(r.Delivery.Nginx.Dir, "ca", fmt.Sprintf("%s.key", c.ServerName))
		keyContent, err := ioutil.ReadFile(keyPath)
		if err != nil {
			return nil, errors.Wrap(err, "Failed to load self-signed SSL Certificate Key")
		}
		c.Key = string(keyContent)

		frontendTLSCreds = append(frontendTLSCreds, c)
	}

	return frontendTLSCreds, nil
}

func getLDAPSettings(r *DeliveryRunning) (*dex.ConfigRequest_V1_Ldap, error) {
	if len(r.Delivery.Delivery.LDAPHosts) < 1 {
		return nil, nil
	}

	port, err := r.Delivery.Delivery.LDAPPort.Int64()
	if err != nil {
		return nil, errors.Wrap(err, "error parsing Automate 1 LDAPPort configuration")
	}
	host := fmt.Sprintf("%s:%d", r.Delivery.Delivery.LDAPHosts[0], port)
	baseDn := r.Delivery.Delivery.LDAPBaseDN

	// A1 will write default LDAP attributes into delivery-running. If we find
	// the default attributes we don't want them to persist to A2 so we'll drop
	// them here and return nil.
	if host == "" && baseDn == "OU=Employees,OU=Domain users,DC=examplecorp,DC=com" {
		return nil, nil
	}

	// If we're still here we should be reasonably confident that our LDAP settings
	// from A1 are real and that we should migrate them over.
	return &dex.ConfigRequest_V1_Ldap{
		BaseUserSearchDn: w.String(r.Delivery.Delivery.LDAPBaseDN),
		BindDn:           w.String(r.Delivery.Delivery.LDAPBindDN),
		Host:             w.String(host),
		BindPassword:     w.String(r.Delivery.Delivery.LDAPBindDNPassword),
		EmailAttr:        w.String(r.Delivery.Delivery.LDAPMail),
		UserIdAttr:       w.String(r.Delivery.Delivery.LDAPLogin),
		UsernameAttr:     w.String(r.Delivery.Delivery.LDAPLogin),
	}, nil
}

func getProxySettings(r *DeliveryRunning) (*ac.Proxy, error) {
	// A1 will write default proxy settings into the delivery-running so we only
	// want to migrate them if we think they have been set by the user. The A1
	// default proxy settings will be `null` in the delivery-running but will
	// be marshaled into an empty string. Since you will need a host proxy we
	// can assume that if it's empty that we don't need to set these.
	if r.Delivery.Delivery.Proxy.Host == "" {
		return nil, nil
	}

	port, err := r.Delivery.Delivery.Proxy.Port.Int64()
	if err != nil {
		return nil, err
	}

	return &ac.Proxy{
		Host:     w.String(r.Delivery.Delivery.Proxy.Host),
		Port:     w.Int32(int32(port)),
		User:     w.String(r.Delivery.Delivery.Proxy.User),
		Password: w.String(r.Delivery.Delivery.Proxy.Password),
		NoProxy:  r.Delivery.Delivery.Proxy.NoProxy,
	}, nil
}

func generateMigrationOverrideConfig(r *DeliveryRunning, s *DeliverySecrets) (*dc.AutomateConfig, error) {
	frontendTLSCreds, err := getFrontendTLSCreds(r)
	if err != nil {
		return nil, err
	}

	pgConfig, err := getPostgresSettings(r)
	if err != nil {
		return nil, errors.Wrap(err, "error parsing Automate 1 PostgreSQL configuration")
	}

	lBServiceConfig, err := getLBServiceSettings(r, s)
	if err != nil {
		return nil, errors.Wrap(err, "error generating Automate 2 Load Balancer configuration from Automate 1 configuration")
	}

	lBNginxConfig, err := getLBNginxSettings(r, s)
	if err != nil {
		return nil, errors.Wrap(err, "error parsing Automate 1 nginx configuration")
	}

	proxyConfig, err := getProxySettings(r)
	if err != nil {
		return nil, errors.Wrap(err, "error parsing Automate 1 proxy configuration")
	}

	ldapConfig, err := getLDAPSettings(r)
	if err != nil {
		return nil, errors.Wrap(err, "error parsing Automate 1 LDAP configuration")
	}

	adminPassword, err := dc.GeneratePassword()
	if err != nil {
		return nil, errors.Wrap(err, "error generating Automate 2 admin password")
	}

	cfg := &dc.AutomateConfig{
		Global: &ac.GlobalConfig{
			V1: &ac.V1{
				Fqdn: w.String(r.Delivery.FQDN),
			},
		},
		AuthN: &authn.ConfigRequest{
			V1: &authn.ConfigRequest_V1{
				Sys: &authn.ConfigRequest_V1_System{
					Proxy: proxyConfig,
					Service: &authn.ConfigRequest_V1_System_Service{
						A1DataCollectorToken: w.String(r.Delivery.DataCollector.Token),
					},
				},
			},
		},
		Deployment: &dc.ConfigRequest{
			V1: &dc.ConfigRequest_V1{
				Svc: &dc.ConfigRequest_V1_Service{
					AdminUser: &dc.ConfigRequest_V1_AdminUser{
						Password: w.String(adminPassword),
					},
					// TODO: These fields are not supported yet
					//  IpVersion: w.String(r.Delivery.IPVersion),
					//  Fips:      w.Bool(r.Delivery.FIPS.Enabled),
				},
			},
		},
		Dex: &dex.ConfigRequest{
			V1: &dex.ConfigRequest_V1{
				Sys: &dex.ConfigRequest_V1_System{
					// TODO: Backup settings
					Connectors: &dex.ConfigRequest_V1_Connectors{
						Ldap: ldapConfig,
					},
				},
			},
		},
		// TODO: Actually override ES Settings instead of overriding with
		// the defaults.
		Elasticsearch: &es.ConfigRequest{
			V1: &es.ConfigRequest_V1{
				// TODO: apply the below config
				// - r.Delivery.Elasticsearch.MaxMapCount
				//   in a1 this is set via sysctl in the run hook but is not exposed in
				//   core/elasticsearch yet. we also have this as a preflight check which
				//   may be the correct approach instead of setting it for our users
				// - r.Delivery.Elasticsearch.NewMemory
				//   set in the jvm options template and but this is not exposed in
				//   core/elasticsearch yet
				// - r.Delivery.Elasticsearch.RepoPath.{Data,Logs,Backup}
				//   in a1 these are going to be set to the default /var/opt/delivery
				//   directories and it doesn't necessarily make sense to move them over
				//   to a2, which will have its own different data directories
				// - r.Delivery.Elasticsearch.ClusterURLS
				//   there's no analog yet in A2, and using the unicast hosts for external
				//   elasticsearch isn't going to work in this way
				Sys: &es.ConfigRequest_V1_System{
					Runtime: &es.ConfigRequest_V1_Runtime{
						MaxOpenFiles:    w.String(r.Delivery.Elasticsearch.MaxOpenFiles.String()),
						MaxLockedMemory: w.String(r.Delivery.Elasticsearch.MaxLockedMemory),
						Heapsize:        w.String(r.Delivery.Elasticsearch.HeapSize),
					},
				},
			},
		},
		LoadBalancer: &lb.ConfigRequest{
			V1: &lb.ConfigRequest_V1{
				Sys: &lb.ConfigRequest_V1_System{
					Service:     lBServiceConfig,
					Ngx:         lBNginxConfig,
					Proxy:       proxyConfig,
					FrontendTls: frontendTLSCreds,
				},
			},
		},
		Notifications: &ns.ConfigRequest{
			V1: &ns.ConfigRequest_V1{
				Sys: &ns.ConfigRequest_V1_System{
					Proxy: proxyConfig,
				},
			},
		},
		Postgresql: &pg.ConfigRequest{
			V1: &pg.ConfigRequest_V1{
				Sys: &pg.ConfigRequest_V1_System{
					Pg: pgConfig,
				},
			},
		},
	}

	return cfg, nil
}
