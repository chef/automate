package a1upgrade

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"path/filepath"
	"reflect"
	"strings"

	"github.com/golang/protobuf/ptypes/wrappers"
	"github.com/pkg/errors"

	"github.com/chef/automate/api/config/authn"
	"github.com/chef/automate/api/config/bifrost"
	"github.com/chef/automate/api/config/bookshelf"
	"github.com/chef/automate/api/config/cs_nginx"
	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/dex"
	es "github.com/chef/automate/api/config/elasticsearch"
	"github.com/chef/automate/api/config/erchef"
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

// nolint: gocyclo
func getErchefSettings(r *ChefServerRunning) (*erchef.ConfigRequest_V1_System, error) {
	var err error

	sys := erchef.NewConfigRequest().GetV1().GetSys()
	c := r.PrivateChef.OpscodeErchef
	d := r.PrivateChef.DataCollector

	sys.Api.AuthSkew, err = to32w(c.AuthSkew)
	if err != nil {
		return sys, err
	}

	sys.Api.BaseResourceUrl = w.String(c.BaseResourceURL)

	sys.Api.BulkFetchBatchSize, err = to32w(c.BulkFetchBatchSize)
	if err != nil {
		return sys, err
	}

	sys.Api.MaxRequestSize, err = to32w(c.MaxRequestSize)
	if err != nil {
		return sys, err
	}

	sys.Api.StrictSearchResultAcls = w.Bool(c.StrictSearchResultACLs)

	sys.Authz.Fanout, err = to32w(c.AuthzFanout)
	if err != nil {
		return sys, err
	}

	sys.Authz.PoolQueueTimeout, err = to32w(c.AuthzPoolerTimeout)
	if err != nil {
		return sys, err
	}

	sys.Authz.Timeout, err = to32w(c.AuthzTimeout)
	if err != nil {
		return sys, err
	}

	sys.DataCollector.PoolInitSize, err = to32w(d.HTTPInitCount)
	if err != nil {
		return sys, err
	}

	sys.DataCollector.PoolMaxSize, err = to32w(d.HTTPMaxCount)
	if err != nil {
		return sys, err
	}

	sys.DataCollector.Timeout, err = to32w(d.Timeout)
	if err != nil {
		return sys, err
	}

	sys.Depsolver.PoolQueueMax, err = to32w(c.DepsolverPoolQueueMax)
	if err != nil {
		return sys, err
	}

	sys.Depsolver.PoolInitSize, err = to32w(c.DepsolverWorkerCount)
	if err != nil {
		return sys, err
	}

	sys.Depsolver.PoolQueueTimeout, err = to32w(c.DepsolverPoolerTimeout)
	if err != nil {
		return sys, err
	}

	sys.Depsolver.PoolMaxSize = sys.Depsolver.PoolInitSize // same as DepsolverWorkerCount

	sys.Depsolver.Timeout, err = to32w(c.DepsolverTimeout)
	if err != nil {
		return sys, err
	}

	sys.Index.BatchMaxWait, err = to32w(c.SearchBatchSizeMaxWait)
	if err != nil {
		return sys, err
	}

	sys.Index.BatchSize, err = to32w(c.SearchBatchSizeMaxSize)
	if err != nil {
		return sys, err
	}

	sys.Index.PoolInitSize, err = to32w(c.SolrHTTPInitCount)
	if err != nil {
		return sys, err
	}

	sys.Index.PoolMaxSize, err = to32w(c.SolrHTTPMaxCount)
	if err != nil {
		return sys, err
	}
	sys.Index.ReindexBatchSize, err = to32w(c.ReindexBatchSize)
	if err != nil {
		return sys, err
	}

	sys.Index.ReindexItemRetries, err = to32w(c.ReindexItemRetries)
	if err != nil {
		return sys, err
	}

	sys.Index.ReindexSleepMaxMs, err = to32w(c.ReindexSleepMaxMs)
	if err != nil {
		return sys, err
	}

	sys.Index.ReindexSleepMinMs, err = to32w(c.ReindexSleepMinMs)
	if err != nil {
		return sys, err
	}

	sys.Index.Timeout, err = to32w(c.SolrTimeout)
	if err != nil {
		return sys, err
	}

	sys.Keygen.CacheSize, err = to32w(c.KeygenCacheSize)
	if err != nil {
		return sys, err
	}

	sys.Keygen.StartSize, err = to32w(c.KeygenStartSize)
	if err != nil {
		return sys, err
	}

	sys.Keygen.Timeout, err = to32w(c.KeygenTimeout)
	if err != nil {
		return sys, err
	}

	sys.Log.MaxErrorLogsPerSecond, err = to32w(c.LogRotation.MaxMessagesPerSecond)
	if err != nil {
		return sys, err
	}

	sys.Log.RotationMaxBytes, err = to64w(c.LogRotation.FileMaxbytes)
	if err != nil {
		return sys, err
	}

	sys.Log.RotationMaxFiles, err = to32w(c.LogRotation.NumToKeep)
	if err != nil {
		return sys, err
	}

	sys.Memory.MaxBytes, err = to64w(c.MemoryMaxbytes)
	if err != nil {
		return sys, err
	}

	sys.Sql.PoolInitSize, err = to32w(c.DBPoolInit)
	if err != nil {
		return sys, err
	}

	sys.Sql.PoolMaxSize, err = to32w(c.DBPoolMax)
	if err != nil {
		return sys, err
	}

	sys.Sql.PoolQueueMax, err = to32w(c.DBPoolQueueMax)
	if err != nil {
		return sys, err
	}

	sys.Sql.PoolQueueTimeout, err = to32w(c.DBPoolerTimeout)
	if err != nil {
		return sys, err
	}

	sys.Sql.Timeout, err = to32w(c.SQLDBTimeout)
	if err != nil {
		return sys, err
	}

	return sys, nil
}

func getInfraServerNginxSettings(cs *ChefServerRunning) (*cs_nginx.ConfigRequest_V1_System, error) {
	var err error

	ngx := cs.PrivateChef.CSNginx
	sys := cs_nginx.NewConfigRequest().GetV1().GetSys()

	sys.Ngx.Events.WorkerConnections, err = to32w(ngx.WorkerConnections)
	if err != nil {
		return sys, err
	}

	sys.Ngx.Http.ClientMaxBodySize = w.String(ngx.ClientMaxBodySize)
	sys.Ngx.Http.Gzip = w.String(ngx.Gzip)
	sys.Ngx.Http.GzipHttpVersion = w.String(ngx.GzipHTTPVersion)
	sys.Ngx.Http.GzipCompLevel = w.String(ngx.GzipCompLevel)
	sys.Ngx.Http.GzipProxied = w.String(ngx.GzipProxied)

	sys.Ngx.Http.KeepaliveTimeout, err = to32w(ngx.KeepaliveTimeout)
	if err != nil {
		return sys, err
	}

	sys.Ngx.Http.ProxyConnectTimeout, err = to32w(ngx.ProxyConnectTimeout)
	if err != nil {
		return sys, err
	}

	sys.Ngx.Http.Sendfile = w.String(ngx.Sendfile)

	sys.Ngx.Http.ServerNamesHashBucketSize, err = to32w(ngx.ServerNamesHashBucketSize)
	if err != nil {
		return sys, err
	}

	sys.Ngx.Http.TcpNodelay = w.String(ngx.TCPNodelay)

	sys.Ngx.Http.TcpNopush = w.String(ngx.TCPNopush)
	if err != nil {
		return sys, err
	}

	sys.Ngx.Main.WorkerProcesses, err = to32w(ngx.WorkerProcesses)
	if err != nil {
		return sys, err
	}

	return sys, nil
}

func getBookshelfSettings(cs *ChefServerRunning) (*bookshelf.ConfigRequest_V1_System, error) {
	var err error

	sys := bookshelf.NewConfigRequest().GetV1().GetSys()
	bk := cs.PrivateChef.Bookshelf

	sys.Bookshelf.AbandonedUploadCleanupInterval, err = to32w(bk.AbandonedUploadCleanupInterval)
	if err != nil {
		return sys, err
	}

	sys.Bookshelf.DeletedDataCleanupInterval, err = to32w(bk.DeleteDataCleanupInterval)
	if err != nil {
		return sys, err
	}

	sys.Bookshelf.SqlRetryCount, err = to32w(bk.SqlRetryCount)
	if err != nil {
		return sys, err
	}

	sys.Bookshelf.SqlRetryDelay, err = to32w(bk.SqlRetryDelay)
	if err != nil {
		return sys, err
	}

	sys.Bookshelf.StreamDownload = w.Bool(bk.StreamDownload)

	sys.Log.RotationMaxBytes, err = to64w(bk.LogRotation.FileMaxbytes)
	if err != nil {
		return sys, err
	}

	sys.Log.RotationMaxFiles, err = to32w(bk.LogRotation.NumToKeep)
	if err != nil {
		return sys, err
	}

	sys.Sql.PoolInitSize, err = to32w(bk.DbPoolSize)
	if err != nil {
		return sys, err
	}

	sys.Sql.PoolMaxSize, err = to32w(bk.DbPoolMax)
	if err != nil {
		return sys, err
	}

	sys.Sql.PoolQueueMax, err = to32w(bk.DbPoolQueueMax)
	if err != nil {
		return sys, err
	}

	sys.Sql.PoolQueueTimeout, err = to32w(bk.DbPoolerTimeout)
	if err != nil {
		return sys, err
	}

	sys.Sql.Timeout, err = to32w(bk.SqlDbTimeout)
	if err != nil {
		return sys, err
	}

	return sys, nil
}

func getBifrostSettings(cs *ChefServerRunning) (*bifrost.ConfigRequest_V1_System, error) {
	var err error

	sys := bifrost.NewConfigRequest().GetV1().GetSys()
	bf := cs.PrivateChef.OcBifrost

	sys.Log.MaxErrorLogsPerSecond, err = to32w(bf.LogRotation.MaxMessagesPerSecond)
	if err != nil {
		return sys, err
	}

	sys.Log.RotationMaxBytes, err = to64w(bf.LogRotation.FileMaxbytes)
	if err != nil {
		return sys, err
	}

	sys.Log.RotationMaxFiles, err = to32w(bf.LogRotation.NumToKeep)
	if err != nil {
		return sys, err
	}

	sys.Sql.PoolInitSize, err = to32w(bf.DbPoolSize)
	if err != nil {
		return sys, err
	}

	sys.Sql.PoolMaxSize, err = to32w(bf.DbPoolMax)
	if err != nil {
		return sys, err
	}

	sys.Sql.PoolQueueMax, err = to32w(bf.DbPoolQueueMax)
	if err != nil {
		return sys, err
	}

	sys.Sql.PoolQueueTimeout, err = to32w(bf.DbPoolerTimeout)
	if err != nil {
		return sys, err
	}

	return sys, nil
}

func generateMigrationOverrideConfig(r *DeliveryRunning, s *DeliverySecrets, cs *ChefServerRunning) (*dc.AutomateConfig, error) {
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
				Fqdn:        w.String(r.Delivery.FQDN),
				FrontendTls: frontendTLSCreds,
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
					Service: lBServiceConfig,
					Ngx:     lBNginxConfig,
					Proxy:   proxyConfig,
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

	// Return early if we're not migrating Chef Infra Server config
	if cs == nil {
		return cfg, nil
	}

	erchefSys, err := getErchefSettings(cs)
	if err != nil {
		return cfg, errors.Wrap(err, "paring Chef Infra Server oc-erchef settings")
	}

	infraNginxSys, err := getInfraServerNginxSettings(cs)
	if err != nil {
		return cfg, errors.Wrap(err, "paring Chef Infra Server nginx settings")
	}

	bookshelfSys, err := getBookshelfSettings(cs)
	if err != nil {
		return cfg, errors.Wrap(err, "paring Chef Infra Server bookshelf settings")
	}

	bifrostSys, err := getBifrostSettings(cs)
	if err != nil {
		return cfg, errors.Wrap(err, "paring Chef Infra Server oc-bifrost settings")
	}

	cfg.Erchef = &erchef.ConfigRequest{
		V1: &erchef.ConfigRequest_V1{
			Sys: erchefSys,
		},
	}
	cfg.CsNginx = &cs_nginx.ConfigRequest{
		V1: &cs_nginx.ConfigRequest_V1{
			Sys: infraNginxSys,
		},
	}
	cfg.Bookshelf = &bookshelf.ConfigRequest{
		V1: &bookshelf.ConfigRequest_V1{
			Sys: bookshelfSys,
		},
	}
	cfg.Bifrost = &bifrost.ConfigRequest{
		V1: &bifrost.ConfigRequest_V1{
			Sys: bifrostSys,
		},
	}

	return cfg, nil
}

func to32w(in json.Number) (*wrappers.Int32Value, error) {
	i, err := in.Int64()
	if err != nil {
		// If the value is not set we'll return an empty wrapper
		if in.String() == "" {
			return nil, nil
		}

		return nil, err
	}

	return w.Int32(int32(i)), nil
}

func to64w(in json.Number) (*wrappers.Int64Value, error) {
	i, err := in.Int64()
	if err != nil {
		// If the value is not set we'll return an empty wrapper
		if in.String() == "" {
			return nil, nil
		}

		return nil, err
	}

	return w.Int64(i), nil
}
