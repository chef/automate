package a1upgrade

import (
	"encoding/json"
	"os"
	"path/filepath"
)

// DeliveryRunning represents the data we're extracting from the A1
// /etc/delivery/delivery-running.json configuration file
type DeliveryRunning struct {
	Delivery struct {
		FQDN      string `json:"fqdn"`
		IPVersion string `json:"ip_version"`
		Backup    struct {
			S3AccessKeyID          string `json:"access_key_id"`
			S3Bucket               string `json:"bucket"`
			S3SecretAccessKey      string `json:"secret_access_key"`
			S3Region               string `json:"region"`
			S3ServerSideEncryption string `json:"server_side_encryption"`
			S3SSECustomerAlgorithm string `json:"sse_customer_algorithm"`
			S3SSECustomerKey       string `json:"sse_customer_key"`
			S3SSECustomerKeyMD5    string `json:"sse_customer_key_md5"`
			S3SSEKMSKeyID          string `json:"ssekms_key_id"`
			Elasticsearch          struct {
				S3AccessKeyID          string `json:"access_key_id"`
				S3Bucket               string `json:"bucket"`
				S3Region               string `json:"region"`
				S3SecretAccessKey      string `json:"secret_access_key"`
				S3ServerSideEncryption string `json:"server_side_encryption"`
				Location               string `json:"location"`
				Type                   string `json:"type"`
			} `json:"elasticsearch"`
			Location  string `json:"location"`
			Type      string `json:"type"`
			Retention struct {
				Enabled      bool        `json:"enabled"`
				MaxArchives  json.Number `json:"max_archives"`
				MaxSnapshots json.Number `json:"max_snapshots"`
				// I'm not sure if we'll honor this but we'll grab it anyway
				Notation string `json:"notation"` // eg: "0 0 * * *"
			} `json:"cron"`
		}
		Compliance struct {
			MarketPath   string `json:"market_path"`
			ProfilesPath string `json:"profiles_path"`
			// I'm not sure we care about log rotation since we're relying
			// on the systemd journal. I've added it because it was in the
			// spec but we might be able to drop these later.
			LogRotation struct {
				MaxBytes       json.Number `json:"file_maxbytes"`
				RetentionCount json.Number `json:"num_to_keep"`
			} `json:"log_rotation"`
		} `json:"compliance_profiles"`
		DataCollector struct {
			Token string `json:"token"`
		} `json:"data_collector"`
		Delivery struct {
			GitRepos           string      `json:"git_repos"`
			LDAPHosts          []string    `json:"ldap_hosts"`
			LDAPPort           json.Number `json:"ldap_port"`
			LDAPTimeout        json.Number `json:"ldap_timeout"`
			LDAPBaseDN         string      `json:"ldap_base_dn"`
			LDAPBindDN         string      `json:"ldap_bind_dn"`
			LDAPBindDNPassword string      `json:"ldap_bind_dn_password"`
			LDAPEncryption     string      `json:"ldap_encryption"`
			LDAPLogin          string      `json:"ldap_attr_login"`
			LDAPMail           string      `json:"ldap_attr_mail"`
			Proxy              struct {
				Host     string      `json:"host"`
				Port     json.Number `json:"port"`
				User     string      `json:"user"`
				Password string      `json:"password"`
				NoProxy  []string    `json:"no_proxy"`
			} `json:"proxy"`
			PrimaryIp         string                       `json:"primary_ip"`
			StandbyIp         string                       `json:"standby_ip"`
			NoSSLVerification []string                     `json:"no_ssl_verification"`
			SSLCertificates   map[string]map[string]string `json:"ssl_certificates"`
			SecretsKey        string                       `json:"secrets_key"`
			ErlCookie         string                       `json:"erl_cookie"`
		} `json:"delivery"`
		Elasticsearch DeliveryRunningElasticsearch `json:"elasticsearch"`
		FIPS          struct {
			Enabled bool `json:"enable"`
		} `json:"fips"`
		Nginx struct {
			AccessLog struct {
				BufferSize string `json:"buffer_size"`
				FlushTime  string `json:"flush_time"`
			} `json:"access_log"`
			ClientMaxBodySize        string      `json:"client_max_body_size"`
			Dir                      string      `json:"dir"`
			GZip                     string      `json:"gzip"`
			GZipCompLevel            string      `json:"gzip_comp_level"`
			GZipHTTPVersion          string      `json:"gzip_http_version"`
			GZipProxied              string      `json:"gzip_proxied"`
			GZipTypes                []string    `json:"gzip_types"`
			HTTPSPort                json.Number `json:"ssl_port"`
			HTTPPort                 json.Number `json:"non_ssl_port"`
			KeepaliveTimeout         json.Number `json:"keepalive_timeout"`
			KeepaliveRequests        json.Number `json:"keepalive_requests"`
			LargeClientHeaderBuffers struct {
				Size   string      `json:"size"`
				Number json.Number `json:"number"`
			} `json:"large_client_header_buffers"`
			LogRotation struct {
				MaxBytes       json.Number `json:"file_maxbytes"`
				RetentionCount json.Number `json:"num_to_keep"`
			} `json:"log_rotation"`
			MultiAccept           string      `json:"multi_accept"`
			Sendfile              string      `json:"sendfile"`
			SSLCiphers            string      `json:"ssl_ciphers"`
			SSLProtocols          string      `json:"ssl_protocols"`
			TCPNoPush             string      `json:"tcp_nopush"`
			TCPNoDelay            string      `json:"tcp_nodelay"`
			WorkerConnections     json.Number `json:"worker_connections"`
			WorkerProcesses       json.Number `json:"worker_processes"`
			WorkerProcessorMethod string      `json:"worker_processor_method"`
		} `json:"nginx"`
		Notifications struct {
			RuleStore   string `json:"rule_store_file"`
			LogRotation struct {
				MaxBytes       json.Number `json:"file_maxbytes"`
				RetentionCount json.Number `json:"num_to_keep"`
			} `json:"log_rotation"`
		} `json:"notifications"`
		PostgreSQL DeliveryRunningPostgreSQL `json:"postgresql"`
		Reaper     struct {
			RetentionPeriod       json.Number `json:"retention_period_in_days"`
			Threshold             json.Number `json:"free_space_threshold_percent"`
			Enabled               bool        `json:"enable"`
			Mode                  string      `json:"mode"`
			ArchiveDestination    string      `json:"archive_destination"`
			ArchiveFilesystemPath string      `json:"archive_filesystem_path"`
		} `json:"reaper"`
		Insights struct {
			DataDirectory string `json:"data_directory"`
		} `json:"insights"`
	} `json:"delivery"`
}

type DeliveryRunningElasticsearch struct {
	ClusterURLS     []string    `json:"urls"`
	MaxOpenFiles    json.Number `json:"max_open_files"`
	MaxMapCount     json.Number `json:"max_map_count"`
	MaxLockedMemory string      `json:"max_locked_memory"`
	HeapSize        string      `json:"memory"`
	NewMemory       string      `json:"new_memory_size"`
	NginxProxyURL   string      `json:"nginx_proxy_url"`
	LogRotation     struct {
		MaxBytes       json.Number `json:"file_maxbytes"`
		RetentionCount json.Number `json:"num_to_keep"`
	} `json:"log_rotation"`
	RepoPath struct {
		Data   string `json:"data"`
		Logs   string `json:"logs"`
		Backup string `json:"repo"`
	} `json:"path"`
}

type DeliveryRunningPostgreSQL struct {
	// Vip and SuperuserUsername are used only during upgrade from A1.
	// We deliberately do not generate entries for them in the A2 config.
	CheckpointSegments         json.Number `json:"checkpoint_segments"`
	CheckpointTimeout          string      `json:"checkpoint_timeout"`
	CheckpointCompletionTarget json.Number `json:"checkpoint_completion_target"`
	CheckpointWarning          string      `json:"checkpoint_warning"`
	DataDirectory              string      `json:"data_dir"`
	EffectiveCacheSize         string      `json:"effective_cache_size"`
	LogRotation                struct {
		MaxBytes       json.Number `json:"file_maxbytes"`
		RetentionCount json.Number `json:"num_to_keep"`
	} `json:"log_rotation"`
	ListenAddress          string      `json:"listen_address"`
	MaxConnections         json.Number `json:"max_connections"`
	MD5AuthCIDRAddresses   []string    `json:"md5_auth_cidr_addresses"`
	Port                   json.Number `json:"port"`
	SharedBuffers          string      `json:"shared_buffers"`
	SHMMAX                 json.Number `json:"shmmax"`
	SHMALL                 json.Number `json:"shmall"`
	SuperuserUsername      string      `json:"superuser_username"`
	SuperuserEnable        bool        `json:"superuser_enable"`
	TrustAuthCIDRAddresses []string    `json:"trust_auth_cidr_addresses"`
	Username               string      `json:"username"`
	WorkMem                string      `json:"work_mem"`
	Vip                    string      `json:"vip"`
}

// DeliverySecrets represents the data we're extracting from the A1
// /etc/delivery/delivery-secrets.json secrets file.
type DeliverySecrets struct {
	Delivery struct {
		SQLPassword     string `json:"sql_password"`
		SQLROPassword   string `json:"sql_ro_password"`
		SQLREPLPassword string `json:"sql_repl_password"`
		SecretsKey      string `json:"secrets_key"`
	} `json:"delivery"`
	Postgresql struct {
		SuperuserPassword string `json:"superuser_password"`
	} `json:"postgresql"`
	RabbitMQ struct {
		Password           string `json:"password"`
		ManagementPassword string `json:"management_password"`
	} `json:"rabbitmq"`
}

type ChefServerRunning struct {
	PrivateChef PrivateChef `json:"private_chef"`
}

type PrivateChef struct {
	OpscodeErchef OpscodeErchef `json:"opscode-erchef"`
	OpscodeSolr4  OpscodeSolr4  `json:"opscode-solr4"`
	Postgresql    CSRPostgreSQL `json:"postgresql"`
	Bookshelf     Bookshelf     `json:"bookshelf"`
	OcID          OcID          `json:"oc_id"`
}

type OpscodeErchef struct {
	SearchProvider  string `json:"search_provider"`
	SearchQueueMode string `json:"search_queue_mode"`
}

type OpscodeSolr4 struct {
	Enable      bool   `json:"enable"`
	External    bool   `json:"external"`
	ExternalURL string `json:"external_url"`
}

type CSRPostgreSQL struct {
	Enable   bool        `json:"enable"`
	External bool        `json:"external"`
	Vip      string      `json:"vip"`
	Port     json.Number `json:"port"`
}

type Bookshelf struct {
	Enable      bool   `json:"enable"`
	StorageType string `json:"storage_type"`
}

type OcID struct {
	Applications map[string]interface{} `json:"applications"`
}

// A1Config represents the A1 configuration
type A1Config struct {
	DeliveryRunningPath   string
	DeliverySecretsPath   string
	ChefServerRunningPath string
	DeliveryRunning       *DeliveryRunning
	DeliverySecrets       *DeliverySecrets
	ChefServerRunning     *ChefServerRunning
}

// NewA1Config returns a new default instance of an A1Config.
func NewA1Config() *A1Config {
	return &A1Config{
		DeliveryRunningPath:   "/etc/delivery/delivery-running.json",
		DeliverySecretsPath:   "/etc/delivery/delivery-secrets.json",
		ChefServerRunningPath: "/etc/opscode/chef-server-running.json",
		DeliveryRunning:       &DeliveryRunning{},
		DeliverySecrets:       &DeliverySecrets{},
		ChefServerRunning:     &ChefServerRunning{},
	}
}

// LoadDeliveryRunning marshals the delivery-running.json into a DeliveryRunning
// struct.
func (c *A1Config) LoadDeliveryRunning() error {
	return load(c.DeliveryRunningPath, c.DeliveryRunning)
}

// LoadDeliverySecrets marshals the delivery-secrets.json into a DeliverySecrets
// struct.
func (c *A1Config) LoadDeliverySecrets() error {
	return load(c.DeliverySecretsPath, c.DeliverySecrets)
}

// LoadChefServerRunning marshals chef-server-running.json into the
// ChefServerRunning struct
func (c *A1Config) LoadChefServerRunning() error {
	return load(c.ChefServerRunningPath, c.ChefServerRunning)
}

func load(path string, obj interface{}) error {
	path, err := filepath.Abs(path)
	if err != nil {
		return err
	}
	reader, err := os.Open(path)
	if err != nil {
		return err
	}
	defer reader.Close()

	return json.NewDecoder(reader).Decode(obj)
}
