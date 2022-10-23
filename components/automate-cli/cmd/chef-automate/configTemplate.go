package main

type OpensearchConfig struct {
	Action struct {
		DestructiveRequiresName string `toml:"destructive_requires_name,omitempty"`
	} `toml:"action,omitempty"`
	Bootstrap struct {
		MemoryLock bool `toml:"memory_lock,omitempty"`
	} `toml:"bootstrap,omitempty"`
	Cluster struct {
		Name    string `toml:"name,omitempty"`
		Routing struct {
			Allocation struct {
				AwarenessAttributes            string `toml:"awareness_attributes,omitempty"`
				NodeConcurrentRecoveries       string `toml:"node_concurrent_recoveries,omitempty"`
				NodeInitialPrimariesRecoveries string `toml:"node_initial_primaries_recoveries,omitempty"`
				SameShardHost                  string `toml:"same_shard_host,omitempty"`
			} `toml:"allocation,omitempty"`
		} `toml:"routing,omitempty"`
	} `toml:"cluster,omitempty"`
	Deprecated struct {
		ExternalOs bool `toml:"external_os,omitempty"`
	} `toml:"deprecated,omitempty"`
	Discovery struct {
		MinimumMasterNodes int      `toml:"minimum_master_nodes,omitempty"`
		PingUnicastHosts   []string `toml:"ping_unicast_hosts,omitempty"`
		ZenFdPingTimeout   string   `toml:"zen_fd_ping_timeout,omitempty"`
	} `toml:"discovery,omitempty"`
	Gateway struct {
		ExpectedDataNodes   string `toml:"expected_data_nodes,omitempty"`
		ExpectedMasterNodes string `toml:"expected_master_nodes,omitempty"`
		ExpectedNodes       string `toml:"expected_nodes,omitempty"`
		RecoverAfterNodes   string `toml:"recover_after_nodes,omitempty"`
		RecoverAfterTime    string `toml:"recover_after_time,omitempty"`
	} `toml:"gateway,omitempty"`
	Indices struct {
		Breaker struct {
			FielddataLimit    string `toml:"fielddata_limit,omitempty"`
			FielddataOverhead string `toml:"fielddata_overhead,omitempty"`
			RequestLimit      string `toml:"request_limit,omitempty"`
			RequestOverhead   string `toml:"request_overhead,omitempty"`
			TotalLimit        string `toml:"total_limit,omitempty"`
		} `toml:"breaker,omitempty"`
		Fielddata struct {
			CacheSize string `toml:"cache_size,omitempty"`
		} `toml:"fielddata,omitempty"`
		Recovery struct {
			MaxBytesPerSec string `toml:"max_bytes_per_sec,omitempty"`
		} `toml:"recovery,omitempty"`
	} `toml:"indices,omitempty"`
	Logger struct {
		Level string `toml:"level,omitempty"`
	} `toml:"logger,omitempty"`
	Network struct {
		Host string `toml:"host,omitempty"`
		Port int    `toml:"port,omitempty"`
	} `toml:"network,omitempty"`
	Node struct {
		Data                 bool   `toml:"data,omitempty"`
		Master               bool   `toml:"master,omitempty"`
		MaxLocalStorageNodes int    `toml:"max_local_storage_nodes,omitempty"`
		Name                 string `toml:"name,omitempty"`
		RackID               string `toml:"rack_id,omitempty"`
		Zone                 string `toml:"zone,omitempty"`
	} `toml:"node,omitempty"`
	OpensearchAuth struct {
		AdminPassword  string `toml:"admin_password,omitempty"`
		AdminUsername  string `toml:"admin_username,omitempty"`
		HashedPassword string `toml:"hashed_password,omitempty"`
	} `toml:"opensearch_auth,omitempty"`
	Path struct {
		Data string `toml:"data,omitempty"`
		Logs string `toml:"logs,omitempty"`
		Repo string `toml:"repo,omitempty"`
	} `toml:"path,omitempty"`
	Plugins struct {
		Security struct {
			AllowDefaultInitSecurityindex       bool   `toml:"allow_default_init_securityindex,omitempty"`
			AllowUnsafeDemocertificates         bool   `toml:"allow_unsafe_democertificates,omitempty"`
			CheckSnapshotRestoreWritePrivileges bool   `toml:"check_snapshot_restore_write_privileges,omitempty"`
			EnableSnapshotRestorePrivilege      bool   `toml:"enable_snapshot_restore_privilege,omitempty"`
			NodesDn                             string `toml:"nodes_dn,omitempty"`
			Audit                               struct {
				Type string `toml:"type,omitempty"`
			} `toml:"audit,omitempty"`
			Authcz struct {
				AdminDn string `toml:"admin_dn,omitempty"`
			} `toml:"authcz,omitempty"`
			Restapi struct {
				RolesEnabled string `toml:"roles_enabled,omitempty"`
			} `toml:"restapi,omitempty"`
			Ssl struct {
				HTTP struct {
					Enabled               bool   `toml:"enabled,omitempty"`
					PemcertFilepath       string `toml:"pemcert_filepath,omitempty"`
					PemkeyFilepath        string `toml:"pemkey_filepath,omitempty"`
					PemtrustedcasFilepath string `toml:"pemtrustedcas_filepath,omitempty"`
				} `toml:"http,omitempty"`
				Transport struct {
					EnforceHostnameVerification bool   `toml:"enforce_hostname_verification,omitempty"`
					PemcertFilepath             string `toml:"pemcert_filepath,omitempty"`
					PemkeyFilepath              string `toml:"pemkey_filepath,omitempty"`
					PemtrustedcasFilepath       string `toml:"pemtrustedcas_filepath,omitempty"`
					ResolveHostname             bool   `toml:"resolve_hostname,omitempty"`
				} `toml:"transport,omitempty"`
			} `toml:"ssl,omitempty"`
			SystemIndices struct {
				CloudAwsSigner string `toml:"cloud_aws_signer,omitempty"`
				Enabled        bool   `toml:"enabled,omitempty"`
				Indices        string `toml:"indices,omitempty"`
			} `toml:"system_indices,omitempty"`
		} `toml:"security,omitempty"`
	} `toml:"plugins,omitempty"`
	Runtime struct {
		EsJavaOpts                     string `toml:"es_java_opts,omitempty"`
		EsStartupSleepTime             string `toml:"es_startup_sleep_time,omitempty"`
		G1ReservePercent               string `toml:"g1ReservePercent,omitempty"`
		InitiatingHeapOccupancyPercent string `toml:"initiatingHeapOccupancyPercent,omitempty"`
		MaxHeapsize                    string `toml:"maxHeapsize,omitempty"`
		MaxLockedMemory                string `toml:"max_locked_memory,omitempty"`
		MaxOpenFiles                   string `toml:"max_open_files,omitempty"`
		MinHeapsize                    string `toml:"minHeapsize,omitempty"`
	} `toml:"runtime,omitempty"`
	S3 struct {
		Client struct {
			Default struct {
				Endpoint           string `toml:"endpoint,omitempty"`
				MaxRetries         string `toml:"max_retries,omitempty"`
				Protocol           string `toml:"protocol,omitempty"`
				ReadTimeout        string `toml:"read_timeout,omitempty"`
				UseThrottleRetries bool   `toml:"use_throttle_retries,omitempty"`
			} `toml:"default,omitempty"`
		} `toml:"client,omitempty"`
	} `toml:"s3,omitempty"`
	TLS struct {
		AdminCert string `toml:"admin_cert,omitempty"`
		AdminKey  string `toml:"admin_key,omitempty"`
		RootCA    string `toml:"rootCA,omitempty"`
		SslCert   string `toml:"ssl_cert,omitempty"`
		SslKey    string `toml:"ssl_key,omitempty"`
	} `toml:"tls,omitempty"`
	Transport struct {
		Port int `toml:"port,omitempty"`
	} `toml:"transport,omitempty"`
}

type PostgresqlConfig struct {
	CheckpointCompletionTarget float64 `toml:"checkpoint_completion_target,omitempty"`
	CheckpointTimeout          string  `toml:"checkpoint_timeout,omitempty"`
	Host                       string  `toml:"host,omitempty"`
	LogLevel                   string  `toml:"log_level,omitempty"`
	LogLinePrefix              string  `toml:"log_line_prefix,omitempty"`
	LoggingCollector           string  `toml:"logging_collector,omitempty"`
	MaxConnections             int     `toml:"max_connections,omitempty"`
	MaxLocksPerTransaction     int     `toml:"max_locks_per_transaction,omitempty"`
	MaxWalSize                 string  `toml:"max_wal_size,omitempty"`
	MinWalSize                 string  `toml:"min_wal_size,omitempty"`
	Port                       int     `toml:"port,omitempty"`
	PrintDbStatistics          bool    `toml:"print_db_statistics,omitempty"`
	WalKeepSize                int     `toml:"wal_keep_size,omitempty"`
	PgDump                     struct {
		Enable bool   `toml:"enable,omitempty"`
		Path   string `toml:"path,omitempty"`
	} `toml:"pg_dump,omitempty"`
	Replication struct {
		LagHealthThreshold         int    `toml:"lag_health_threshold,omitempty"`
		MaxReplayLagBeforeRestartS int    `toml:"max_replay_lag_before_restart_s,omitempty"`
		Name                       string `toml:"name,omitempty"`
		Password                   string `toml:"password,omitempty"`
	} `toml:"replication,omitempty"`
	S3 struct {
		Client struct {
			Default struct {
				ReadTimeout string `toml:"read_timeout,omitempty"`
			} `toml:"default,omitempty"`
		} `toml:"client,omitempty"`
	} `toml:"s3,omitempty"`
	Ssl struct {
		Enable     bool   `toml:"enable,omitempty"`
		IssuerCert string `toml:"issuer_cert,omitempty"`
		SslCert    string `toml:"ssl_cert,omitempty"`
		SslKey     string `toml:"ssl_key,omitempty"`
		TLSCiphers string `toml:"tls_ciphers,omitempty"`
	} `toml:"ssl,omitempty"`
	Superuser struct {
		Name     string `toml:"name,omitempty"`
		Password string `toml:"password,omitempty"`
	} `toml:"superuser,omitempty"`
	WalArchive struct {
		Enable bool   `toml:"enable,omitempty"`
		Path   string `toml:"path,omitempty"`
	} `toml:"wal_archive,omitempty"`
}
