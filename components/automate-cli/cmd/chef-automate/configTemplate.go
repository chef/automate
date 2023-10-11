package main

import (
	"google.golang.org/protobuf/runtime/protoimpl"
	"google.golang.org/protobuf/types/known/wrapperspb"
)

type OpensearchConfig struct {
	Global *struct {
		V1 *struct {
			Log *struct {
				state         protoimpl.MessageState
				sizeCache     protoimpl.SizeCache
				unknownFields protoimpl.UnknownFields

				Level                *wrapperspb.StringValue `protobuf:"bytes,1,opt,name=level,proto3" json:"level,omitempty" toml:"level,omitempty" mapstructure:"level,omitempty"`
				Format               *wrapperspb.StringValue `protobuf:"bytes,2,opt,name=format,proto3" json:"format,omitempty" toml:"format,omitempty" mapstructure:"format,omitempty"`
				RedirectSysLog       *wrapperspb.BoolValue   `protobuf:"bytes,3,opt,name=redirect_sys_log,json=redirectSysLog,proto3" json:"redirect_sys_log,omitempty" toml:"redirect_sys_log,omitempty" mapstructure:"redirect_sys_log,omitempty"`
				RedirectLogFilePath  *wrapperspb.StringValue `protobuf:"bytes,4,opt,name=redirect_log_file_path,json=redirectLogFilePath,proto3" json:"redirect_log_file_path,omitempty" toml:"redirect_log_file_path,omitempty" mapstructure:"redirect_log_file_path,omitempty"`
				CompressRotatedLogs  *wrapperspb.BoolValue   `protobuf:"bytes,5,opt,name=compress_rotated_logs,json=compressRotatedLogs,proto3" json:"compress_rotated_logs,omitempty" toml:"compress_rotated_logs,omitempty" mapstructure:"compress_rotated_logs,omitempty"`
				MaxSizeRotateLogs    *wrapperspb.StringValue `protobuf:"bytes,6,opt,name=max_size_rotate_logs,json=maxSizeRotateLogs,proto3" json:"max_size_rotate_logs,omitempty" toml:"max_size_rotate_logs,omitempty" mapstructure:"max_size_rotate_logs,omitempty"`
				MaxNumberRotatedLogs *wrapperspb.Int32Value  `protobuf:"bytes,7,opt,name=max_number_rotated_logs,json=maxNumberRotatedLogs,proto3" json:"max_number_rotated_logs,omitempty" toml:"max_number_rotated_logs,omitempty" mapstructure:"max_number_rotated_logs,omitempty"`
			}
		}
	}
	Action *struct {
		DestructiveRequiresName string `protobuf:"bytes,,opt,name=destructive_requires_name,proto3" toml:"destructive_requires_name,omitempty" json:"destructive_requires_name,omitempty" mapstructure:"destructive_requires_name,omitempty"`
	} `protobuf:"bytes,,opt,name=action,proto3" toml:"action,omitempty" json:"action,omitempty" mapstructure:"action,omitempty"`
	Bootstrap *struct {
		MemoryLock *bool `protobuf:"bytes,,opt,name=memory_lock,proto3" toml:"memory_lock,omitempty" json:"memory_lock,omitempty" mapstructure:"memory_lock,omitempty"`
	} `protobuf:"bytes,,opt,name=bootstrap,proto3" toml:"bootstrap,omitempty" json:"bootstrap,omitempty" mapstructure:"bootstrap,omitempty"`
	Cluster *struct {
		Name             string `protobuf:"bytes,,opt,name=name,proto3" toml:"name,omitempty" json:"name,omitempty" mapstructure:"name,omitempty"`
		MaxShardsPerNode string `protobuf:"bytes,,opt,name=max_shards_per_node,proto3" toml:"max_shards_per_node,omitempty" json:"max_shards_per_node,omitempty" mapstructure:"max_shards_per_node,omitempty"`
		Routing          struct {
			Allocation struct {
				AwarenessAttributes            string `protobuf:"bytes,,opt,name=awareness_attributes,proto3" toml:"awareness_attributes,omitempty" json:"awareness_attributes,omitempty" mapstructure:"awareness_attributes,omitempty"`
				NodeConcurrentRecoveries       string `protobuf:"bytes,,opt,name=node_concurrent_recoveries,proto3" toml:"node_concurrent_recoveries,omitempty" json:"node_concurrent_recoveries,omitempty" mapstructure:"node_concurrent_recoveries,omitempty"`
				NodeInitialPrimariesRecoveries string `protobuf:"bytes,,opt,name=node_initial_primaries_recoveries,proto3" toml:"node_initial_primaries_recoveries,omitempty" json:"node_initial_primaries_recoveries,omitempty" mapstructure:"node_initial_primaries_recoveries,omitempty"`
				SameShardHost                  string `protobuf:"bytes,,opt,name=same_shard_host,proto3" toml:"same_shard_host,omitempty" json:"same_shard_host,omitempty" mapstructure:"same_shard_host,omitempty"`
			} `protobuf:"bytes,,opt,name=allocation,proto3" toml:"allocation,omitempty" json:"allocation,omitempty" mapstructure:"allocation,omitempty"`
		} `protobuf:"bytes,,opt,name=routing,proto3" toml:"routing,omitempty" json:"routing,omitempty" mapstructure:"routing,omitempty"`
	} `protobuf:"bytes,,opt,name=cluster,proto3" toml:"cluster,omitempty" json:"cluster,omitempty" mapstructure:"cluster,omitempty"`
	Deprecated *struct {
		ExternalOs *bool `protobuf:"bytes,,opt,name=external_os,proto3" toml:"external_os,omitempty" json:"external_os,omitempty" mapstructure:"external_os,omitempty"`
	} `protobuf:"bytes,,opt,name=deprecated,proto3" toml:"deprecated,omitempty" json:"deprecated,omitempty" mapstructure:"deprecated,omitempty"`
	Discovery *struct {
		MinimumMasterNodes int      `protobuf:"bytes,,opt,name=minimum_master_nodes,proto3" toml:"minimum_master_nodes,omitzero" json:"minimum_master_nodes,omitzero" mapstructure:"minimum_master_nodes,omitzero"`
		PingUnicastHosts   []string `protobuf:"bytes,,opt,name=ping_unicast_hosts,proto3" toml:"ping_unicast_hosts,omitempty" json:"ping_unicast_hosts,omitempty" mapstructure:"ping_unicast_hosts,omitempty"`
		ZenFdPingTimeout   string   `protobuf:"bytes,,opt,name=zen_fd_ping_timeout,proto3" toml:"zen_fd_ping_timeout,omitempty" json:"zen_fd_ping_timeout,omitempty" mapstructure:"zen_fd_ping_timeout,omitempty"`
	} `protobuf:"bytes,,opt,name=discovery,proto3" toml:"discovery,omitempty" json:"discovery,omitempty" mapstructure:"discovery,omitempty"`
	Gateway *struct {
		ExpectedDataNodes   string `protobuf:"bytes,,opt,name=expected_data_nodes,proto3" toml:"expected_data_nodes,omitempty" json:"expected_data_nodes,omitempty" mapstructure:"expected_data_nodes,omitempty"`
		ExpectedMasterNodes string `protobuf:"bytes,,opt,name=expected_master_nodes,proto3" toml:"expected_master_nodes,omitempty" json:"expected_master_nodes,omitempty" mapstructure:"expected_master_nodes,omitempty"`
		ExpectedNodes       string `protobuf:"bytes,,opt,name=expected_nodes,proto3" toml:"expected_nodes,omitempty" json:"expected_nodes,omitempty" mapstructure:"expected_nodes,omitempty"`
		RecoverAfterNodes   string `protobuf:"bytes,,opt,name=recover_after_nodes,proto3" toml:"recover_after_nodes,omitempty" json:"recover_after_nodes,omitempty" mapstructure:"recover_after_nodes,omitempty"`
		RecoverAfterTime    string `protobuf:"bytes,,opt,name=recover_after_time,proto3" toml:"recover_after_time,omitempty" json:"recover_after_time,omitempty" mapstructure:"recover_after_time,omitempty"`
	} `protobuf:"bytes,,opt,name=gateway,proto3" toml:"gateway,omitempty" json:"gateway,omitempty" mapstructure:"gateway,omitempty"`
	Indices *struct {
		Breaker struct {
			FielddataLimit    string `protobuf:"bytes,,opt,name=fielddata_limit,proto3" toml:"fielddata_limit,omitempty" json:"fielddata_limit,omitempty" mapstructure:"fielddata_limit,omitempty"`
			FielddataOverhead string `protobuf:"bytes,,opt,name=fielddata_overhead,proto3" toml:"fielddata_overhead,omitempty" json:"fielddata_overhead,omitempty" mapstructure:"fielddata_overhead,omitempty"`
			RequestLimit      string `protobuf:"bytes,,opt,name=request_limit,proto3" toml:"request_limit,omitempty" json:"request_limit,omitempty" mapstructure:"request_limit,omitempty"`
			RequestOverhead   string `protobuf:"bytes,,opt,name=request_overhead,proto3" toml:"request_overhead,omitempty" json:"request_overhead,omitempty" mapstructure:"request_overhead,omitempty"`
			TotalLimit        string `protobuf:"bytes,,opt,name=total_limit,proto3" toml:"total_limit,omitempty" json:"total_limit,omitempty" mapstructure:"total_limit,omitempty"`
		} `protobuf:"bytes,,opt,name=breaker,proto3" toml:"breaker,omitempty" json:"breaker,omitempty" mapstructure:"breaker,omitempty"`
		Fielddata struct {
			CacheSize string `protobuf:"bytes,,opt,name=cache_size,proto3" toml:"cache_size,omitempty" json:"cache_size,omitempty" mapstructure:"cache_size,omitempty"`
		} `protobuf:"bytes,,opt,name=fielddata,proto3" toml:"fielddata,omitempty" json:"fielddata,omitempty" mapstructure:"fielddata,omitempty"`
		Recovery struct {
			MaxBytesPerSec string `protobuf:"bytes,,opt,name=max_bytes_per_sec,proto3" toml:"max_bytes_per_sec,omitempty" json:"max_bytes_per_sec,omitempty" mapstructure:"max_bytes_per_sec,omitempty"`
		} `protobuf:"bytes,,opt,name=recovery,proto3" toml:"recovery,omitempty" json:"recovery,omitempty" mapstructure:"recovery,omitempty"`
	} `protobuf:"bytes,,opt,name=indices,proto3" toml:"indices,omitempty" json:"indices,omitempty" mapstructure:"indices,omitempty"`
	Logger *struct {
		Level string `protobuf:"bytes,,opt,name=level,proto3" toml:"level,omitempty" json:"level,omitempty" mapstructure:"level,omitempty"`
	} `protobuf:"bytes,,opt,name=logger,proto3" toml:"logger,omitempty" json:"logger,omitempty" mapstructure:"logger,omitempty"`
	Network *struct {
		Host string `protobuf:"bytes,,opt,name=host,proto3" toml:"host,omitempty" json:"host,omitempty" mapstructure:"host,omitempty"`
		Port int    `protobuf:"bytes,,opt,name=port,proto3" toml:"port,omitzero" json:"port,omitzero" mapstructure:"port,omitzero"`
	} `protobuf:"bytes,,opt,name=network,proto3" toml:"network,omitempty" json:"network,omitempty" mapstructure:"network,omitempty"`
	Node *struct {
		Data                 bool   `protobuf:"bytes,,opt,name=data,proto3" toml:"data,omitempty" json:"data,omitempty" mapstructure:"data,omitempty"`
		Master               bool   `protobuf:"bytes,,opt,name=master,proto3" toml:"master,omitempty" json:"master,omitempty" mapstructure:"master,omitempty"`
		MaxLocalStorageNodes int    `protobuf:"bytes,,opt,name=max_local_storage_nodes,proto3" toml:"max_local_storage_nodes,omitzero" json:"max_local_storage_nodes,omitzero" mapstructure:"max_local_storage_nodes,omitzero"`
		Name                 string `protobuf:"bytes,,opt,name=name,proto3" toml:"name,omitempty" json:"name,omitempty" mapstructure:"name,omitempty"`
		RackID               string `protobuf:"bytes,,opt,name=rack_id,proto3" toml:"rack_id,omitempty" json:"rack_id,omitempty" mapstructure:"rack_id,omitempty"`
		Zone                 string `protobuf:"bytes,,opt,name=zone,proto3" toml:"zone,omitempty" json:"zone,omitempty" mapstructure:"zone,omitempty"`
	} `protobuf:"bytes,,opt,name=node,proto3" toml:"node,omitempty" json:"node,omitempty" mapstructure:"node,omitempty"`
	OpensearchAuth *struct {
		AdminPassword  string `protobuf:"bytes,,opt,name=admin_password,proto3" toml:"admin_password,omitempty" json:"admin_password,omitempty" mapstructure:"admin_password,omitempty"`
		AdminUsername  string `protobuf:"bytes,,opt,name=admin_username,proto3" toml:"admin_username,omitempty" json:"admin_username,omitempty" mapstructure:"admin_username,omitempty"`
		HashedPassword string `protobuf:"bytes,,opt,name=hashed_password,proto3" toml:"hashed_password,omitempty" json:"hashed_password,omitempty" mapstructure:"hashed_password,omitempty"`
	} `protobuf:"bytes,,opt,name=opensearch_auth,proto3" toml:"opensearch_auth,omitempty" json:"opensearch_auth,omitempty" mapstructure:"opensearch_auth,omitempty"`
	Path *struct {
		Data string `protobuf:"bytes,,opt,name=data,proto3" toml:"data,omitempty" json:"data,omitempty" mapstructure:"data,omitempty"`
		Logs string `protobuf:"bytes,,opt,name=logs,proto3" toml:"logs,omitempty" json:"logs,omitempty" mapstructure:"logs,omitempty"`
		Repo string `protobuf:"bytes,,opt,name=repo,proto3" toml:"repo,omitempty" json:"repo,omitempty" mapstructure:"repo,omitempty"`
	} `protobuf:"bytes,,opt,name=path,proto3" toml:"path,omitempty" json:"path,omitempty" mapstructure:"path,omitempty"`
	Plugins *struct {
		Security struct {
			AllowDefaultInitSecurityindex       bool   `protobuf:"bytes,,opt,name=allow_default_init_securityindex,proto3" toml:"allow_default_init_securityindex,omitempty" json:"allow_default_init_securityindex,omitempty" mapstructure:"allow_default_init_securityindex,omitempty"`
			AllowUnsafeDemocertificates         bool   `protobuf:"bytes,,opt,name=allow_unsafe_democertificates,proto3" toml:"allow_unsafe_democertificates,omitempty" json:"allow_unsafe_democertificates,omitempty" mapstructure:"allow_unsafe_democertificates,omitempty"`
			CheckSnapshotRestoreWritePrivileges bool   `protobuf:"bytes,,opt,name=check_snapshot_restore_write_privileges,proto3" toml:"check_snapshot_restore_write_privileges,omitempty" json:"check_snapshot_restore_write_privileges,omitempty" mapstructure:"check_snapshot_restore_write_privileges,omitempty"`
			EnableSnapshotRestorePrivilege      bool   `protobuf:"bytes,,opt,name=enable_snapshot_restore_privilege,proto3" toml:"enable_snapshot_restore_privilege,omitempty" json:"enable_snapshot_restore_privilege,omitempty" mapstructure:"enable_snapshot_restore_privilege,omitempty"`
			NodesDn                             string `protobuf:"bytes,,opt,name=nodes_dn,proto3" toml:"nodes_dn,omitempty" json:"nodes_dn,omitempty" mapstructure:"nodes_dn,omitempty"`
			Audit                               *struct {
				Type string `protobuf:"bytes,,opt,name=type,proto3" toml:"type,omitempty" json:"type,omitempty" mapstructure:"type,omitempty"`
			} `protobuf:"bytes,,opt,name=audit,proto3" toml:"audit,omitempty" json:"audit,omitempty" mapstructure:"audit,omitempty"`
			Authcz struct {
				AdminDn string `protobuf:"bytes,,opt,name=admin_dn,proto3" toml:"admin_dn,omitempty" json:"admin_dn,omitempty" mapstructure:"admin_dn,omitempty"`
			} `protobuf:"bytes,,opt,name=authcz,proto3" toml:"authcz,omitempty" json:"authcz,omitempty" mapstructure:"authcz,omitempty"`
			Restapi *struct {
				RolesEnabled string `protobuf:"bytes,,opt,name=roles_enabled,proto3" toml:"roles_enabled,omitempty" json:"roles_enabled,omitempty" mapstructure:"roles_enabled,omitempty"`
			} `protobuf:"bytes,,opt,name=restapi,proto3" toml:"restapi,omitempty" json:"restapi,omitempty" mapstructure:"restapi,omitempty"`
			Ssl struct {
				HTTP *struct {
					Enabled               bool   `protobuf:"bytes,,opt,name=enabled,proto3" toml:"enabled,omitempty" json:"enabled,omitempty" mapstructure:"enabled,omitempty"`
					PemcertFilepath       string `protobuf:"bytes,,opt,name=pemcert_filepath,proto3" toml:"pemcert_filepath,omitempty" json:"pemcert_filepath,omitempty" mapstructure:"pemcert_filepath,omitempty"`
					PemkeyFilepath        string `protobuf:"bytes,,opt,name=pemkey_filepath,proto3" toml:"pemkey_filepath,omitempty" json:"pemkey_filepath,omitempty" mapstructure:"pemkey_filepath,omitempty"`
					PemtrustedcasFilepath string `protobuf:"bytes,,opt,name=pemtrustedcas_filepath,proto3" toml:"pemtrustedcas_filepath,omitempty" json:"pemtrustedcas_filepath,omitempty" mapstructure:"pemtrustedcas_filepath,omitempty"`
				} `protobuf:"bytes,,opt,name=http,proto3" toml:"http,omitempty" json:"http,omitempty" mapstructure:"http,omitempty"`
				Transport struct {
					EnforceHostnameVerification *bool  `protobuf:"bytes,,opt,name=enforce_hostname_verification,proto3" toml:"enforce_hostname_verification,omitempty" json:"enforce_hostname_verification,omitempty" mapstructure:"enforce_hostname_verification,omitempty"`
					PemcertFilepath             string `protobuf:"bytes,,opt,name=pemcert_filepath,proto3" toml:"pemcert_filepath,omitempty" json:"pemcert_filepath,omitempty" mapstructure:"pemcert_filepath,omitempty"`
					PemkeyFilepath              string `protobuf:"bytes,,opt,name=pemkey_filepath,proto3" toml:"pemkey_filepath,omitempty" json:"pemkey_filepath,omitempty" mapstructure:"pemkey_filepath,omitempty"`
					PemtrustedcasFilepath       string `protobuf:"bytes,,opt,name=pemtrustedcas_filepath,proto3" toml:"pemtrustedcas_filepath,omitempty" json:"pemtrustedcas_filepath,omitempty" mapstructure:"pemtrustedcas_filepath,omitempty"`
					ResolveHostname             *bool  `protobuf:"bytes,,opt,name=resolve_hostname,proto3" toml:"resolve_hostname,omitempty" json:"resolve_hostname,omitempty" mapstructure:"resolve_hostname,omitempty"`
				} `protobuf:"bytes,,opt,name=transport,proto3" toml:"transport,omitempty" json:"transport,omitempty" mapstructure:"transport,omitempty"`
			} `protobuf:"bytes,,opt,name=ssl,proto3" toml:"ssl,omitempty" json:"ssl,omitempty" mapstructure:"ssl,omitempty"`
			SystemIndices *struct {
				CloudAwsSigner string `protobuf:"bytes,,opt,name=cloud_aws_signer,proto3" toml:"cloud_aws_signer,omitempty" json:"cloud_aws_signer,omitempty" mapstructure:"cloud_aws_signer,omitempty"`
				Enabled        *bool  `protobuf:"bytes,,opt,name=enabled,proto3" toml:"enabled,omitempty" json:"enabled,omitempty" mapstructure:"enabled,omitempty"`
				Indices        string `protobuf:"bytes,,opt,name=indices,proto3" toml:"indices,omitempty" json:"indices,omitempty" mapstructure:"indices,omitempty"`
			} `protobuf:"bytes,,opt,name=system_indices,proto3" toml:"system_indices,omitempty" json:"system_indices,omitempty" mapstructure:"system_indices,omitempty"`
		} `protobuf:"bytes,,opt,name=security,proto3" toml:"security,omitempty" json:"security,omitempty" mapstructure:"security,omitempty"`
	} `protobuf:"bytes,,opt,name=plugins,proto3" toml:"plugins,omitempty" json:"plugins,omitempty" mapstructure:"plugins,omitempty"`
	Runtime *struct {
		EsJavaOpts                     string `protobuf:"bytes,,opt,name=es_java_opts,proto3" toml:"es_java_opts,omitempty" json:"es_java_opts,omitempty" mapstructure:"es_java_opts,omitempty"`
		EsStartupSleepTime             string `protobuf:"bytes,,opt,name=es_startup_sleep_time,proto3" toml:"es_startup_sleep_time,omitempty" json:"es_startup_sleep_time,omitempty" mapstructure:"es_startup_sleep_time,omitempty"`
		G1ReservePercent               string `protobuf:"bytes,,opt,name=g1ReservePercent,proto3" toml:"g1ReservePercent,omitempty" json:"g1ReservePercent,omitempty" mapstructure:"g1ReservePercent,omitempty"`
		InitiatingHeapOccupancyPercent string `protobuf:"bytes,,opt,name=initiatingHeapOccupancyPercent,proto3" toml:"initiatingHeapOccupancyPercent,omitempty" json:"initiatingHeapOccupancyPercent,omitempty" mapstructure:"initiatingHeapOccupancyPercent,omitempty"`
		MaxHeapsize                    string `protobuf:"bytes,,opt,name=maxHeapsize,proto3" toml:"maxHeapsize,omitempty" json:"maxHeapsize,omitempty" mapstructure:"maxHeapsize,omitempty"`
		MaxLockedMemory                string `protobuf:"bytes,,opt,name=max_locked_memory,proto3" toml:"max_locked_memory,omitempty" json:"max_locked_memory,omitempty" mapstructure:"max_locked_memory,omitempty"`
		MaxOpenFiles                   string `protobuf:"bytes,,opt,name=max_open_files,proto3" toml:"max_open_files,omitempty" json:"max_open_files,omitempty" mapstructure:"max_open_files,omitempty"`
		MinHeapsize                    string `protobuf:"bytes,,opt,name=minHeapsize,proto3" toml:"minHeapsize,omitempty" json:"minHeapsize,omitempty" mapstructure:"minHeapsize,omitempty"`
	} `protobuf:"bytes,,opt,name=runtime,proto3" toml:"runtime,omitempty" json:"runtime,omitempty" mapstructure:"runtime,omitempty"`
	S3 *struct {
		Client struct {
			Default struct {
				Endpoint           string `protobuf:"bytes,,opt,name=endpoint,proto3" toml:"endpoint,omitempty" json:"endpoint,omitempty" mapstructure:"endpoint,omitempty"`
				MaxRetries         string `protobuf:"bytes,,opt,name=max_retries,proto3" toml:"max_retries,omitempty" json:"max_retries,omitempty" mapstructure:"max_retries,omitempty"`
				Protocol           string `protobuf:"bytes,,opt,name=protocol,proto3" toml:"protocol,omitempty" json:"protocol,omitempty" mapstructure:"protocol,omitempty"`
				ReadTimeout        string `protobuf:"bytes,,opt,name=read_timeout,proto3" toml:"read_timeout,omitempty" json:"read_timeout,omitempty" mapstructure:"read_timeout,omitempty"`
				UseThrottleRetries bool   `protobuf:"bytes,,opt,name=use_throttle_retries,proto3" toml:"use_throttle_retries,omitempty" json:"use_throttle_retries,omitempty" mapstructure:"use_throttle_retries,omitempty"`
			} `protobuf:"bytes,,opt,name=default,proto3" toml:"default,omitempty" json:"default,omitempty" mapstructure:"default,omitempty"`
		} `protobuf:"bytes,,opt,name=client,proto3" toml:"client,omitempty" json:"client,omitempty" mapstructure:"client,omitempty"`
	} `protobuf:"bytes,,opt,name=s3,proto3" toml:"s3,omitempty" json:"s3,omitempty" mapstructure:"s3,omitempty"`
	*TLS      `protobuf:"bytes,,opt,name=tls,proto3" toml:"tls,omitempty" json:"tls,omitempty" mapstructure:"tls,omitempty"`
	Transport *struct {
		Port int `protobuf:"bytes,,opt,name=port,proto3" toml:"port,omitzero" json:"port,omitzero" mapstructure:"port,omitzero"`
	} `protobuf:"bytes,,opt,name=transport,proto3" toml:"transport,omitempty" json:"transport,omitempty" mapstructure:"transport,omitempty"`
}

type PatchOpensearchConfig struct {
	Cluster *struct {
		MaxShardsPerNode string `protobuf:"bytes,,opt,name=max_shards_per_node,proto3" toml:"max_shards_per_node,omitempty" json:"max_shards_per_node,omitempty" mapstructure:"max_shards_per_node,omitempty"`
	} `protobuf:"bytes,,opt,name=cluster,proto3" toml:"cluster,omitempty" json:"cluster,omitempty" mapstructure:"cluster,omitempty"`
}

type TLS struct {
	AdminCert string `protobuf:"bytes,,opt,name=admin_cert,proto3" toml:"admin_cert,omitempty" json:"admin_cert,omitempty" mapstructure:"admin_cert,omitempty"`
	AdminKey  string `protobuf:"bytes,,opt,name=admin_key,proto3" toml:"admin_key,omitempty" json:"admin_key,omitempty" mapstructure:"admin_key,omitempty"`
	RootCA    string `protobuf:"bytes,,opt,name=rootCA,proto3" toml:"rootCA,omitempty" json:"rootCA,omitempty" mapstructure:"rootCA,omitempty"`
	SslCert   string `protobuf:"bytes,,opt,name=ssl_cert,proto3" toml:"ssl_cert,omitempty" json:"ssl_cert,omitempty" mapstructure:"ssl_cert,omitempty"`
	SslKey    string `protobuf:"bytes,,opt,name=ssl_key,proto3" toml:"ssl_key,omitempty" json:"ssl_key,omitempty" mapstructure:"ssl_key,omitempty"`
}

type PostgresqlConfig struct {
	Global *struct {
		V1 *struct {
			Log *struct {
				state         protoimpl.MessageState
				sizeCache     protoimpl.SizeCache
				unknownFields protoimpl.UnknownFields

				Level                *wrapperspb.StringValue `protobuf:"bytes,1,opt,name=level,proto3" json:"level,omitempty" toml:"level,omitempty" mapstructure:"level,omitempty"`
				Format               *wrapperspb.StringValue `protobuf:"bytes,2,opt,name=format,proto3" json:"format,omitempty" toml:"format,omitempty" mapstructure:"format,omitempty"`
				RedirectSysLog       *wrapperspb.BoolValue   `protobuf:"bytes,3,opt,name=redirect_sys_log,json=redirectSysLog,proto3" json:"redirect_sys_log,omitempty" toml:"redirect_sys_log,omitempty" mapstructure:"redirect_sys_log,omitempty"`
				RedirectLogFilePath  *wrapperspb.StringValue `protobuf:"bytes,4,opt,name=redirect_log_file_path,json=redirectLogFilePath,proto3" json:"redirect_log_file_path,omitempty" toml:"redirect_log_file_path,omitempty" mapstructure:"redirect_log_file_path,omitempty"`
				CompressRotatedLogs  *wrapperspb.BoolValue   `protobuf:"bytes,5,opt,name=compress_rotated_logs,json=compressRotatedLogs,proto3" json:"compress_rotated_logs,omitempty" toml:"compress_rotated_logs,omitempty" mapstructure:"compress_rotated_logs,omitempty"`
				MaxSizeRotateLogs    *wrapperspb.StringValue `protobuf:"bytes,6,opt,name=max_size_rotate_logs,json=maxSizeRotateLogs,proto3" json:"max_size_rotate_logs,omitempty" toml:"max_size_rotate_logs,omitempty" mapstructure:"max_size_rotate_logs,omitempty"`
				MaxNumberRotatedLogs *wrapperspb.Int32Value  `protobuf:"bytes,7,opt,name=max_number_rotated_logs,json=maxNumberRotatedLogs,proto3" json:"max_number_rotated_logs,omitempty" toml:"max_number_rotated_logs,omitempty" mapstructure:"max_number_rotated_logs,omitempty"`
			}
		}
	}
	CheckpointCompletionTarget float64 `protobuf:"bytes,,opt,name=checkpoint_completion_target,proto3" toml:"checkpoint_completion_target,omitzero" json:"checkpoint_completion_target,omitzero" mapstructure:"checkpoint_completion_target,omitzero"`
	CheckpointTimeout          string  `protobuf:"bytes,,opt,name=checkpoint_timeout,proto3" toml:"checkpoint_timeout,omitempty" json:"checkpoint_timeout,omitempty" mapstructure:"checkpoint_timeout,omitempty"`
	Host                       string  `protobuf:"bytes,,opt,name=host,proto3" toml:"host,omitempty" json:"host,omitempty" mapstructure:"host,omitempty"`
	LogLevel                   string  `protobuf:"bytes,,opt,name=log_level,proto3" toml:"log_level,omitempty" json:"log_level,omitempty" mapstructure:"log_level,omitempty"`
	LogLinePrefix              string  `protobuf:"bytes,,opt,name=log_line_prefix,proto3" toml:"log_line_prefix,omitempty" json:"log_line_prefix,omitempty" mapstructure:"log_line_prefix,omitempty"`
	LoggingCollector           string  `protobuf:"bytes,,opt,name=logging_collector,proto3" toml:"logging_collector,omitempty" json:"logging_collector,omitempty" mapstructure:"logging_collector,omitempty"`
	MaxConnections             int     `protobuf:"bytes,,opt,name=max_connections,proto3" toml:"max_connections,omitzero" json:"max_connections,omitzero" mapstructure:"max_connections,omitzero"`
	MaxLocksPerTransaction     int     `protobuf:"bytes,,opt,name=max_locks_per_transaction,proto3" toml:"max_locks_per_transaction,omitzero" json:"max_locks_per_transaction,omitzero" mapstructure:"max_locks_per_transaction,omitzero"`
	MaxWalSize                 string  `protobuf:"bytes,,opt,name=max_wal_size,proto3" toml:"max_wal_size,omitempty" json:"max_wal_size,omitempty" mapstructure:"max_wal_size,omitempty"`
	MinWalSize                 string  `protobuf:"bytes,,opt,name=min_wal_size,proto3" toml:"min_wal_size,omitempty" json:"min_wal_size,omitempty" mapstructure:"min_wal_size,omitempty"`
	Port                       int     `protobuf:"bytes,,opt,name=port,proto3" toml:"port,omitzero" json:"port,omitzero" mapstructure:"port,omitzero"`
	PrintDbStatistics          bool    `protobuf:"bytes,,opt,name=print_db_statistics,proto3" toml:"print_db_statistics,omitempty" json:"print_db_statistics,omitempty" mapstructure:"print_db_statistics,omitempty"`
	WalKeepSize                int     `protobuf:"bytes,,opt,name=wal_keep_size,proto3" toml:"wal_keep_size,omitzero" json:"wal_keep_size,omitzero" mapstructure:"wal_keep_size,omitzero"`
	PgDump                     *struct {
		Enable bool   `protobuf:"bytes,,opt,name=enable,proto3" toml:"enable,omitempty" json:"enable,omitempty" mapstructure:"enable,omitempty"`
		Path   string `protobuf:"bytes,,opt,name=path,proto3" toml:"path,omitempty" json:"path,omitempty" mapstructure:"path,omitempty"`
	} `protobuf:"bytes,,opt,name=pg_dump,proto3" toml:"pg_dump,omitempty" json:"pg_dump,omitempty" mapstructure:"pg_dump,omitempty"`
	Replication *struct {
		LagHealthThreshold         int    `protobuf:"bytes,,opt,name=lag_health_threshold,proto3" toml:"lag_health_threshold,omitzero" json:"lag_health_threshold,omitzero" mapstructure:"lag_health_threshold,omitzero"`
		MaxReplayLagBeforeRestartS int    `protobuf:"bytes,,opt,name=max_replay_lag_before_restart_s,proto3" toml:"max_replay_lag_before_restart_s,omitzero" json:"max_replay_lag_before_restart_s,omitzero" mapstructure:"max_replay_lag_before_restart_s,omitzero"`
		Name                       string `protobuf:"bytes,,opt,name=name,proto3" toml:"name,omitempty" json:"name,omitempty" mapstructure:"name,omitempty"`
		Password                   string `protobuf:"bytes,,opt,name=password,proto3" toml:"password,omitempty" json:"password,omitempty" mapstructure:"password,omitempty"`
	} `protobuf:"bytes,,opt,name=replication,proto3" toml:"replication,omitempty" json:"replication,omitempty" mapstructure:"replication,omitempty"`
	S3 *struct {
		Client struct {
			Default struct {
				ReadTimeout string `protobuf:"bytes,,opt,name=read_timeout,proto3" toml:"read_timeout,omitempty" json:"read_timeout,omitempty" mapstructure:"read_timeout,omitempty"`
			} `protobuf:"bytes,,opt,name=default,proto3" toml:"default,omitempty" json:"default,omitempty" mapstructure:"default,omitempty"`
		} `protobuf:"bytes,,opt,name=client,proto3" toml:"client,omitempty" json:"client,omitempty" mapstructure:"client,omitempty"`
	} `protobuf:"bytes,,opt,name=s3,proto3" toml:"s3,omitempty" json:"s3,omitempty" mapstructure:"s3,omitempty"`
	Ssl *struct {
		Enable     bool   `protobuf:"bytes,,opt,name=enable,proto3" toml:"enable,omitempty" json:"enable,omitempty" mapstructure:"enable,omitempty"`
		IssuerCert string `protobuf:"bytes,,opt,name=issuer_cert,proto3" toml:"issuer_cert,omitempty" json:"issuer_cert,omitempty" mapstructure:"issuer_cert,omitempty"`
		SslCert    string `protobuf:"bytes,,opt,name=ssl_cert,proto3" toml:"ssl_cert,omitempty" json:"ssl_cert,omitempty" mapstructure:"ssl_cert,omitempty"`
		SslKey     string `protobuf:"bytes,,opt,name=ssl_key,proto3" toml:"ssl_key,omitempty" json:"ssl_key,omitempty" mapstructure:"ssl_key,omitempty"`
		TLSCiphers string `protobuf:"bytes,,opt,name=tls_ciphers,proto3" toml:"tls_ciphers,omitempty" json:"tls_ciphers,omitempty" mapstructure:"tls_ciphers,omitempty"`
	} `protobuf:"bytes,,opt,name=ssl,proto3" toml:"ssl,omitempty" json:"ssl,omitempty" mapstructure:"ssl,omitempty"`
	Superuser *struct {
		Name     string `protobuf:"bytes,,opt,name=name,proto3" toml:"name,omitempty" json:"name,omitempty" mapstructure:"name,omitempty"`
		Password string `protobuf:"bytes,,opt,name=password,proto3" toml:"password,omitempty" json:"password,omitempty" mapstructure:"password,omitempty"`
	} `protobuf:"bytes,,opt,name=superuser,proto3" toml:"superuser,omitempty" json:"superuser,omitempty" mapstructure:"superuser,omitempty"`
	WalArchive *struct {
		Enable bool   `protobuf:"bytes,,opt,name=enable,proto3" toml:"enable,omitempty" json:"enable,omitempty" mapstructure:"enable,omitempty"`
		Path   string `protobuf:"bytes,,opt,name=path,proto3" toml:"path,omitempty" json:"path,omitempty" mapstructure:"path,omitempty"`
	} `protobuf:"bytes,,opt,name=wal_archive,proto3" toml:"wal_archive,omitempty" json:"wal_archive,omitempty" mapstructure:"wal_archive,omitempty"`
}
