package elastic

import "path/filepath"

// S3BackupsConfig represents the settings available for "s3" type Es snapshot
// repos
type S3BackupsConfig struct {
	BucketName             string `mapstructure:"name"`
	ClientName             string `mapstructure:"client"`
	BasePath               string `mapstructure:"base_path"`
	Compress               bool   `mapstructure:"compress"`
	ServerSideEncryption   string `mapstructure:"server_side_encryption"`
	BufferSize             string `mapstructure:"buffer_size"`
	CannedACL              string `mapstructure:"canned_acl"`
	StorageClass           string `mapstructure:"storage_class"`
	MaxSnapshotBytesPerSec string `mapstructure:"max_snapshot_bytes_per_sec"`
	MaxRestoreBytesPerSec  string `mapstructure:"max_restore_bytes_per_sec"`
	ChunkSize              string `mapstructure:"chunk_size"`
}

// ConfigureSnapshotCreateRepositoryService takes a pointer to an existing snapshot
// create service and applies the configuration settings to it.
func (s3 *S3BackupsConfig) createRepoReq(repoName string) createRepoReq {
	req := createRepoReq{
		Type: "s3",
		Settings: map[string]interface{}{
			"bucket":    s3.BucketName,
			"base_path": filepath.Join(s3.BasePath, RepoBaseName, repoName),
			"compress":  s3.Compress,
		},
	}

	if s3.ClientName != "" {
		req.Settings["client"] = s3.ClientName
	}
	if s3.ServerSideEncryption != "" {
		req.Settings["server_side_encryption"] = s3.ServerSideEncryption
	}
	if s3.BufferSize != "" {
		req.Settings["buffer_size"] = s3.BufferSize
	}
	if s3.CannedACL != "" {
		req.Settings["canned_acl"] = s3.CannedACL
	}
	if s3.ChunkSize != "" {
		req.Settings["chunk_size"] = s3.ChunkSize
	}
	if s3.StorageClass != "" {
		req.Settings["storage_class"] = s3.StorageClass
	}
	if s3.MaxRestoreBytesPerSec != "" {
		req.Settings["max_restore_bytes_per_sec"] = s3.MaxRestoreBytesPerSec
	}
	if s3.MaxSnapshotBytesPerSec != "" {
		req.Settings["max_snapshot_bytes_per_sec"] = s3.MaxSnapshotBytesPerSec
	}

	return req
}
