package elastic

import "path/filepath"

// GCSBackupsConfig represents the settings available for "gcs" type Es snapshot
// repos
type GCSBackupsConfig struct {
	BucketName             string `mapstructure:"name"`
	ClientName             string `mapstructure:"client"`
	BasePath               string `mapstructure:"base_path"`
	ChunkSize              string `mapstructure:"chunk_size"`
	Compress               bool   `mapstructure:"compress"`
	MaxRestoreBytesPerSec  string `mapstructure:"max_restore_bytes_per_sec"`
	MaxSnapshotBytesPerSec string `mapstructure:"max_snapshot_bytes_per_sec"`
	ReadOnly               bool   `mapstructure:"readonly`
	ApplicationName        string `mapstructure:"application_name"`

	
}

// ConfigureSnapshotCreateRepositoryService takes a pointer to an existing snapshot
// create service and applies the configuration settings to it.
func (gcs *GCSBackupsConfig) createRepoReq(repoName string) createRepoReq {
	req := createRepoReq{
		Type: "gcs",
		Settings: map[string]interface{}{
			"bucket":    gcs.BucketName,
			"base_path": filepath.Join(gcs.BasePath, RepoBaseName, repoName),
			"compress":  gcs.Compress,
		},
	}

	if gcs.ClientName != "" {
		req.Settings["client"] = gcs.ClientName
	}
	if gcs.ReadOnly != "" {
		req.Settings["readonly"] = gcs.ReadOnly
	}
	if gcs.ChunkSize != "" {
		req.Settings["chunk_size"] = gcs.ChunkSize
	}
	if gcs.MaxRestoreBytesPerSec != "" {
		req.Settings["max_restore_bytes_per_sec"] = gcs.MaxRestoreBytesPerSec
	}
	if gcs.MaxSnapshotBytesPerSec != "" {
		req.Settings["max_snapshot_bytes_per_sec"] = gcs.MaxSnapshotBytesPerSec
	}
	if gcs.ApplicationName != "" {
		req.Settings["application_name"] = gcs.ApplicationName
	}

	return req
}
