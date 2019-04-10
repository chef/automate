package deployment

import (
	api "github.com/chef/automate/api/config/deployment"
	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

// NewUserOverrideConfigFromBackupRestoreTask takes a BackupRestoreTask and
// converts the restoration configuration contained within into a new user
// override AutomateConfig.
func NewUserOverrideConfigFromBackupRestoreTask(req *BackupRestoreTask) *api.AutomateConfig {
	// Convert a BackupRestoreTask into a sparse AutomateConfig that is suitable
	// to be merged into the UserOverrideConfig
	cfg := &api.AutomateConfig{}
	if req == nil {
		return cfg
	}

	reqS3 := req.GetS3BackupLocation()
	if reqS3.GetBucketName() == "" {
		// Return the defaults because we're in filesystem mode and the request
		// caries no meaningful backup info in that mode.
		// NOTE: It would be nice to carry the endpoint type in the backup req
		return cfg
	}

	// We're in S3 mode so we need to populate the config with any options
	// that are in the req.
	cfg.Global = config.NewGlobalConfig()
	cfg.Global.V1.Backups = &config.Backups{
		S3: &config.Backups_S3{
			Credentials: &config.Backups_S3_AWSCredentials{},
			Bucket:      &config.Backups_S3_Bucket{},
		},
	}
	creds := cfg.GetGlobal().GetV1().GetBackups().GetS3().GetCredentials()
	bucket := cfg.GetGlobal().GetV1().GetBackups().GetS3().GetBucket()

	if bName := reqS3.GetBucketName(); bName != "" {
		bucket.Name = w.String(bName)
	}

	if endpoint := reqS3.GetEndpoint(); endpoint != "" {
		bucket.Endpoint = w.String(endpoint)
	}

	if basePath := reqS3.GetBasePath(); basePath != "" {
		bucket.BasePath = w.String(basePath)
	}

	if accessKey := reqS3.GetAccessKey(); accessKey != "" {
		creds.AccessKey = w.String(accessKey)
	}

	if secretKey := reqS3.GetSecretKey(); secretKey != "" {
		creds.SecretKey = w.String(secretKey)
	}

	if sessionToken := reqS3.GetSessionToken(); sessionToken != "" {
		creds.SessionToken = w.String(sessionToken)
	}

	return cfg
}
