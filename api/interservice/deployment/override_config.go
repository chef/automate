package deployment

import (
	api "github.com/chef/automate/api/config/deployment"
	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/sirupsen/logrus"
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
	if req.PatchConfig != nil {
		cfg = req.PatchConfig
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
	if cfg.GetGlobal().GetV1() == nil {
		cfg.Global = config.NewGlobalConfig()
	}

	if cfg.Global.V1.Backups == nil {
		cfg.Global.V1.Backups = &config.Backups{}
	}

	if cfg.Global.V1.Backups.GetS3() != nil {
		// We can't use the config from the override config because it might
		// be different than where you told us the backup lives. We need to
		// configure automate to use the backup location where the backup lives
		// in order to restore (mostly because of the way ES works)
		logrus.Warn("Ignoring s3 config from restore override config in favor of user-specified backup location")
	}

	cfg.Global.V1.Backups.S3 = &config.Backups_S3{
		Credentials: &config.Backups_S3_AWSCredentials{},
		Bucket:      &config.Backups_S3_Bucket{},
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
