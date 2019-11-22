package deployment

import (
	"github.com/sirupsen/logrus"

	api "github.com/chef/automate/api/config/deployment"
	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

// MergeAndValidateNewUserOverrideConfig takes an existing override config
// and the BackupRestoreTask and builds/merges a new user override config.
//
// Validation of the configuration is a bit tricky. We still want to prevent
// people from _adding_ deprecated values if the pass in set or patch config,
// but we still want them to be able to restore if their config already has
// deprecated values.
func MergeAndValidateNewUserOverrideConfig(existing *api.AutomateConfig, req *BackupRestoreTask) error {
	var err error
	cfg := &api.AutomateConfig{}

	// They gave us nothing, return the existing config.
	if req == nil {
		return nil
	}

	// If they passed a config set we'll only use that
	if req.GetSetConfig() != nil {
		*existing = *req.SetConfig
		return existing.ValidateWithGlobalAndDefaults()
	}

	// If they passed a patch we'll validate it against a redacted copy. If
	// it doesn't pass then they've passed in an invalid patch. Later we'll
	// merge the patch config into the config that we'll generate from the
	// restore task.
	if req.GetPatchConfig() != nil {
		existingCopy, err := existing.RedactedCopy()
		if err != nil {
			return err
		}

		err = existingCopy.OverrideConfigValues(req.PatchConfig)
		if err != nil {
			return err
		}

		err = existingCopy.ValidateWithGlobalAndDefaults()
		if err != nil {
			return err
		}
	}

	// Merge the restore task options into the config
	reqS3 := req.GetS3BackupLocation()
	if reqS3.GetBucketName() == "" {
		// Return the defaults because we're in filesystem mode and the request
		// caries no meaningful backup info in that mode.
		// NOTE: It would be nice to carry the endpoint type in the backup req
		return nil
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

	if req.GetPatchConfig() != nil {
		err = cfg.OverrideConfigValues(req.PatchConfig)
		if err != nil {
			return err
		}
	}

	err = existing.OverrideConfigValues(cfg)
	if err != nil {
		return err
	}

	return nil
}
