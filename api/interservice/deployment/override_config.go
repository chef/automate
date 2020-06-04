package deployment

import (
	"github.com/sirupsen/logrus"

	api "github.com/chef/automate/api/config/deployment"
	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

// MergeAndValidateNewUserOverrideConfig is responsible for taking an existing
// user override AutomateConfig (from a backup), building a new user override
// config from a BackupRestoreTask (args for the backup restore), and validating
// the resulting user override config.
//
// Validation of the config is a bit tricky as there are a few corner cases we
// need to worry about. It's possible that the user wants to pass in config set
// or patch that includes deprecated values since it's possible that the version
// of the software they are restoring could use those values. We'll handle this
// case by validating the with a redacted copy of the config.
func MergeAndValidateNewUserOverrideConfig(existing *api.AutomateConfig, req *BackupRestoreTask) error {
	var err error
	cfg := &api.AutomateConfig{}

	// They gave us nothing, return the existing config.
	if req == nil {
		return nil
	}

	// If they passed a config set we'll only use it as that is their desired
	// config.
	if setConfig := req.GetSetConfig(); setConfig != nil {
		redactedCopy, err := setConfig.RedactedCopy()
		if err != nil {
			return err
		}

		err = redactedCopy.ValidateWithGlobalAndDefaults()
		if err != nil {
			return err
		}

		*existing = *setConfig
		return nil
	}

	// If they passed a patch we'll validate a redacted version. If it doesn't
	// pass then they've passed in an invalid patch. Later we'll
	// merge the patch config into the config that we'll generate from the
	// restore task.
	patchConfig := req.GetPatchConfig()
	if patchConfig != nil {
		existingRedactedCopy, err := existing.RedactedCopy()
		if err != nil {
			return err
		}

		redactedPatch, err := patchConfig.RedactedCopy()
		if err != nil {
			return err
		}

		err = existingRedactedCopy.OverrideConfigValues(redactedPatch)
		if err != nil {
			return err
		}

		err = existingRedactedCopy.ValidateWithGlobalAndDefaults()
		if err != nil {
			return err
		}
	}

	// Merge the restore task options into the config
	if reqS3 := req.GetS3BackupLocation(); reqS3.GetBucketName() != "" {
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
	} else if reqGCS := req.GetGcsBackupLocation(); reqGCS.BucketName != "" {
		if cfg.GetGlobal().GetV1() == nil {
			cfg.Global = config.NewGlobalConfig()
		}

		if cfg.Global.V1.Backups == nil {
			cfg.Global.V1.Backups = &config.Backups{}
		}

		cfg.Global.V1.Backups.Gcs = &config.Backups_GCS{
			Credentials: &config.Backups_GCS_GCPCredentials{},
			Bucket: &config.Backups_GCS_Bucket{
				Name: w.String(reqGCS.BucketName),
			},
		}

		if reqGCS.BasePath != "" {
			cfg.Global.V1.Backups.Gcs.Bucket.BasePath = w.String(reqGCS.BasePath)
		}

		if reqGCS.ProjectId != "" {
			cfg.Global.V1.Backups.Gcs.Bucket.ProjectId = w.String(reqGCS.ProjectId)
		}

		if reqGCS.GoogleApplicationCredentials != "" {
			cfg.Global.V1.Backups.Gcs.Credentials.Json = w.String(reqGCS.GoogleApplicationCredentials)
		}
	}

	if patchConfig != nil {
		err = cfg.OverrideConfigValues(patchConfig)
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
