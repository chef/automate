package opensearchs3bucketaccesschecktrigger

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/lib/logger"
)

type OpensearchS3BucketAccessCheck struct {
	log  logger.Logger
	port string
	host string
}

func NewOpensearchS3BucketAccessCheck(log logger.Logger, port string) *OpensearchS3BucketAccessCheck {
	return &OpensearchS3BucketAccessCheck{
		log:  log,
		port: port,
		host: constants.LOCAL_HOST_URL,
	}
}

func (osb *OpensearchS3BucketAccessCheck) Run(config models.Config) []models.CheckTriggerResponse {
	s3OpensearchBackupRequest := models.S3OpenSearchBackupRequest{
		Endpoint:   config.ExternalOS.OSDomainURL,
		Username:   config.ExternalOS.OSUsername,
		Password:   config.ExternalOS.OSUserPassword,
		S3Bucket:   config.Backup.ObjectStorage.BucketName,
		S3BasePath: config.Backup.ObjectStorage.BasePath,
		AccessKey:  config.Backup.ObjectStorage.AccessKey,
		SecretKey:  config.Backup.ObjectStorage.SecretKey,
		AWSRegion:  config.Backup.ObjectStorage.AWSRegion,
		AWSRoleArn: config.ExternalOS.OSRoleArn,
	}
	response := trigger.RunCheckOnSpecifiedNode([]string{osb.host}, osb.log, osb.port, constants.AWS_OPENSEARCH_S3_BUCKET_ACCESS_API_PATH, constants.OPENSEARCH, http.MethodPost, s3OpensearchBackupRequest)

	return setHostAsOpensearchInResponse(response, config.ExternalOS.OSDomainURL)

}

func setHostAsOpensearchInResponse(response []models.CheckTriggerResponse, osExternalUrl string) []models.CheckTriggerResponse {
	var result []models.CheckTriggerResponse
	for _, resp := range response {
		resp.Host = osExternalUrl
		result = append(result, resp)
	}
	return result
}
