package opensearchs3bucketaccesschecktrigger

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/checkutils"
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

	endPoint := checkutils.PrepareEndPoint(osb.host, osb.port, constants.AWS_OPENSEARCH_S3_BUCKET_ACCESS_API_PATH)

	response := triggerCheckForOpensearchS3Backup(endPoint, osb.log, constants.OPENSEARCH, http.MethodPost, s3OpensearchBackupRequest)

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

// RunCheckOnSpecifiedNodes triggers the API on gives node ips only, requires for various API's like S3/Minio backup config
func triggerCheckForOpensearchS3Backup(endPoint string, log logger.Logger, nodeType string, method string, reqBody models.S3OpenSearchBackupRequest) []models.CheckTriggerResponse {
	var result []models.CheckTriggerResponse
	log.Debugf("Triggering the api call for Opensearch for S3 backup")
	outputCh := make(chan models.CheckTriggerResponse)

	//There will be only one request which will check the connection
	go trigger.TriggerCheckAPI(endPoint, reqBody.Endpoint, nodeType, method, outputCh, reqBody)

	//As we are triggering only one request for checking the connection for opensearch and s3 bucket
	res := <-outputCh
	result = append(result, res)

	return result

}
