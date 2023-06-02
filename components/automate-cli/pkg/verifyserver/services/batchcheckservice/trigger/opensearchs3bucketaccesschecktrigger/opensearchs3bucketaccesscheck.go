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
	s3OpensearchBackupRequest := models.S3BackupDetails{
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

	response := triggerCheckForOpensearchS3Backup(endPoint, osb.log, http.MethodPost, s3OpensearchBackupRequest)

	return setHostAsOpensearchInResponse(response, config.ExternalOS.OSDomainURL)

}

func (ss *OpensearchS3BucketAccessCheck) GetPortsForMockServer() map[string]map[string][]int {
	nodeTypePortMap := make(map[string]map[string][]int)
	return nodeTypePortMap
}

// setHostAsOpensearchInResponse sets the Host as external OS endpoint as this will help us in mapping the result correctly
func setHostAsOpensearchInResponse(response []models.CheckTriggerResponse, osExternalUrl string) []models.CheckTriggerResponse {
	for i := range response {
		response[i].Host = osExternalUrl

	}
	return response
}

// triggerCheckForOpensearchS3Backup triggers the API on given for external opensearch connectivity with s3
func triggerCheckForOpensearchS3Backup(endPoint string, log logger.Logger, method string, reqBody models.S3BackupDetails) []models.CheckTriggerResponse {
	var result []models.CheckTriggerResponse
	log.Debugf("Triggering the api call for Opensearch for S3 backup")
	outputCh := make(chan models.CheckTriggerResponse)

	//There will be only one request which will check the connection
	go trigger.TriggerCheckAPI(endPoint, reqBody.Endpoint, constants.OPENSEARCH, method, outputCh, reqBody)

	//As we are triggering only one request for checking the connection for opensearch and s3 bucket
	res := <-outputCh
	result = append(result, res)

	return result

}
