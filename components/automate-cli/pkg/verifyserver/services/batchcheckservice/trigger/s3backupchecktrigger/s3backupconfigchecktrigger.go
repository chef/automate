package s3backupchecktrigger

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/checkutils"
	"github.com/chef/automate/lib/logger"
)

type S3BackupConfigCheck struct {
	log  logger.Logger
	port string
}

func NewS3BackupConfigCheck(log logger.Logger, port string) *S3BackupConfigCheck {
	return &S3BackupConfigCheck{
		log:  log,
		port: port,
	}
}

func (svc *S3BackupConfigCheck) Run(config models.Config) []models.CheckTriggerResponse {
	req := getS3CheckRequest(config.Backup.ObjectStorage)
	return runCheckForS3Config(config.Hardware.AutomateNodeIps, svc.log, svc.port, constants.S3_BACKUP_CHECK_API_PATH, constants.AUTOMATE, http.MethodPost, req)
}

//runCheckForS3Config triggers the API on gives node automate nodes only for validating s3 backup config
func runCheckForS3Config(nodeIps []string, log logger.Logger, port string, path string, nodeType string, method string, reqBody models.S3ConfigRequest) []models.CheckTriggerResponse {
	log.Debugf("Triggering the api call for automate nodes only for s3 backup config")
	outputCh := make(chan models.CheckTriggerResponse)
	for _, ip := range nodeIps {
		log.Debugf("Triggering api %s for the node %s", path, ip)
		endpoint := checkutils.PrepareEndPoint(ip, port, path)
		go trigger.TriggerCheckAPI(endpoint, ip, nodeType, method, outputCh, reqBody)
	}

	response := getResultFromOutputChan(len(nodeIps), outputCh)
	close(outputCh)
	return response
}

//getResultFromOutputChan gets the result from output channel
func getResultFromOutputChan(reqList int, outputCh chan models.CheckTriggerResponse) []models.CheckTriggerResponse {
	var result []models.CheckTriggerResponse

	for i := 0; i < reqList; i++ {
		res := <-outputCh
		result = append(result, res)
	}

	return result
}

func getS3CheckRequest(object models.ObjectStorage) models.S3ConfigRequest {
	return models.S3ConfigRequest{
		Endpoint:   object.Endpoint,
		BucketName: object.BucketName,
		BasePath:   object.BasePath,
		AccessKey:  object.AccessKey,
		SecretKey:  object.SecretKey,
		Region:     object.AWSRegion,
	}
}
