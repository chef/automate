package gcsbackupchecktrigger

import (
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/checkutils"
	"github.com/chef/automate/lib/logger"
)

type GcsBackupConfigCheck struct {
	log  logger.Logger
	port string
}

func NewGcsBackupConfigCheck(log logger.Logger, port string) *GcsBackupConfigCheck {
	return &GcsBackupConfigCheck{
		log:  log,
		port: port,
	}
}

func (svc *GcsBackupConfigCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	if config.Hardware == nil {
		return []models.CheckTriggerResponse{
			trigger.SkippedTriggerCheckResp("-", constants.GCP_BACKUP_CONFIG, constants.AUTOMATE, constants.SKIP_MISSING_HARDWARE_MESSAGE),
		}
	}

	if config.Backup == nil || config.Backup.ObjectStorage == nil || config.Backup.ObjectStorage.Location != "gcs" {
		return GcsConfigSkippedResponse(config, constants.GCP_BACKUP_CONFIG, constants.SKIP_BACKUP_TEST_MESSAGE_GCS)
	}

	if !isGcsStorage(config) {
		return emptyResp(config, constants.GCP_BACKUP_CONFIG)
	}

	req := getGCSCheckRequest(config.Backup.ObjectStorage)
	return runCheckForGCSConfig(config.Hardware.AutomateNodeIps, svc.log, svc.port, http.MethodPost, req)
}

func GcsConfigSkippedResponse(config *models.Config, checkType, message string) []models.CheckTriggerResponse {
	var triggerResps []models.CheckTriggerResponse

	for _, ip := range config.Hardware.AutomateNodeIps {
		triggerResps = append(triggerResps, trigger.SkippedTriggerCheckResp(ip, checkType, constants.AUTOMATE, message))
	}

	for _, ip := range config.Hardware.ChefInfraServerNodeIps {
		triggerResps = append(triggerResps, trigger.SkippedTriggerCheckResp(ip, checkType, constants.CHEF_INFRA_SERVER, message))
	}

	return triggerResps
}

func (ss *GcsBackupConfigCheck) GetPortsForMockServer() map[string]map[string][]int {
	nodeTypePortMap := make(map[string]map[string][]int)
	return nodeTypePortMap
}

// runCheckForGCSConfig triggers the API on gives node automate nodes only for validating GCS backup config
func runCheckForGCSConfig(nodeIps []string, log logger.Logger, port string, method string, reqBody models.GCPCloudStorageConfigRequest) []models.CheckTriggerResponse {
	log.Debugf("Triggering the api call for automate nodes only for Gcp backup config")
	outputCh := make(chan models.CheckTriggerResponse)
	for _, ip := range nodeIps {
		log.Debugf("Triggering api %s for the node %s", constants.GCP_CLOUD_STORAGE_CONFIG_API_PATH, ip)
		endpoint := checkutils.PrepareEndPoint(ip, port, constants.GCP_CLOUD_STORAGE_CONFIG_API_PATH)
		go trigger.TriggerCheckAPI(endpoint, ip, constants.AUTOMATE, method, outputCh, reqBody)
	}

	response := getResultFromOutputChan(len(nodeIps), outputCh)
	close(outputCh)
	return response
}

// getResultFromOutputChan gets the result from output channel
func getResultFromOutputChan(reqList int, outputCh chan models.CheckTriggerResponse) []models.CheckTriggerResponse {
	var result []models.CheckTriggerResponse

	for i := 0; i < reqList; i++ {
		res := <-outputCh
		result = append(result, res)
	}

	return result
}

func getGCSCheckRequest(object *models.ObjectStorage) models.GCPCloudStorageConfigRequest {
	//passing a default value if GCS bucket path not provided
	if object.BasePath == "" {
		object.BasePath = "automate"
	}
	return models.GCPCloudStorageConfigRequest{
		BucketName:               object.BucketName,
		GoogleServiceAccountFile: object.GoogleServiceAccountFile,
		GcpServiceAccount: &models.GcpServiceAccount{
			Type:                    object.GcpServiceAccount.Type,
			ProjectID:               object.GcpServiceAccount.ProjectID,
			PrivateKeyID:            object.GcpServiceAccount.PrivateKeyID,
			PrivateKey:              object.GcpServiceAccount.PrivateKey,
			ClientEmail:             object.GcpServiceAccount.ClientEmail,
			ClientID:                object.GcpServiceAccount.ClientID,
			AuthURI:                 object.GcpServiceAccount.AuthURI,
			TokenURI:                object.GcpServiceAccount.TokenURI,
			AuthProviderX509CertURL: object.GcpServiceAccount.AuthProviderX509CertURL,
			ClientX509CertURL:       object.GcpServiceAccount.ClientX509CertURL,
			UniverseDomain:          object.GcpServiceAccount.UniverseDomain,
		},
	}
}

func isGcsStorage(config *models.Config) bool {
	backup := config.Backup
	return backup.ObjectStorage.BucketName != ""
}

func emptyResp(config *models.Config, checktype string) []models.CheckTriggerResponse {
	resps := []models.CheckTriggerResponse{}
	for _, ip := range config.Hardware.AutomateNodeIps {
		resps = append(resps, trigger.ErrTriggerCheckResp(ip, checktype, constants.AUTOMATE, constants.GCS_BACKUP_MISSING))
	}

	return resps
}
