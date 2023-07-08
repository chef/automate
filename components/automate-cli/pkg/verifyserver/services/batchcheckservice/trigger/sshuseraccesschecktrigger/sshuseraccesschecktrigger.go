package sshuseraccesschecktrigger

import (
	"fmt"
	"net/http"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/batchcheckservice/trigger"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/configutils"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
	"github.com/gofiber/fiber/v2"
	"github.com/pkg/errors"
)

type SshUserAccessCheck struct {
	host      string
	port      string
	log       logger.Logger
	fileUtils fileutils.FileUtils
}

func NewSshUserAccessCheck(log logger.Logger, fileutils fileutils.FileUtils, port string) *SshUserAccessCheck {
	return &SshUserAccessCheck{
		log:       log,
		host:      constants.LOCAL_HOST_URL,
		port:      port,
		fileUtils: fileutils,
	}
}

func (ss *SshUserAccessCheck) Run(config *models.Config) []models.CheckTriggerResponse {
	ss.log.Info("Performing SSH user access check from batch check ")

	// Check if certificate is empty or nil
	if config.Hardware == nil {
		return trigger.HardwareNil(constants.SSH_USER, true, true, false)
	}
	if config.SSHUser == nil {
		return trigger.ConstructNilResp(config, constants.SSH_USER)
	}
	if IsSSHUserEmpty(config.SSHUser) {
		return trigger.ConstructEmptyResp(config, constants.SSH_USER, "SSH credentials is missing")
	}

	count := 0

	outputCh := make(chan models.CheckTriggerResponse)
	url := fmt.Sprintf("%s:%s%s", ss.host, ss.port, constants.SSH_USER_CHECK_API_PATH)
	var finalResult []models.CheckTriggerResponse
	hostMap := configutils.GetNodeTypeMap(config.Hardware)
	for ip, types := range hostMap {
		for i := 0; i < len(types); i++ {
			requestBody, err := ss.getSShUserAPIRequest(ip, config.SSHUser)
			if err != nil {
				finalResult = append(finalResult, models.CheckTriggerResponse{
					Host:     ip,
					NodeType: types[i],
					Result: models.ApiResult{
						Passed: false,
						Error: &fiber.Error{
							Code:    http.StatusBadRequest,
							Message: err.Error(),
						},
					},
				})
				continue
			}
			count = count + 1
			go trigger.TriggerCheckAPI(url, ip, types[i], http.MethodPost, outputCh, requestBody)
		}
	}
	for i := 0; i < count; i++ {
		resp := <-outputCh
		finalResult = append(finalResult, resp)
	}
	close(outputCh)
	return finalResult
}

func (ss *SshUserAccessCheck) getSShUserAPIRequest(ip string, sshUser *models.SSHUser) (models.SShUserRequest, error) {
	permisssion, err := ss.fileUtils.GetFilePermission(sshUser.PrivateKey)
	if err != nil {
		return models.SShUserRequest{}, err
	}
	if permisssion > 400 {
		return models.SShUserRequest{}, errors.New("Provide permission on the SSH key file as 400 (Read Only by Owner).")
	}
	ct, err := ss.fileUtils.ReadFile(sshUser.PrivateKey)
	if err != nil {
		ss.log.Errorf("Error while opening the private file path %s: %v\n", sshUser.PrivateKey, err)
		return models.SShUserRequest{}, err
	}
	keyContents := string(ct)
	return models.SShUserRequest{
		IP:           ip,
		Username:     sshUser.Username,
		Port:         sshUser.Port,
		SudoPassword: sshUser.SudoPassword,
		PrivateKey:   keyContents,
	}, nil
}

func (ss *SshUserAccessCheck) GetPortsForMockServer() map[string]map[string][]int {
	nodeTypePortMap := make(map[string]map[string][]int)
	return nodeTypePortMap
}

func IsSSHUserEmpty(sshUser *models.SSHUser) bool {
	return (sshUser.Username == "" || sshUser.Port == "" || sshUser.PrivateKey == "")
}
