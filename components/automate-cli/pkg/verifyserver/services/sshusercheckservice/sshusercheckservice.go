package sshusercheckservice

import (
	"fmt"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/sshutils"
)

type SshUserCheckService interface {
	CheckSshUserDetails(*models.SshUserChecksRequest) (*models.ChecksResponse, error)
}

type SshUserServiceImpl struct {
	logger      logger.Logger
	SshUtil     sshutils.SSHUtil
	pemFileName string
	FileUtils   fileutils.FileUtils
}

func NewSshUserCheckService(logger logger.Logger, fileutils fileutils.FileUtils, sshUtils sshutils.SSHUtil) *SshUserServiceImpl {
	return &SshUserServiceImpl{
		logger:      logger,
		SshUtil:     sshUtils,
		pemFileName: constants.PEM_FILE_NAME,
		FileUtils:   fileutils,
	}
}

func (ssu *SshUserServiceImpl) CheckSshUserDetails(req *models.SshUserChecksRequest) (*models.ChecksResponse, error) {
	ssu.logger.Debugf("The Request value entered by the user ip: %v, port: %v, userName: %v ", req.Ip, req.Port, req.UserName)

	filePath, err := ssu.FileUtils.CreateTempFile(req.PrivateKey, constants.PEM_FILE_NAME)
	if err != nil {
		ssu.logger.Error("Error while creating the key file on the desired file path: ", err)
		return nil, err
	}

	sshConfig := sshutils.NewSshConfig(req.Ip, req.Port, filePath, req.UserName)

	sshCheckResponse, isCheckPassed := ssu.CheckSshConnection(sshConfig, req.Ip, req.SudoPassword)
	serviceResponse := &models.ChecksResponse{}
	serviceResponse.Passed = isCheckPassed
	serviceResponse.Checks = sshCheckResponse

	ssu.FileUtils.DeleteTempFile(filePath)
	return serviceResponse, nil
}

func (ssu *SshUserServiceImpl) CheckSshConnection(sshConfig sshutils.SSHConfig, ip string, sudoPassword string) ([]models.Checks, bool) {
	responseArray := []models.Checks{}
	command := fmt.Sprintf(constants.SUDO_PASSWORD_CMD, sudoPassword)
	checkResponse, err := ssu.SshUtil.Execute(sshConfig, command)
	ssu.logger.Debug("The response after creating connection and executing command on the terminal: ", checkResponse)
	if err != nil {
		if strings.Contains(checkResponse, "Connection creation failed") {
			sshCheckResponse := models.NewFailureCheck(constants.SSH_USER_FAILURE_TITLE, constants.SSH_USER_ERROR_MESSAGE+ip, constants.SSH_USER_RESOLUTION_MESSAGE+ip)
			sudoPasswordCheckResponse := models.NewFailureCheck(constants.SUDO_PASSWORD_FAILURE_TITLE, constants.SUDO_PASSWORD_CONNECTION_ERROR_MESSAGE+ip, constants.SUDO_PASSWORD_CONNECTION_RESOLUTION_MESSAGE+ip)
			responseArray = append(responseArray, *sshCheckResponse, *sudoPasswordCheckResponse)
			return responseArray, false
		}
		sshCheckResponse := models.NewSuccessCheck(constants.SSH_USER_SUCCESS_TITLE, constants.SSH_USER_SUCCESS_MESSAGE+ip)
		sudoPasswordCheckResponse := models.NewFailureCheck(constants.SUDO_PASSWORD_FAILURE_TITLE, constants.SUDO_PASSWORD_ERROR_MESSAGE+ip, constants.SUDO_PASSWORD_FAILURE_RESOLUTION_MESSAGE+ip)
		responseArray = append(responseArray, *sshCheckResponse, *sudoPasswordCheckResponse)
		return responseArray, false
	}
	sshCheckResponse := models.NewSuccessCheck(constants.SSH_USER_SUCCESS_TITLE, constants.SSH_USER_SUCCESS_MESSAGE+ip)
	sudoPasswordCheckResponse := models.NewSuccessCheck(constants.SUDO_PASSWORD_TITLE, constants.SUDO_PASSWORD_SUCCESS_MESSAGE+ip)
	responseArray = append(responseArray, *sshCheckResponse, *sudoPasswordCheckResponse)
	return responseArray, true

}
