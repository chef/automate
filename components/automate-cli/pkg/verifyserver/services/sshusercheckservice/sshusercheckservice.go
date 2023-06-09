package sshusercheckservice

import (
	"fmt"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
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
		pemFileName: PEM_FILE_NAME,
		FileUtils:   fileutils,
	}
}

const (
	SUDO_PASSWORD_CMD = `echo "%s" | sudo -S ls -l`
	PEM_FILE_NAME     = "private_key.pem"
)

func (ssu *SshUserServiceImpl) CheckSshUserDetails(req *models.SshUserChecksRequest) (*models.ChecksResponse, error) {
	ssu.logger.Debugf("The Request value entered by the user ip: %v, port: %v, userName: %v ", req.Ip, req.Port, req.UserName)

	filePath, err := ssu.FileUtils.CreateTempFile(req.PrivateKey, PEM_FILE_NAME)
	if err != nil {
		ssu.logger.Error("Error while creating the key file on the desired file path: ", err)
		return nil, err
	}

	sshConfig := ssu.getConfig(req, filePath)

	sshCheckResponse, isCheckPassed := ssu.CheckSshConnection(sshConfig, req.Ip, req.SudoPassword)
	serviceResponse := &models.ChecksResponse{}
	serviceResponse.Passed = isCheckPassed
	serviceResponse.Checks = sshCheckResponse

	ssu.FileUtils.DeleteTempFile(filePath)
	return serviceResponse, nil
}

func (ssu *SshUserServiceImpl) CheckSshConnection(sshConfig sshutils.SSHConfig, ip string, sudoPassword string) ([]models.Checks, bool) {
	responseArray := []models.Checks{}
	command := fmt.Sprintf(SUDO_PASSWORD_CMD, sudoPassword)
	checkResponse, err := ssu.SshUtil.Execute(sshConfig, command)
	ssu.logger.Debug("The response after creating connection and executing command on the terminal: ", checkResponse)
	if err != nil {
		if strings.Contains(checkResponse, "Connection creation falied") {
			sshCheckResponse := failureResponse("SSH user unaccessible", "SSH user is unaccessible for the node with IP: "+ip, "Give SSH access to the user with the give key on the node: "+ip)
			sudoPasswordCheckResponse := failureResponse("Sudo password invalid", "SSH connection failed on the node so unable to check the sudo password for the node with IP: "+ip, "Ensure the correct credentials are provided for the SSH connection of node with IP: "+ip)
			responseArray = append(responseArray, *sshCheckResponse, *sudoPasswordCheckResponse)
			return responseArray, false
		}
		sshCheckResponse := successResponse("SSH user accessible", "SSH user is accessible for the node: "+ip)
		sudoPasswordCheckResponse := failureResponse("Sudo password invalid", "SSH user sudo password is invalid for the node with IP: "+ip, "Ensure you have provided the correct sudo password and the user has sudo access on the node: "+ip)
		responseArray = append(responseArray, *sshCheckResponse, *sudoPasswordCheckResponse)
		return responseArray, false
	}
	sshCheckResponse := successResponse("SSH user accessible", "SSH user is accessible for the node: "+ip)
	sudoPasswordCheckResponse := successResponse("Sudo password valid", "SSH user sudo password is valid for the node: "+ip)
	responseArray = append(responseArray, *sshCheckResponse, *sudoPasswordCheckResponse)
	return responseArray, true

}

func (ssu *SshUserServiceImpl) getConfig(req *models.SshUserChecksRequest, filePath string) sshutils.SSHConfig {
	sshConfig := sshutils.SSHConfig{
		SshUser:    req.UserName,
		SshPort:    req.Port,
		SshKeyFile: filePath,
		HostIP:     req.Ip,
		Timeout:    150,
	}
	return sshConfig
}

func successResponse(title string, successMsg string) *models.Checks {
	checkResponse := &models.Checks{
		Title:         title,
		Passed:        true,
		SuccessMsg:    successMsg,
		ErrorMsg:      "",
		ResolutionMsg: "",
	}
	return checkResponse
}

func failureResponse(title string, errorMsg string, resolutionMsg string) *models.Checks {
	checkResponse := &models.Checks{
		Title:         title,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      errorMsg,
		ResolutionMsg: resolutionMsg,
	}
	return checkResponse
}
