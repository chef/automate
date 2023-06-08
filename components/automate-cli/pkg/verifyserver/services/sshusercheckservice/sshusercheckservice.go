package sshusercheckservice

import (
	"fmt"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/sshutils"
)

type SshUserCheckService interface {
	CheckSshUserDetails(*models.SshUserChecksRequest) (*models.SshUserChecksResponse, error)
}

type SshUserServiceImpl struct {
	logger      logger.Logger
	sshUtil     sshutils.SSHUtil
	pemFileName string
	fileUtils   fileutils.FileUtils
}

func NewSshUserCheckService(logger logger.Logger, fileutils fileutils.FileUtils, sshUtils sshutils.SSHUtil) *SshUserServiceImpl {
	return &SshUserServiceImpl{
		logger:      logger,
		sshUtil:     sshUtils,
		pemFileName: PEM_FILE_NAME,
		fileUtils:   fileutils,
	}
}

const (
	SUDO_PASSWORD_CMD = `echo "%s" | sudo -S ls -l`
	PEM_FILE_NAME     = "private_key"
)

func (ssu *SshUserServiceImpl) CheckSshUserDetails(req *models.SshUserChecksRequest) (*models.SshUserChecksResponse, error) {
	ssu.logger.Debug("The Request Value for the SSH User Check: ", req.Ip, req.Port, req.UserName)
	
	filePath, err := ssu.fileUtils.CreateTempFile(req.PrivateKey, PEM_FILE_NAME)
	if err != nil {
		ssu.logger.Error("Error while creating the key file on the desired file path: ", err)
		return nil, err
	}
	sshConfig, err := ssu.getConfig(req, filePath)
	if err != nil {
		ssu.logger.Error("Error in getting the SSHConfig for SSH connection: ", err)
		return nil, err
	}
	checkSshConnectionResponse := ssu.GetSshConnectionDetails(&sshConfig, req.Ip)

	checkSudoResponse := ssu.GetSudoPasswordDetails(&sshConfig, req.Ip, req.SudoPassword)

	serviceResponse := &models.SshUserChecksResponse{}
	serviceResponse.Passed = true
	if !checkSshConnectionResponse.Passed || !checkSudoResponse.Passed {
		serviceResponse.Passed = false
	}
	serviceResponse.Checks = append(serviceResponse.Checks, *checkSshConnectionResponse, *checkSudoResponse)
	ssu.fileUtils.DeleteTempFile(filePath)
	return serviceResponse, nil
}

func (ssu *SshUserServiceImpl) GetSshConnectionDetails(sshConfig *sshutils.SSHConfig, ip string) *models.Checks {
	ssu.sshUtil.SetSSHConfig(sshConfig)

	sshCheckResponse, err := ssu.sshUtil.GetConnection()

	ssu.logger.Debug("The connection establishment was successfully:", sshCheckResponse)

	if err != nil {
		ssu.logger.Error("Error while establishing the connection on the remote host: ", err)
		return failureResponse("SSH user unaccessible", "SSH user is unaccessible for the node with IP "+fmt.Sprintf("%v", ip), "Give SSH access to the user with the given key on the node: "+fmt.Sprintf("%v", ip))
	}
	return successResponse("SSH user accessible", "SSH user is accessible for the node: "+fmt.Sprintf("%v", ip))
}

func (ssu *SshUserServiceImpl) GetSudoPasswordDetails(sshConfig *sshutils.SSHConfig, ip string, sudoPassword string) *models.Checks {
	sudoCheckResponse, err := ssu.sshUtil.ConnectAndExecuteCommandOnRemoteWithSudoPassword(sshConfig, sudoPassword, SUDO_PASSWORD_CMD)

	ssu.logger.Debug("The sudo command execution on Remote response:", sudoCheckResponse)
	if err != nil {
		return failureResponse("Sudo password invalid", "SSH user sudo password is invalid for the node with IP "+fmt.Sprintf("%v", ip), "Ensure you have provided the correct sudo password and the user has sudo access on the node: "+fmt.Sprintf("%v", ip))
	}
	return successResponse("Sudo password valid", "SSH user sudo password is valid for the node: "+fmt.Sprintf("%v", ip))
}

func (ssu *SshUserServiceImpl) getConfig(req *models.SshUserChecksRequest, filePath string) (sshutils.SSHConfig, error) {
	sshConfig := &sshutils.SSHConfig{
		SshUser:    req.UserName,
		SshPort:    req.Port,
		SshKeyFile: filePath,
		HostIP:     req.Ip,
		Timeout:    150,
	}
	return *sshConfig, nil
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
