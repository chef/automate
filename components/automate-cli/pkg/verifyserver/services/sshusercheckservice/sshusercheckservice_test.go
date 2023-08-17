package sshusercheckservice_test

import (
	"errors"
	"testing"

	"github.com/bmizerany/assert"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/services/sshusercheckservice"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/sshutils"
)

const (
	testPemFilePath     = "./testfiles/ssh.pem"
	nodeIp              = "1.1.1.1"
	userName            = "ubuntu"
	port                = "22"
	sudoPassword        = "123456"
	SuccessTitle        = "SSH user accessible"
	SuccessSudoPassword = "Sudo password valid"
	FailureSudoPassword = "Sudo password invalid"
	FailureSSHUser      = "SSH user unaccessible"
	SuccessResponse     = "SSH user is accessible for the node: 1.1.1.1"
	SuccessSudoResponse = "SSH user sudo password is valid for the node: 1.1.1.1"
)

func TestCheckSshUserDetails(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	ssu := sshusercheckservice.NewSshUserCheckService(log, &fileutils.FileSystemUtils{}, &sshutils.SSHUtilImpl{})

	type args struct {
		req           *models.SshUserChecksRequest
		MockFileUtils fileutils.FileUtils
		MockSSHUtil   sshutils.SSHUtil
	}
	tests := []struct {
		description string
		args        args
		want        *models.ChecksResponse
		wantErr     error
	}{
		{
			description: "SSH Connection and Sudo password is correct",
			args: args{
				req: &models.SshUserChecksRequest{
					Ip:           nodeIp,
					UserName:     userName,
					Port:         port,
					PrivateKey:   testPemFilePath,
					SudoPassword: sudoPassword,
				},
				MockFileUtils: &fileutils.MockFileSystemUtils{
					CreateTempFileFunc: func(content, filename string) (string, error) {
						return "", nil
					},
					DeleteTempFileFunc: func(tempFile string) error {
						return nil
					},
				},
				MockSSHUtil: &sshutils.MockSSHUtilsImpl{
					Executefunc: func(sshConfig sshutils.SSHConfig, cmd string) (string, error) {
						return "", nil
					},
				},
			},
			want: &models.ChecksResponse{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         SuccessTitle,
						Passed:        true,
						SuccessMsg:    SuccessResponse,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         SuccessSudoPassword,
						Passed:        true,
						SuccessMsg:    SuccessSudoResponse,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			wantErr: nil,
		},
		{
			description: "Temp file creation fails",
			args: args{
				req: &models.SshUserChecksRequest{
					Ip:           nodeIp,
					UserName:     userName,
					Port:         port,
					PrivateKey:   testPemFilePath,
					SudoPassword: sudoPassword,
				},
				MockFileUtils: &fileutils.MockFileSystemUtils{
					CreateTempFileFunc: func(content, filename string) (string, error) {
						return "", errors.New("file creation failed ")
					},
					DeleteTempFileFunc: func(tempFile string) error {
						return nil
					},
				},
				MockSSHUtil: &sshutils.MockSSHUtilsImpl{
					Executefunc: func(sshConfig sshutils.SSHConfig, cmd string) (string, error) {
						return "", nil
					},
				},
			},
			want:    nil,
			wantErr: errors.New("file creation failed "),
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			ssu.FileUtils = tt.args.MockFileUtils
			ssu.SshUtil = tt.args.MockSSHUtil
			got, err := ssu.CheckSshUserDetails(tt.args.req)
			assert.Equal(t, got, tt.want)
			assert.Equal(t, err, tt.wantErr)
		})
	}
}

func TestCheckSshConnection(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	ssu := sshusercheckservice.NewSshUserCheckService(log, &fileutils.MockFileSystemUtils{}, &sshutils.SSHUtilImpl{})

	type args struct {
		SSHConfig    sshutils.SSHConfig
		ip           string
		sudoPassword string
		MockSSHUtil  sshutils.SSHUtil
	}
	tests := []struct {
		description        string
		args               args
		want               []models.Checks
		wantPassedResponse bool
	}{
		{
			description: "The connection and cammand execution passes on the Remote Host",
			args: args{
				SSHConfig: sshutils.SSHConfig{
					SshUser:    "ubuntu",
					SshPort:    "22",
					SshKeyFile: testPemFilePath,
					HostIP:     nodeIp,
					Timeout:    150,
				},
				ip:           nodeIp,
				sudoPassword: sudoPassword,
				MockSSHUtil: &sshutils.MockSSHUtilsImpl{
					Executefunc: func(sshConfig sshutils.SSHConfig, cmd string) (string, error) {
						return "", nil
					},
				},
			},
			want: []models.Checks{
				{
					Title:         SuccessTitle,
					Passed:        true,
					SuccessMsg:    SuccessResponse,
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         SuccessSudoPassword,
					Passed:        true,
					SuccessMsg:    SuccessSudoResponse,
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
			},
			wantPassedResponse: true,
		},
		{
			description: "SSH Connection fails",
			args: args{
				SSHConfig: sshutils.SSHConfig{
					SshUser:    "wrongUsername",
					SshPort:    "22",
					SshKeyFile: testPemFilePath,
					HostIP:     nodeIp,
					Timeout:    150,
				},
				ip:           nodeIp,
				sudoPassword: sudoPassword,
				MockSSHUtil: &sshutils.MockSSHUtilsImpl{
					Executefunc: func(sshConfig sshutils.SSHConfig, cmd string) (string, error) {
						return "Connection creation failed", errors.New("ssh: unable to authenticate, attempted methods [none publickey], no supported methods remain")
					},
				},
			},
			want: []models.Checks{
				{
					Title:         FailureSSHUser,
					Passed:        false,
					SuccessMsg:    "",
					ErrorMsg:      "SSH user is unaccessible for the node with IP: " + nodeIp,
					ResolutionMsg: "Give SSH access to the user with the given key on the node: " + nodeIp,
				},
				{
					Title:         FailureSudoPassword,
					Passed:        false,
					SuccessMsg:    "",
					ErrorMsg:      "SSH connection failed on the node so unable to check the sudo password for the node with IP: " + nodeIp,
					ResolutionMsg: "Ensure the correct credentials are provided for the SSH connection of node with IP: " + nodeIp,
				},
			},
			wantPassedResponse: false,
		},
		{
			description: "SSH Connecton was success but the session creation got failed",
			args: args{
				SSHConfig: sshutils.SSHConfig{
					SshUser:    "wrongUsername",
					SshPort:    "22",
					SshKeyFile: testPemFilePath,
					HostIP:     nodeIp,
					Timeout:    150,
				},
				ip:           nodeIp,
				sudoPassword: sudoPassword,
				MockSSHUtil: &sshutils.MockSSHUtilsImpl{
					Executefunc: func(sshConfig sshutils.SSHConfig, cmd string) (string, error) {
						return "Session creation failed", errors.New("Session creation failed for the remote host")
					},
				},
			},
			want: []models.Checks{
				{
					Title:         SuccessTitle,
					Passed:        true,
					SuccessMsg:    SuccessResponse,
					ErrorMsg:      "",
					ResolutionMsg: "",
				},
				{
					Title:         FailureSudoPassword,
					Passed:        false,
					SuccessMsg:    "",
					ErrorMsg:      "SSH user sudo password is invalid for the node with IP: " + nodeIp,
					ResolutionMsg: "Ensure you have provided the correct sudo password and the user has sudo access on the node: " + nodeIp,
				},
			},
			wantPassedResponse: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			ssu.SshUtil = tt.args.MockSSHUtil
			got, passed := ssu.CheckSshConnection(tt.args.SSHConfig, tt.args.ip, tt.args.sudoPassword)
			assert.Equal(t, got, tt.want)
			assert.Equal(t, passed, tt.wantPassedResponse)
		})
	}
}
