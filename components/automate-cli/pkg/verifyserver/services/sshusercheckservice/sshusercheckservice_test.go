package sshusercheckservice

import (
	"errors"
	"testing"

	"github.com/bmizerany/assert"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/sshutils"
	"golang.org/x/crypto/ssh"
)

const (
	testPemFilePath     = "./testfiles/ssh"
	nodeIp              = "1.1.1.1"
	SuccessTitle        = "SSH user accessible"
	SuccessSudoPassword = "Sudo password valid"
	FailureSudoPassword = "Sudo password invalid"
	FailureTitle        = "Sudo password invalid"
	FailureSSHUser      = "SSH user unaccessible"
	SuccessResponse     = "SSH user is accessible for the node: 1.1.1.1"
	SuccessSudoResponse = "SSH user sudo password is valid for the node: 1.1.1.1"
)

func TestCheckSshUserDetails(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	ssu := NewSshUserCheckService(log, &fileutils.FileSystemUtils{}, &sshutils.SSHUtilImpl{})

	type args struct {
		req           *models.SshUserChecksRequest
		MockSSHUtil   sshutils.SSHUtil
		MockFileUtils fileutils.FileUtils
	}
	tests := []struct {
		description string
		args        args
		want        *models.SshUserChecksResponse
	}{
		{
			description: "User has correct access and correct password",
			args: args{
				req: &models.SshUserChecksRequest{
					Ip:           nodeIp,
					UserName:     "ubuntu",
					Port:         "22",
					PrivateKey:   testPemFilePath,
					SudoPassword: "123456",
				},
				MockSSHUtil: &sshutils.MockSSHUtilsImpl{
					SetSSHConfigfunc: func(sshConfig *sshutils.SSHConfig) {},
					GetConnectionfunc: func() (*ssh.Client, error) {
						return &ssh.Client{}, nil
					},
					ConnectAndExecuteCommandOnRemoteWithSudoPasswordfunc: func(s1 *sshutils.SSHConfig, s2, s3 string) (bool, error) {
						return true, nil
					},
				},
				MockFileUtils: &fileutils.MockFileSystemUtils{
					CreateTempFileFunc: func(content, filename string) (string, error) {
						return "", nil
					},
					DeleteTempFileFunc: func(tempFile string) error {
						return nil
					},
				},
			},
			want: &models.SshUserChecksResponse{
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
		},
		{
			description: "User is able to SSH but password provided is not supported",
			args: args{
				req: &models.SshUserChecksRequest{
					Ip:           nodeIp,
					UserName:     "ubuntu",
					Port:         "22",
					PrivateKey:   testPemFilePath,
					SudoPassword: "123456",
				},
				MockSSHUtil: &sshutils.MockSSHUtilsImpl{
					SetSSHConfigfunc: func(sshConfig *sshutils.SSHConfig) {},
					GetConnectionfunc: func() (*ssh.Client, error) {
						return &ssh.Client{}, nil
					},
					ConnectAndExecuteCommandOnRemoteWithSudoPasswordfunc: func(s1 *sshutils.SSHConfig, s2, s3 string) (bool, error) {
						return false, errors.New("Error while running cammand:Process exited with status 1")
					},
				},
				MockFileUtils: &fileutils.MockFileSystemUtils{
					CreateTempFileFunc: func(content, filename string) (string, error) {
						return "", nil
					},
					DeleteTempFileFunc: func(tempFile string) error {
						return nil
					},
				},
			},
			want: &models.SshUserChecksResponse{
				Passed: false,
				Checks: []models.Checks{
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
						ErrorMsg:      "SSH user sudo password is invalid for the node with IP 1.1.1.1",
						ResolutionMsg: "Ensure you have provided the correct sudo password and the user has sudo access on the node: 1.1.1.1",
					},
				},
			},
		},
	}
	for _, tt := range tests {
		ssu.sshUtil = tt.args.MockSSHUtil
		t.Run(tt.description, func(t *testing.T) {
			ssu.sshUtil = tt.args.MockSSHUtil
			got, _ := ssu.CheckSshUserDetails(tt.args.req)
			assert.Equal(t, got, tt.want)
		})
	}
}

func TestGetSshConnectionDetails(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	ssu := NewSshUserCheckService(log, &fileutils.MockFileSystemUtils{
		CreateTempFileFunc: func(content, filename string) (string, error) {
			return "", nil
		},
	}, &sshutils.SSHUtilImpl{})

	type args struct {
		SSHConfig   *sshutils.SSHConfig
		ip          string
		MockSSHUtil sshutils.SSHUtil
	}
	tests := []struct {
		description string
		args        args
		want        *models.Checks
	}{
		{
			description: "SSH Connection with the user is successfull",
			args: args{
				SSHConfig: &sshutils.SSHConfig{
					SshUser:    "ubuntu",
					SshPort:    "22",
					SshKeyFile: testPemFilePath,
					HostIP:     nodeIp,
					Timeout:    150,
				},
				ip: "1.1.1.1",
				MockSSHUtil: &sshutils.MockSSHUtilsImpl{
					SetSSHConfigfunc: func(sshConfig *sshutils.SSHConfig) {},
					GetConnectionfunc: func() (*ssh.Client, error) {
						return &ssh.Client{}, nil
					},
				},
			},
			want: &models.Checks{
				Title:         SuccessTitle,
				Passed:        true,
				SuccessMsg:    SuccessResponse,
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
		},
		{
			description: "SSH Connection failed with the given credentials",
			args: args{
				SSHConfig: &sshutils.SSHConfig{
					SshUser:    "ubuntu",
					SshPort:    "22",
					SshKeyFile: testPemFilePath,
					HostIP:     nodeIp,
					Timeout:    150,
				},
				ip: "1.1.1.1",
				MockSSHUtil: &sshutils.MockSSHUtilsImpl{
					SetSSHConfigfunc: func(sshConfig *sshutils.SSHConfig) {},
					GetConnectionfunc: func() (*ssh.Client, error) {
						return nil, errors.New("dial failed:ssh: handshake failed: ssh: unable to authenticate, attempted methods [none publickey], no supported methods remain")
					},
				},
			},
			want: &models.Checks{
				Title:         FailureSSHUser,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      "SSH user is unaccessible for the node with IP 1.1.1.1",
				ResolutionMsg: "Give SSH access to the user with the given key on the node: 1.1.1.1",
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			ssu.sshUtil = tt.args.MockSSHUtil
			got := ssu.GetSshConnectionDetails(tt.args.SSHConfig, tt.args.ip)
			assert.Equal(t, got, tt.want)
		})
	}

}

func TestGetSudoPasswordDetails(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	ssu := NewSshUserCheckService(log, &fileutils.MockFileSystemUtils{
		CreateTempFileFunc: func(content, filename string) (string, error) {
			return "", nil
		},
	}, &sshutils.SSHUtilImpl{})
	type args struct {
		SSHConfig    *sshutils.SSHConfig
		ip           string
		sudoPassword string
		MockSSHUtil  sshutils.SSHUtil
	}
	tests := []struct {
		description string
		args        args
		want        *models.Checks
	}{
		{
			description: "Sudo Password Entered has the access to the run sudo command on the host",
			args: args{
				SSHConfig: &sshutils.SSHConfig{
					SshUser:    "ubuntu",
					SshPort:    "22",
					SshKeyFile: testPemFilePath,
					HostIP:     nodeIp,
					Timeout:    150,
				},
				ip:           nodeIp,
				sudoPassword: "123456",
				MockSSHUtil: &sshutils.MockSSHUtilsImpl{
					ConnectAndExecuteCommandOnRemoteWithSudoPasswordfunc: func(s1 *sshutils.SSHConfig, s2, s3 string) (bool, error) {
						return true, nil
					},
				},
			},
			want: &models.Checks{
				Title:         SuccessSudoPassword,
				Passed:        true,
				SuccessMsg:    SuccessSudoResponse,
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
		},
		{
			description: "Sudo password provided was incorrect for the sudo command execution",
			args: args{
				SSHConfig: &sshutils.SSHConfig{
					SshUser:    "ubuntu",
					SshPort:    "22",
					SshKeyFile: testPemFilePath,
					HostIP:     nodeIp,
					Timeout:    150,
				},
				ip:           nodeIp,
				sudoPassword: "12345",
				MockSSHUtil: &sshutils.MockSSHUtilsImpl{
					ConnectAndExecuteCommandOnRemoteWithSudoPasswordfunc: func(s1 *sshutils.SSHConfig, s2, s3 string) (bool, error) {
						return false, errors.New("Error while running cammand:Process exited with status 1")
					},
				},
			},
			want: &models.Checks{
				Title:         FailureSudoPassword,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      "SSH user sudo password is invalid for the node with IP 1.1.1.1",
				ResolutionMsg: "Ensure you have provided the correct sudo password and the user has sudo access on the node: 1.1.1.1",
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			ssu.sshUtil = tt.args.MockSSHUtil
			got := ssu.GetSudoPasswordDetails(tt.args.SSHConfig, tt.args.ip, tt.args.sudoPassword)
			assert.Equal(t, got, tt.want)
		})
	}
}
