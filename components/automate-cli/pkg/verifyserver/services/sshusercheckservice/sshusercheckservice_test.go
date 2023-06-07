package sshusercheckservice

import (
	"errors"
	"reflect"
	"testing"

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
)

func TestCheckSshUserDetails(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	ssu := NewSshUserCheckService(log, &fileutils.MockFileSystemUtils{
		CreateTempFileFunc: func(content, filename string) (string, error) {
			return "", nil
		},
		DeleteTempFileFunc: func(tempFile string) error {
			return nil
		},
	}, &sshutils.SSHUtilImpl{})

	type args struct {
		req         *models.SshUserChecksRequest
		MockSSHUtil sshutils.SSHUtil
	}
	tests := []struct {
		description string
		args        args
		want        *models.SshUserChecksResponse
	}{
		{
			description: "SSH User has correct access and correct password",
			args: args{
				req: &models.SshUserChecksRequest{
					Ip:            nodeIp,
					User_Name:     "ubuntu",
					Port:          "22",
					Private_Key:   testPemFilePath,
					Sudo_Password: "123456",
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
			},
			want: &models.SshUserChecksResponse{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         SuccessTitle,
						Passed:        true,
						SuccessMsg:    "SSH user is accessible for the node: 1.1.1.1",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         SuccessSudoPassword,
						Passed:        true,
						SuccessMsg:    "SSH user sudo password is valid for the node: 1.1.1.1",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
		},
		{
			description: "If the user is able to SSH but password provied is not supported",
			args: args{
				req: &models.SshUserChecksRequest{
					Ip:            nodeIp,
					User_Name:     "ubuntu",
					Port:          "22",
					Private_Key:   testPemFilePath,
					Sudo_Password: "12345",
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
			},
			want: &models.SshUserChecksResponse{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         SuccessTitle,
						Passed:        true,
						SuccessMsg:    "SSH user is accessible for the node: 1.1.1.1",
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
			if got, _ := ssu.CheckSshUserDetails(tt.args.req); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("SshUserServiceImpl.GetSudoPasswordDetails() = %v, want %v", got, tt.want)
			}
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
			description: "If the SSH Connection with the user is successfull",
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
				SuccessMsg:    "SSH user is accessible for the node: 1.1.1.1",
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
		},
		{
			description: "If the SSH Connection failed",
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
			if got := ssu.GetSshConnectionDetails(tt.args.SSHConfig, tt.args.ip); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("SshUserServiceImpl.GetSudoPasswordDetails() = %v, want %v", got, tt.want)
			}
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
			description: "",
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
				SuccessMsg:    "SSH user sudo password is valid for the node: 1.1.1.1",
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
		},
		{
			description: "",
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
			if got := ssu.GetSudoPasswordDetails(tt.args.SSHConfig, tt.args.ip, tt.args.sudoPassword); !reflect.DeepEqual(got, tt.want) {
				t.Errorf("SshUserServiceImpl.GetSudoPasswordDetails() = %v, want %v", got, tt.want)
			}
		})
	}
}
