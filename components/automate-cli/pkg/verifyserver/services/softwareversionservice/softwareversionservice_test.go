package softwareversionservice

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

var osTestVersion = map[string][]string{
	"Red Hat Linux": {"7, 8, 9"},
	"Ubuntu":        {"16.04.x", "18.04.x", "20.04.x", "22.04.x"},
	"Centos":        {"7"},
	"Amazon Linux":  {"2"},
	"SUSE Linux":    {"12.5"},
	"Debian":        {"9", "10", "11", "12"},
}

const (
	osReleaseRedHatFail  = "./testfiles/os_release_red_hat_failure.txt"
	osReleaseUbuntuFail  = "./testfiles/os_release_ubuntu_failure.txt"
	osReleaseDebian      = "./testfiles/os_release_debian.txt"
	osReleaseSuse        = "./testfiles/os_release_suse.txt"
	osReleaseAmazonLinux = "./testfiles/os_release_amazon_linux.txt"
	osReleaseRedHat      = "./testfiles/os_release_red_hat.txt"
	osReleaseCentos      = "./testfiles/os_release_centos.txt"
	successfile          = "./testfiles/os_release_ubuntu.txt"
	failurefile          = "./testfiles/failure.txt"
	versionfile          = "./testfiles/os_release_suse_failure.txt"
	failfilepath         = "./failfilepath"
	LinuxVersionTitle    = "Linux Version Check"
	KernalVersionTitle   = "Kernal Version Check"
	MkdirTitle           = "mkdir availability"
	OpensslTitle         = "openssl availability"
	StatTitle            = "stat availability"
	MkdirIsAvailable     = "mkdir is available"
	OpensslIsAvailable   = "openssl is available"
	StatIsAvailable      = "stat is available"
	successKernelfile    = "./testfiles/successkernel.txt"
	failureKernelfile    = "./testfiles/failurekernel.txt"
)

var (
	checktrue  = []string{"mkdir"}
	checkfalse = []string{"wrong-cammand"}
)

func TestGetSoftwareVersionDetails(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	service := NewSoftwareVersionService(log, func(cmd string) (string, error) {
		return "", nil
	})
	type args struct {
		query          string
		checkarray     []string
		osFilepath     string
		kernelFilepath string
	}
	tests := []struct {
		description  string
		args         args
		expectedBody *models.SoftwareVersionDetails
	}{
		{
			description: "If the query parameter is postgres",
			args: args{
				query:          "postgresql",
				checkarray:     checktrue,
				osFilepath:     successfile,
				kernelFilepath: successKernelfile,
			},
			expectedBody: &models.SoftwareVersionDetails{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         StatTitle,
						Passed:        true,
						SuccessMsg:    StatIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         MkdirTitle,
						Passed:        true,
						SuccessMsg:    MkdirIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         KernalVersionTitle,
						Passed:        true,
						SuccessMsg:    "Linux kernal version is 5.10",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         LinuxVersionTitle,
						Passed:        true,
						SuccessMsg:    "Ubuntu version is 20.04",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
		},
		{
			description: "If the query parameter is opensearch",
			args: args{
				query:          "opensearch",
				checkarray:     checktrue,
				osFilepath:     successfile,
				kernelFilepath: successKernelfile,
			},
			expectedBody: &models.SoftwareVersionDetails{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         OpensslTitle,
						Passed:        true,
						SuccessMsg:    OpensslIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         MkdirTitle,
						Passed:        true,
						SuccessMsg:    MkdirIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         KernalVersionTitle,
						Passed:        true,
						SuccessMsg:    "Linux kernal version is 5.10",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         LinuxVersionTitle,
						Passed:        true,
						SuccessMsg:    "Ubuntu version is 20.04",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
		},
		{
			description: "If the query parameter is automate, chef-server or Bastion",
			args: args{
				query:          "automate",
				checkarray:     checktrue,
				osFilepath:     successfile,
				kernelFilepath: successKernelfile,
			},
			expectedBody: &models.SoftwareVersionDetails{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         MkdirTitle,
						Passed:        true,
						SuccessMsg:    MkdirIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         KernalVersionTitle,
						Passed:        true,
						SuccessMsg:    "Linux kernal version is 5.10",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         LinuxVersionTitle,
						Passed:        true,
						SuccessMsg:    "Ubuntu version is 20.04",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
		},
		{
			description: "If the query parameter is file is wrong",
			args: args{
				query:          "postgresql",
				checkarray:     checkfalse,
				osFilepath:     successfile,
				kernelFilepath: successKernelfile,
			},
			expectedBody: &models.SoftwareVersionDetails{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         StatTitle,
						Passed:        true,
						SuccessMsg:    StatIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "wrong-cammand availability",
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      "wrong-cammand is not available",
						ResolutionMsg: "Ensure wrong-cammand is available in $PATH on the node",
					},
					{
						Title:         KernalVersionTitle,
						Passed:        true,
						SuccessMsg:    "Linux kernal version is 5.10",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         LinuxVersionTitle,
						Passed:        true,
						SuccessMsg:    "Ubuntu version is 20.04",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
		},
		{
			description: "If the file is not avalable for OS Version ",
			args: args{
				query:          "postgresql",
				checkarray:     checktrue,
				osFilepath:     failfilepath,
				kernelFilepath: successKernelfile,
			},
			expectedBody: &models.SoftwareVersionDetails{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         StatTitle,
						Passed:        true,
						SuccessMsg:    StatIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         MkdirTitle,
						Passed:        true,
						SuccessMsg:    MkdirIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         KernalVersionTitle,
						Passed:        true,
						SuccessMsg:    "Linux kernal version is 5.10",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         LinuxVersionTitle,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      "Its not feasible to determine the Operating system version",
						ResolutionMsg: "Please run automate on the supported platforms.",
					},
				},
			},
		},
		{
			description: "If the os version is not supported by automate",
			args: args{
				query:          "postgresql",
				checkarray:     checktrue,
				osFilepath:     failurefile,
				kernelFilepath: successKernelfile,
			},
			expectedBody: &models.SoftwareVersionDetails{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         StatTitle,
						Passed:        true,
						SuccessMsg:    StatIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         MkdirTitle,
						Passed:        true,
						SuccessMsg:    MkdirIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         KernalVersionTitle,
						Passed:        true,
						SuccessMsg:    "Linux kernal version is 5.10",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         LinuxVersionTitle,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      "Kali Linux version is not supported by automate",
						ResolutionMsg: "Ensure Kali Linux correct version is installed on the node",
					},
				},
			},
		},
		{
			description: "If the kernal version is not supported by automate",
			args: args{
				query:          "postgresql",
				checkarray:     checktrue,
				osFilepath:     successfile,
				kernelFilepath: failureKernelfile,
			},
			expectedBody: &models.SoftwareVersionDetails{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         StatTitle,
						Passed:        true,
						SuccessMsg:    StatIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         MkdirTitle,
						Passed:        true,
						SuccessMsg:    MkdirIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         KernalVersionTitle,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      "Linux kernel version is lower than 3.2",
						ResolutionMsg: "Use a linux version whose kernel version is greater than 3.2",
					},
					{
						Title:         LinuxVersionTitle,
						Passed:        true,
						SuccessMsg:    "Ubuntu version is 20.04",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
		},
		{
			description: "If file is not present for reading the kernal version",
			args: args{
				query:          "postgresql",
				checkarray:     checktrue,
				osFilepath:     successfile,
				kernelFilepath: failfilepath,
			},
			expectedBody: &models.SoftwareVersionDetails{
				Passed: false,
				Checks: []models.Checks{
					{
						Title:         StatTitle,
						Passed:        true,
						SuccessMsg:    StatIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         MkdirTitle,
						Passed:        true,
						SuccessMsg:    MkdirIsAvailable,
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         KernalVersionTitle,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      "Its not feasible to determine the Kernal version of the system",
						ResolutionMsg: "Please run automate on the supported platforms.",
					},
					{
						Title:         LinuxVersionTitle,
						Passed:        true,
						SuccessMsg:    "Ubuntu version is 20.04",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
		},
		{
			description: "If the entered Query is not supported by us",
			args: args{
				query:          "wrong-query",
				checkarray:     checktrue,
				osFilepath:     successfile,
				kernelFilepath: successKernelfile,
			},
			expectedBody: nil,
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			service.osFilePath = tt.args.osFilepath
			service.kernelFilePath = tt.args.kernelFilepath
			service.cmdCheckArray = tt.args.checkarray
			got, _ := service.GetSoftwareVersionDetails(tt.args.query)
			assert.Equal(t, got, tt.expectedBody)
		})
	}
}
func TestCheckOs(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sv := NewSoftwareVersionService(log, func(cmd string) (string, error) {
		return "", nil
	})
	type args struct {
		osVersions map[string][]string
		version    string
		key        string
	}
	tests := []struct {
		description  string
		args         args
		expectedBody bool
	}{
		{
			description: "If the os is Ubuntu",
			args: args{
				osVersions: osTestVersion,
				version:    "20.04",
				key:        "Ubuntu",
			},
			expectedBody: true,
		},
		{
			description: "If the os is Red Hat OS",
			args: args{
				osVersions: osTestVersion,
				version:    "8",
				key:        "Red Hat",
			},
			expectedBody: true,
		},
		{
			description: "If the OS is debian",
			args: args{
				osVersions: osTestVersion,
				version:    "10",
				key:        "Debian",
			},
			expectedBody: true,
		},
		{
			description: "If the os is out of other three that is Centos, AmazonLinux or SUSE LINUX",
			args: args{
				osVersions: osTestVersion,
				version:    "2",
				key:        "Amazon Linux",
			},
			expectedBody: true,
		},
		{
			description: "If the OS is not supported",
			args: args{
				osVersions: osTestVersion,
				version:    "10",
				key:        "Centos",
			},
			expectedBody: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			got := sv.checkOs(tt.args.osVersions, tt.args.version, tt.args.key)
			assert.Equal(t, got, tt.expectedBody)

		})
	}
}

func TestCheckCommandVersion(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sv := NewSoftwareVersionService(log, func(cmd string) (string, error) {
		return "", nil
	})
	type args struct {
		cmdName string
	}
	tests := []struct {
		description  string
		args         args
		expectedBody *models.Checks
	}{
		{
			description: "If the cammand is present in the node",
			args: args{
				cmdName: "mkdir",
			},
			expectedBody: &models.Checks{
				Title:         MkdirTitle,
				Passed:        true,
				SuccessMsg:    MkdirIsAvailable,
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
		},
		{
			description: "If the cammand is not present in the node",
			args: args{
				cmdName: "abc",
			},
			expectedBody: &models.Checks{
				Title:         "abc availability",
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      "abc is not available",
				ResolutionMsg: "Ensure abc is available in $PATH on the node",
			},
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			got := sv.checkCommandVersion(tt.args.cmdName)
			assert.Equal(t, got, tt.expectedBody)

		})
	}
}

func TestCheckOsVersion(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sv := NewSoftwareVersionService(log, func(cmd string) (string, error) {
		return "", nil
	})
	type args struct {
		osFilepath string
	}
	tests := []struct {
		description  string
		args         args
		expectedBody *models.Checks
		wantErr      bool
		expectedErr  string
	}{
		{
			description: "If the OS is not compatble with the automate",
			args: args{
				osFilepath: failurefile,
			},
			expectedBody: &models.Checks{
				Title:         LinuxVersionTitle,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      "Kali Linux version is not supported by automate",
				ResolutionMsg: "Ensure Kali Linux correct version is installed on the node",
			},
		},
		{
			description: "If the os and version both are correct for Ubuntu",
			args: args{
				osFilepath: successfile,
			},
			expectedBody: &models.Checks{
				Title:         LinuxVersionTitle,
				Passed:        true,
				SuccessMsg:    "Ubuntu version is 20.04",
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			wantErr:     false,
			expectedErr: "",
		},
		{
			description: "If the os and version both are correct for red_hat",
			args: args{
				osFilepath: osReleaseRedHat,
			},
			expectedBody: &models.Checks{
				Title:         "Linux Version Check",
				Passed:        true,
				SuccessMsg:    "Red Hat version is 9.2",
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			wantErr:     false,
			expectedErr: "",
		},
		{
			description: "If the os and version both are correct for centos",
			args: args{
				osFilepath: osReleaseCentos,
			},
			expectedBody: &models.Checks{
				Title:         "Linux Version Check",
				Passed:        true,
				SuccessMsg:    "CentOS version is 7",
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			wantErr:     false,
			expectedErr: "",
		},
		{
			description: "If the os and version both are correct for amazon linux",
			args: args{
				osFilepath: osReleaseAmazonLinux,
			},
			expectedBody: &models.Checks{
				Title:         "Linux Version Check",
				Passed:        true,
				SuccessMsg:    "Amazon Linux version is 2",
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			wantErr:     false,
			expectedErr: "",
		},
		{
			description: "If the os and version both are correct for debian",
			args: args{
				osFilepath: osReleaseDebian,
			},
			expectedBody: &models.Checks{
				Title:         "Linux Version Check",
				Passed:        true,
				SuccessMsg:    "Debian version is 11",
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			wantErr:     false,
			expectedErr: "",
		},
		{
			description: "If the os and version both are correct for SUSE",
			args: args{
				osFilepath: osReleaseSuse,
			},
			expectedBody: &models.Checks{
				Title:         "Linux Version Check",
				Passed:        true,
				SuccessMsg:    "SUSE Linux version is 12.5",
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			wantErr:     false,
			expectedErr: "",
		},
		{
			description: "If the os name is correct but version is incorrect for SUSE",
			args: args{
				osFilepath: versionfile,
			},
			expectedBody: &models.Checks{
				Title:         LinuxVersionTitle,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      "SUSE Linux version is not supported by automate",
				ResolutionMsg: "Ensure SUSE Linux correct version is installed on the node",
			},
			wantErr:     false,
			expectedErr: "",
		},
		{
			description: "If the os name is correct but version is incorrect for Ubuntu",
			args: args{
				osFilepath: osReleaseUbuntuFail,
			},
			expectedBody: &models.Checks{
				Title:         LinuxVersionTitle,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      "Ubuntu version is not supported by automate",
				ResolutionMsg: "Ensure Ubuntu correct version is installed on the node",
			},
			wantErr:     false,
			expectedErr: "",
		},
		{
			description: "If the os name is correct but version is incorrect for Red Hat",
			args: args{
				osFilepath: osReleaseRedHatFail,
			},
			expectedBody: &models.Checks{
				Title:         LinuxVersionTitle,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      "Red Hat version is not supported by automate",
				ResolutionMsg: "Ensure Red Hat correct version is installed on the node",
			},
			wantErr:     false,
			expectedErr: "",
		},
		{
			description: "If not able to get the OS Details from the filepath",
			args: args{
				osFilepath: failfilepath,
			},
			expectedBody: &models.Checks{
				Title:         LinuxVersionTitle,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      "Its not feasible to determine the Operating system version",
				ResolutionMsg: "Please run automate on the supported platforms.",
			},
			wantErr:     true,
			expectedErr: "open ./failfilepath: no such file or directory",
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			got, _, err := sv.checkOsVersion(tt.args.osFilepath)
			if tt.wantErr {
				assert.Equal(t, tt.expectedErr, err.Error())
				return
			} else {
				assert.Equal(t, tt.expectedBody, got)
			}
		})
	}
}

func TestCheckKernelVersion(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	sv := NewSoftwareVersionService(log, func(cmd string) (string, error) {
		return "", nil
	})
	type args struct {
		kernelFilePath string
		osname         string
	}
	tests := []struct {
		description  string
		args         args
		expectedBody *models.Checks
		wantErr      bool
		expectedErr  string
	}{
		{
			description: "If the Kernal version is supported",
			args: args{
				kernelFilePath: successKernelfile,
				osname:         AMAZON_LINUX,
			},
			expectedBody: &models.Checks{
				Title:         "Kernal Version Check",
				Passed:        true,
				SuccessMsg:    "Linux kernal version is 5.10",
				ErrorMsg:      "",
				ResolutionMsg: "",
			},
			wantErr:     false,
			expectedErr: "",
		},
		{
			description: "If the Kernal Version entered is not supported suse linux",
			args: args{
				kernelFilePath: failureKernelfile,
				osname:         SUSE_LINUX,
			},
			expectedBody: &models.Checks{
				Title:         KernalVersionTitle,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      "Linux kernel version is lower than 3.2",
				ResolutionMsg: "Use a linux version whose kernel version is greater than 3.2",
			},
			wantErr:     false,
			expectedErr: "",
		},
		{
			description: "If the Kernal Version entered is not supported Amazon linux",
			args: args{
				kernelFilePath: failureKernelfile,
				osname:         AMAZON_LINUX,
			},
			expectedBody: &models.Checks{
				Title:  "Kernal Version Check",
				Passed: false, SuccessMsg: "",
				ErrorMsg:      "Linux kernel version is not equal to 5.10",
				ResolutionMsg: "Use a linux version whose kernel version is equal to 5.10",
			},
			wantErr:     false,
			expectedErr: "",
		},
		{
			description: "If not able to get the kernal Version",
			args: args{
				kernelFilePath: failfilepath,
				osname:         AMAZON_LINUX,
			},
			expectedBody: &models.Checks{},
			wantErr:      true,
			expectedErr:  "open ./failfilepath: no such file or directory",
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			got, err := sv.checkKernelVersion(tt.args.kernelFilePath, tt.args.osname)
			if tt.wantErr {
				assert.Equal(t, tt.expectedErr, err.Error())
				return
			}
			assert.Equal(t, tt.expectedBody, got)
		})
	}
}
