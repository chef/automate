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
	"SUSE Linux":    {"12"},
	"Debian":        {"9", "10", "11", "12"},
}

const (
	successfile        = "./testfiles/success.txt"
	failurefile        = "./testfiles/failure.txt"
	versionfile        = "./testfiles/version.txt"
	failfilepath       = "./failfilepath"
	LinuxVersionTitle  = "Linux Version Check"
	MkdirTitle         = "mkdir availability"
	OpensslTitle       = "openssl availability"
	StatTitle          = "stat availability"
	MkdirIsAvailable   = "mkdir is available"
	OpensslIsAvailable = "openssl is available"
	StatIsAvailable    = "stat is available"
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
		query      string
		checkarray []string
		osFilepath string
	}
	tests := []struct {
		description   string
		args          args
		expectedBody  *models.SoftwareVersionDetails
		expectedError string
	}{
		{
			description: "If the query parameter is postgres",
			args: args{
				query:      "postgres",
				checkarray: checktrue,
				osFilepath: successfile,
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
						Title:         LinuxVersionTitle,
						Passed:        true,
						SuccessMsg:    "Ubuntu version is 20.04",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			expectedError: "",
		},
		{
			description: "If the query parameter is opensearch",
			args: args{
				query:      "opensearch",
				checkarray: checktrue,
				osFilepath: successfile,
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
						Title:         LinuxVersionTitle,
						Passed:        true,
						SuccessMsg:    "Ubuntu version is 20.04",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			expectedError: "",
		},
		{
			description: "If the query parameter is automate, chef-server or Bastion",
			args: args{
				query:      "automate",
				checkarray: checktrue,
				osFilepath: successfile,
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
						Title:         LinuxVersionTitle,
						Passed:        true,
						SuccessMsg:    "Ubuntu version is 20.04",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			expectedError: "",
		},
		{
			description: "If the query parameter is file is wrong",
			args: args{
				query:      "postgres",
				checkarray: checkfalse,
				osFilepath: successfile,
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
						Title:         LinuxVersionTitle,
						Passed:        true,
						SuccessMsg:    "Ubuntu version is 20.04",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
				},
			},
			expectedError: "",
		},
		{
			description: "If the file is not avalable for OS Version ",
			args: args{
				query:      "postgres",
				checkarray: checktrue,
				osFilepath: failfilepath,
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
						Title:         LinuxVersionTitle,
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      "Its not feasible to determine the Operating system version",
						ResolutionMsg: "Please run system on the supported platforms.",
					},
				},
			},
			expectedError: "",
		},
		{
			description: "If the os version is not supported by automate",
			args: args{
				query:      "postgres",
				checkarray: checktrue,
				osFilepath: failurefile,
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
			description: "If the entered Query is not supported by us",
			args: args{
				query:      "wrong-query",
				checkarray: checktrue,
				osFilepath: successfile,
			},
			expectedBody:  nil,
			expectedError: "The query wrong-query is not supported. The Supported query's are: postgres, opensearch, bastion, automate, chef-server",
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			service.(*SoftwareVersionService).osFilepath = tt.args.osFilepath
			service.(*SoftwareVersionService).cmdCheckArray = tt.args.checkarray
			got, err := service.GetSoftwareVersionDetails(tt.args.query)
			if err != nil {
				assert.Equal(t, tt.expectedError, err.Error())
			} else {
				assert.Equal(t, got, tt.expectedBody)
			}
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
				version:    "20.04.65",
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
			got := sv.(*SoftwareVersionService).checkOs(tt.args.osVersions, tt.args.version, tt.args.key)
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
			got := sv.(*SoftwareVersionService).checkCommandVersion(tt.args.cmdName)
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
		expectedErr  bool
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
			description: "If the os and version both are correct",
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
			expectedErr: false,
		},
		{
			description: "If the os name is correct but version is incorrect",
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
			expectedErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			got, err := sv.(*SoftwareVersionService).checkOsVersion(tt.args.osFilepath)
			if (err != nil) != tt.expectedErr {
				assert.Equal(t, err, tt.expectedErr)
				return
			} else {
				assert.Equal(t, got, tt.expectedBody)
			}
		})
	}
}
