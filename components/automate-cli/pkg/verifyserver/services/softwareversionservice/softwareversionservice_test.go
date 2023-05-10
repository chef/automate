package softwareversionservice

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/assert"
)

var osTestVersion = map[string][]string{
	"Red Hat Linux": {"7,8,9"},
	"Ubuntu":        {"16.04.x", "18.04.x", "20.04.x", "22.04.x"},
	"Centos":        {"7"},
	"Amazon Linux":  {"2"},
	"SUSE Linux":    {"12"},
}

const successfile = "./testfiles/success.txt"
const failurefile = "./testfiles/failure.txt"
const versionfile = "./testfiles/version.txt"
const failfilepath = "./failfilepath"

var checktrue = []string{"mkdir"}
var checkfalse = []string{"wrongcammand"}

func TestGetSoftwareVersionService(t *testing.T) {
	log, _ := logger.NewLogger("text", "debug")
	service := NewSoftwareVersionService(log)
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
						Title:         "stat availability",
						Passed:        true,
						SuccessMsg:    "stat is available",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "mkdir availability",
						Passed:        true,
						SuccessMsg:    "mkdir is available",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "Ubuntu availability",
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
						Title:         "openssl availability",
						Passed:        true,
						SuccessMsg:    "openssl is available",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "mkdir availability",
						Passed:        true,
						SuccessMsg:    "mkdir is available",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "Ubuntu availability",
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
			description: "If the query parameter is automate or chef-server",
			args: args{
				query:      "automate",
				checkarray: checktrue,
				osFilepath: successfile,
			},
			expectedBody: &models.SoftwareVersionDetails{
				Passed: true,
				Checks: []models.Checks{
					{
						Title:         "mkdir availability",
						Passed:        true,
						SuccessMsg:    "mkdir is available",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "Ubuntu availability",
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
						Title:         "stat availability",
						Passed:        true,
						SuccessMsg:    "stat is available",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "wrongcammand availability",
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      "wrongcammand is not available",
						ResolutionMsg: "Ensure wrongcammand is available in $PATH on the node",
					},
					{
						Title:         "Ubuntu availability",
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
			expectedBody: nil,
			expectedError: "Error while getting the OS Version: open ./failfilepath: no such file or directory",
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
						Title:         "stat availability",
						Passed:        true,
						SuccessMsg:    "stat is available",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "mkdir availability",
						Passed:        true,
						SuccessMsg:    "mkdir is available",
						ErrorMsg:      "",
						ResolutionMsg: "",
					},
					{
						Title:         "Debian GNU/Linux 10 (buster) availability",
						Passed:        false,
						SuccessMsg:    "",
						ErrorMsg:      "Debian GNU/Linux 10 (buster) version is not supported by automate",
						ResolutionMsg: "Ensure Debian GNU/Linux 10 (buster) correct version is installed on the node",
					},
				},
			},
		},
		{
			description: "If the entered Query is not supported by us",
			args: args{
				query:      "wrongquery",
				checkarray: checktrue,
				osFilepath: successfile,
			},
			expectedBody: nil,
			expectedError: "The query wrongquery is not supported. The Supported query's are = postgres, opensearch, bastion, automate, chef-server",
		},
	}
	for _, tt := range tests {
		t.Run(tt.description, func(t *testing.T) {
			service.(*SoftwareVersionService).osFilepath = tt.args.osFilepath
			service.(*SoftwareVersionService).cmdCheckArray = tt.args.checkarray
			got, err := service.GetSoftwareVersionServices(tt.args.query)
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
	sv := NewSoftwareVersionService(log)
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
	sv := NewSoftwareVersionService(log)
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
				Title:         "mkdir availability",
				Passed:        true,
				SuccessMsg:    "mkdir is available",
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
	sv := NewSoftwareVersionService(log)
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
				Title:         "Debian GNU/Linux 10 (buster) availability",
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      "Debian GNU/Linux 10 (buster) version is not supported by automate",
				ResolutionMsg: "Ensure Debian GNU/Linux 10 (buster) correct version is installed on the node",
			},
		},
		{
			description: "If the os and version both are correct",
			args: args{
				osFilepath: successfile,
			},
			expectedBody: &models.Checks{
				Title:         "Ubuntu availability",
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
				Title:         "SUSE Linux availability",
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
