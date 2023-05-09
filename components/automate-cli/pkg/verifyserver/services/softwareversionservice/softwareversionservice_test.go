package softwareversionservice

import (
    "errors"
    "reflect"
    "testing"

    "github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
    "github.com/chef/automate/lib/logger"
)

var osTestVersion = map[string][]string{
    "Red Hat Linux": {"7,8,9"},
    "Ubuntu":        {"16.04.x", "18.04.x", "20.04.x", "22.04.x"},
    "Centos":        {"7"},
    "Amazon Linux":  {"2"},
    "SUSE Linux":    {"12"},
}

const successfile = "./successfile"
const failurefile = "./failurefile"
const versionfile = "./versionfile"
const failfilepath = "./failfilepath"

var checktrue = []string{"mkdir"}
var checkfalse = []string{"wrongcammand"}

func TestGetSoftwareVersionService(t *testing.T) {
    service := NewSoftwareVersionService()
    service.(*SoftwareVersionService).osFilepath = successfile
    type args struct {
        query      string
        checkarray []string
        osFilepath string
    }
    tests := []struct {
        description   string
        args          args
        expectedBody  models.SoftwareVersionDetails
        expectedError error
    }{
        {
            description: "If the query parameter is postgres",
            args: args{
                query:      "postgres",
                checkarray: checktrue,
                osFilepath: successfile,
            },
            expectedBody: models.SoftwareVersionDetails{
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
            expectedError: nil,
        },
        {
            description: "If the query parameter is opensearch",
            args: args{
                query:      "opensearch",
                checkarray: checktrue,
                osFilepath: successfile,
            },
            expectedBody: models.SoftwareVersionDetails{
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
            expectedError: nil,
        },
        {
            description: "If the query parameter is automate or chef-server",
            args: args{
                query:      "automate",
                checkarray: checktrue,
                osFilepath: successfile,
            },
            expectedBody: models.SoftwareVersionDetails{
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
            expectedError: nil,
        },
        {
            description: "If the query parameter is file is wrong",
            args: args{
                query:      "postgres",
                checkarray: checkfalse,
                osFilepath: successfile,
            },
            expectedBody: models.SoftwareVersionDetails{
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
            expectedError: nil,
        },
        {
            description: "If we are not able to get the OS Version",
            args: args{
                query:      "postgres",
                checkarray: checktrue,
                osFilepath: failfilepath,
            },
            expectedBody: models.SoftwareVersionDetails{
                Passed: false,
                Checks: []models.Checks{
                    {},
                },
            },
            expectedError: errors.New("open /failfilepath: no such file or director"),
        },
		{
			description: "If the os version is not supported by automate",
			args: args{
				query: "postgres",
				checkarray: checktrue,
				osFilepath: failurefile,
			},
			expectedBody: models.SoftwareVersionDetails{
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
    }
    for _, tt := range tests {
        t.Run(tt.description, func(t *testing.T) {
            service.(*SoftwareVersionService).osFilepath = tt.args.osFilepath
            service.(*SoftwareVersionService).cmdCheckArray = tt.args.checkarray
            got, err := service.GetSoftwareVersionServices(tt.args.query)
            if err != nil {
                logger.NewLogrusStandardLogger().Error(err)
            }
            if !reflect.DeepEqual(got, tt.expectedBody) {
                t.Errorf("Get Softwarever = %v want %v", got, tt.expectedBody)
            }
        })
    }
}
func TestCheckOs(t *testing.T) {
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
            if got := checkOs(tt.args.osVersions, tt.args.version, tt.args.key); got != tt.expectedBody {
                t.Errorf("checkOs() = %v, want %v", got, tt.expectedBody)
            }
        })
    }
}

func TestCheckCommandVersion(t *testing.T) {
    type args struct {
        cmdName string
    }
    tests := []struct {
        description  string
        args         args
        expectedBody models.Checks
    }{
        {
            description: "If the cammand is present in the node",
            args: args{
                cmdName: "mkdir",
            },
            expectedBody: models.Checks{
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
            expectedBody: models.Checks{
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
            if got := checkCommandVersion(tt.args.cmdName); !reflect.DeepEqual(got, tt.expectedBody) {
                t.Errorf("checkCommandVersion() = %v, want %v", got, tt.expectedBody)
            }
        })
    }
}

func TestCheckOsVersion(t *testing.T) {
    type args struct {
        osFilepath string
    }
    tests := []struct {
        description  string
        args         args
        expectedBody models.Checks
        expectedErr  bool
    }{
        {
            description: "If the OS is not compatble with the automate",
            args: args{
                osFilepath: failurefile,
            },
            expectedBody: models.Checks{
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
            expectedBody: models.Checks{
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
            expectedBody: models.Checks{
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
            got, err := checkOsVersion(tt.args.osFilepath)
            if (err != nil) != tt.expectedErr {
                t.Errorf("checkOsVersion() error = %v, wantErr %v", err, tt.expectedErr)
                return
            }
            if !reflect.DeepEqual(got, tt.expectedBody) {
                t.Errorf("checkOsVersion() = %v, want %v", got, tt.expectedBody)
            }
        })
    }
}
