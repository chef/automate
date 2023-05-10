package softwareversionservice

import (
	"os/exec"
	"regexp"
	"strconv"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/getosutils"
	"github.com/chef/automate/lib/logger"
	"github.com/pkg/errors"
)

type ISoftwareVersionService interface {
	GetSoftwareVersionServices(string) (*models.SoftwareVersionDetails, error)
}

type SoftwareVersionService struct {
	cmdCheckArray []string
	osFilepath    string
	logger        logger.Logger
}

func NewSoftwareVersionService(logger logger.Logger) ISoftwareVersionService {
	return &SoftwareVersionService{
		cmdCheckArray: cmdCheckArray,
		osFilepath:    OSFILEPATH,
		logger:        logger,
	}
}

const (
	AVAILABILITY = " availability"
	ENSURE       = "Ensure "
	OSFILEPATH   = "/etc/os-release"
)

var cmdCheckArray = []string{"mkdir", "useradd", "chown", "rm", "touch", "truncate", "echo", "sleep", "ls", "grep", "yum", "which", "cp", "curl", "bash", "sysctl", "cat", "sed", "mount", "pvcreate", "vgcreate", "lvcreate", "mv", "systemd", "wget", "exec"}

func (sv *SoftwareVersionService) GetSoftwareVersionServices(query string) (*models.SoftwareVersionDetails, error) {
	sv.logger.Debug("The query paramter entered = ", query)
	serviceResponse := models.SoftwareVersionDetails{}
	serviceResponse.Passed = true
	serviceResponseArray := []models.Checks{}
	cmdArray := []string{}
	if query == "postgres" {
		cmdArray = []string{"stat"}
		cmdArray = append(cmdArray, sv.cmdCheckArray...)
	} else if query == "opensearch" {
		cmdArray = []string{"openssl"}
		cmdArray = append(cmdArray, sv.cmdCheckArray...)
	} else if query == "bastion" || query == "automate" || query == "chef-server" {
		cmdArray = append(cmdArray, sv.cmdCheckArray...)
	} else {
		sv.logger.Error("The Query parameter is not supported")
		return nil, errors.New("The query "+ query +" is not supported. The Supported query's are = postgres, opensearch, bastion, automate, chef-server")
	}

	for i := 0; i < len(cmdArray); i++ {
		checkResponse := sv.checkCommandVersion(cmdArray[i])
		if !checkResponse.Passed {
			serviceResponse.Passed = false
		}
		serviceResponseArray = append(serviceResponseArray, *checkResponse)
	}
	osResponse, err := sv.checkOsVersion(sv.osFilepath)
	if err != nil {
		sv.logger.Error("Error while getting the OS Version = ", err)
		return nil, errors.Wrap(err, "Error while getting the OS Version")
	}
	if !osResponse.Passed {
		serviceResponse.Passed = false
	}
	serviceResponseArray = append(serviceResponseArray, *osResponse)
	serviceResponse.Checks = serviceResponseArray
	sv.logger.Debug("The Passed value for the response = ", serviceResponse.Passed)
	sv.logger.Debug("The Checks array for the response = ", serviceResponse.Checks)
	return &models.SoftwareVersionDetails{
		Passed: serviceResponse.Passed,
		Checks: serviceResponse.Checks,
	}, nil
}
func (sv *SoftwareVersionService) checkCommandVersion(cmdName string) *models.Checks {
	_, err := exec.LookPath(cmdName)
	if err != nil {
		return &models.Checks{
			Title:         cmdName + AVAILABILITY,
			Passed:        false,
			SuccessMsg:    "",
			ErrorMsg:      cmdName + " is not available",
			ResolutionMsg: ENSURE + cmdName + " is available in $PATH on the node",
		}
	}
	return &models.Checks{
		Title:         cmdName + AVAILABILITY,
		Passed:        true,
		SuccessMsg:    cmdName + " is available",
		ErrorMsg:      "",
		ResolutionMsg: "",
	}
}

func (sv *SoftwareVersionService) checkOsVersion(osFilepath string) (*models.Checks, error) {
	osVersions := map[string][]string{
		"Red Hat":      {"7,8,9"},
		"Ubuntu":       {"16.04.x", "18.04.x", "20.04.x", "22.04.x"},
		"Centos":       {"7"},
		"Amazon Linux": {"2"},
		"SUSE Linux":   {"12"},
	}
	checkResponse := models.Checks{}
	var osName, osVersion, err = getosutils.GetOsVersion(osFilepath)
	sv.logger.Debug("Got the OS Version = ", osVersion)
	sv.logger.Debug("Got the OS Name = ", osName)
	if err != nil {
		sv.logger.Error("Enable to get OS Version as file path doesnot exit = ", err)
		return nil, err
	}
	for key := range osVersions {
		if strings.Contains(osName, key) {
			correctVersion := sv.checkOs(osVersions, osVersion, key)
			if correctVersion {
				checkResponse = models.Checks{
					Title:         key + AVAILABILITY,
					Passed:        true,
					SuccessMsg:    key + " version is " + osVersion,
					ErrorMsg:      "",
					ResolutionMsg: "",
				}
				break
			}
			checkResponse = models.Checks{
				Title:         key + AVAILABILITY,
				Passed:        false,
				SuccessMsg:    "",
				ErrorMsg:      key + " version is not supported by automate",
				ResolutionMsg: ENSURE + key + " correct version is installed on the node",
			}
			break
		}
		checkResponse = models.Checks{
			Title:         osName + AVAILABILITY,
			Passed:        false,
			SuccessMsg:    "",
			ErrorMsg:      osName + " version is not supported by automate",
			ResolutionMsg: ENSURE + osName + " correct version is installed on the node",
		}
	}
	return &checkResponse, nil
}

func (sv *SoftwareVersionService) checkOs(osVersions map[string][]string, osVersion string, osName string) bool {
	correctVersion := false
	sv.logger.Debug("The Version of the node = ", osVersion)
	if osName == "Ubuntu" {
		split := strings.Split(osVersion, ".")
		checkVersion := split[0] + "." + split[1]
		re := regexp.MustCompile(checkVersion)

		for _, str := range osVersions[osName] {
			if re.MatchString(str) {
				correctVersion = true
				break
			}
		}
	} else if osName == "Red Hat" {
		checkVersion, _ := strconv.ParseFloat(osVersion, 64)
		if checkVersion >= 7 {
			correctVersion = true
		}
	} else {
		if osVersions[osName][0] == osVersion {
			correctVersion = true
		}
	}
	return correctVersion
}
