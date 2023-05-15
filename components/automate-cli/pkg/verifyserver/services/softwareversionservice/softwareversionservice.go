package softwareversionservice

import (
	"regexp"
	"strconv"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/fiberutils"
	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/utils/getosutils"
	"github.com/chef/automate/lib/logger"
	"github.com/pkg/errors"
)

type ISoftwareVersionService interface {
	GetSoftwareVersionDetails(string) (*models.SoftwareVersionDetails, error)
}

type SoftwareVersionService struct {
	cmdCheckArray []string
	osFilepath    string
	logger        logger.Logger
	CheckPath     func(cmd string) (string, error)
}

func NewSoftwareVersionService(logger logger.Logger, CheckPath func(cmd string) (string, error)) ISoftwareVersionService {
	return &SoftwareVersionService{
		cmdCheckArray: cmdCheckArray,
		osFilepath:    OSFILEPATH,
		logger:        logger,
	}
}

const (
	AVAILABILITY              = " availability"
	ENSURE                    = "Ensure "
	OSFILEPATH                = "/etc/os-release"
	LINUX_VERSION_CHECK       = "Linux Version Check"
	UBUNTU                    = "Ubuntu"
	RED_HAT                   = "Red Hat"
	OPENSEARCH                = "opensearch"
	AUTOMATE                  = "automate"
	POSTGRES                  = "postgres"
	CHEF_SERVER               = "chef-server"
	BASTION                   = "bastion"
	DEBIAN                    = "Debian"
	RED_HAT_SUPPORTED_VERSION = 7
)

var cmdCheckArray = []string{"mkdir", "useradd", "chown", "rm", "touch", "truncate", "echo", "sleep", "ls", "grep", "yum", "which", "cp", "curl", "bash", "sysctl", "cat", "sed", "mount", "mv", "systemd", "wget", "exec", "rsync"}

func (sv *SoftwareVersionService) GetSoftwareVersionDetails(query string) (*models.SoftwareVersionDetails, error) {
	sv.logger.Debug("The query parameter entered: ", query)
	serviceResponse := &models.SoftwareVersionDetails{}
	serviceResponse.Passed = true
	serviceResponseArray := []*models.Checks{}
	cmdArray := []string{}
	switch query {
	case POSTGRES:
		cmdArray = []string{"stat"}
		cmdArray = append(cmdArray, sv.cmdCheckArray...)
	case OPENSEARCH:
		cmdArray = []string{"openssl"}
		cmdArray = append(cmdArray, sv.cmdCheckArray...)
	case AUTOMATE, CHEF_SERVER, BASTION:
		cmdArray = append(cmdArray, sv.cmdCheckArray...)
	default:
		sv.logger.Error("The Query parameter is not supported")
		return nil, errors.New("The query " + query + " is not supported. The Supported query's are: postgres, opensearch, bastion, automate, chef-server")
	}

	for i := 0; i < len(cmdArray); i++ {
		checkResponse := sv.checkCommandVersion(cmdArray[i])
		if !checkResponse.Passed {
			serviceResponse.Passed = false
		}
		serviceResponseArray = append(serviceResponseArray, checkResponse)
	}
	osResponse, err := sv.checkOsVersion(sv.osFilepath)
	if err != nil {
		sv.logger.Error("Error while getting the OS Version: ", osResponse.ErrorMsg)
	}
	if !osResponse.Passed {
		serviceResponse.Passed = false
	}
	serviceResponseArray = append(serviceResponseArray, osResponse)
	checks := make([]models.Checks, len(serviceResponseArray))
	for i, svcResp := range serviceResponseArray {
		checks[i] = *svcResp
	}
	serviceResponse.Checks = checks
	sv.logger.Debug("The Passed value for the response: ", serviceResponse.Passed)
	sv.logger.Debug("The Checks array for the response: ", serviceResponse.Checks)
	return serviceResponse, nil
}

func (sv *SoftwareVersionService) checkCommandVersion(cmdName string) *models.Checks {
	_, err := fiberutils.CheckPath(cmdName)
	if err != nil {
		sv.logger.Error("The errror which checking cammand file path: ", err)
		return failureResponse(cmdName+AVAILABILITY, cmdName+" is not available", ENSURE+cmdName+" is available in $PATH on the node")
	}
	return successResponse(cmdName+AVAILABILITY, cmdName+" is available")
}

func (sv *SoftwareVersionService) checkOsVersion(osFilepath string) (*models.Checks, error) {
	osVersions := map[string][]string{
		"Red Hat":      {"7,8,9"},
		"Ubuntu":       {"16.04.x", "18.04.x", "20.04.x", "22.04.x"},
		"Centos":       {"7"},
		"Amazon Linux": {"2"},
		"SUSE Linux":   {"12"},
		"Debian":       {"9", "10", "11", "12"},
	}
	checkResponse := &models.Checks{}
	var osName, osVersion, err = getosutils.GetOsVersion(osFilepath)
	if err != nil {
		sv.logger.Error("Enable to get OS Version as the file on the path does not exit: ", err)
		return failureResponse(LINUX_VERSION_CHECK, "Its not feasible to determine the Operating system version", "Please run system on the supported platforms."), nil
	}
	sv.logger.Debug("Got the OS Version: ", osVersion)
	sv.logger.Debug("Got the OS Name: ", osName)
	for key := range osVersions {
		if strings.Contains(strings.ToLower(osName), strings.ToLower(key)) {
			correctVersion := sv.checkOs(osVersions, osVersion, key)
			if correctVersion {
				checkResponse = successResponse(LINUX_VERSION_CHECK, key+" version is "+osVersion)
				break
			}
			checkResponse = failureResponse(LINUX_VERSION_CHECK, key+" version is not supported by automate", ENSURE+key+" correct version is installed on the node")
			break
		}
		checkResponse = failureResponse(LINUX_VERSION_CHECK, osName+" version is not supported by automate", ENSURE+osName+" correct version is installed on the node")
	}
	return checkResponse, nil
}

func (sv *SoftwareVersionService) checkOs(osVersions map[string][]string, osVersion string, osName string) bool {
	correctVersion := false
	switch osName {
	case UBUNTU:
		split := strings.Split(osVersion, ".")
		checkVersion := split[0] + "." + split[1]
		re := regexp.MustCompile(checkVersion)

		for _, str := range osVersions[osName] {
			if re.MatchString(str) {
				correctVersion = true
				break
			}
		}
	case RED_HAT:
		checkVersion, _ := strconv.ParseFloat(osVersion, 64)
		if checkVersion >= RED_HAT_SUPPORTED_VERSION {
			correctVersion = true
		}
	case DEBIAN:
		for _, str := range osVersions[osName] {
			if str == osVersion {
				correctVersion = true
				break
			}
		}
	default:
		if osVersions[osName][0] == osVersion {
			correctVersion = true
		}
	}
	return correctVersion
}

func successResponse(title string, success_msg string) *models.Checks {
	checkResponse := &models.Checks{
		Title:         title,
		Passed:        true,
		SuccessMsg:    success_msg,
		ErrorMsg:      "",
		ResolutionMsg: "",
	}
	return checkResponse
}

func failureResponse(title string, error_msg string, resolution_msg string) *models.Checks {
	checkResponse := &models.Checks{
		Title:         title,
		Passed:        false,
		SuccessMsg:    "",
		ErrorMsg:      error_msg,
		ResolutionMsg: resolution_msg,
	}
	return checkResponse
}
