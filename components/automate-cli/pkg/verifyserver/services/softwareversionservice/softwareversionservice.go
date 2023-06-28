package softwareversionservice

import (
	"fmt"
	"regexp"
	"strconv"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/constants"
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
	cmdCheckArray  []string
	osFilePath     string
	kernelFilePath string
	logger         logger.Logger
	CheckPath      func(cmd string) (string, error)
}

func NewSoftwareVersionService(logger logger.Logger, CheckPath func(cmd string) (string, error)) *SoftwareVersionService {
	return &SoftwareVersionService{
		cmdCheckArray:  cmdCheckArray,
		osFilePath:     OSFILEPATH,
		kernelFilePath: KERNELFILEPATH,
		logger:         logger,
	}
}

const (
	AVAILABILITY                          = " availability"
	ENSURE                                = "Ensure "
	OSFILEPATH                            = "/etc/os-release"
	KERNELFILEPATH                        = "/proc/sys/kernel/osrelease"
	LINUX_VERSION_CHECK                   = "Linux Version Check"
	KERNAL_VERSION_CHECK                  = "Kernal Version Check"
	UBUNTU                                = "Ubuntu"
	RED_HAT                               = "Red Hat"
	AMAZON_LINUX                          = "Amazon Linux"
	SUSE_LINUX                            = "SUSE Linux"
	RED_HAT_SUPPORTED_VERSION             = 7
	KERNAL_SUPPORTED_VERSION              = 3.2
	KERNAL_SUPPORTED_VERSION_AMAZON_LINUX = 5.10
)

var cmdCheckArray = []string{"mkdir", "useradd", "usermod", "groupadd", "chown", "chmod",
	"rm", "touch", "truncate", "echo", "sleep", "ls", "ln",
	"grep", "which", "cp", "curl", "bash", "sh",
	"sysctl", "cat", "sed", "mount", "mv", "systemctl",
	"wget", "rsync", "tar", "find", "sort", "awk",
	"xargs", "tail", "id"}

func (sv *SoftwareVersionService) GetSoftwareVersionDetails(query string) (*models.SoftwareVersionDetails, error) {
	sv.logger.Debug("The query parameter entered: ", query)
	serviceResponse := &models.SoftwareVersionDetails{}
	serviceResponse.Passed = true
	serviceResponseArray := []*models.Checks{}
	cmdArray := []string{}
	switch query {
	case constants.POSTGRESQL:
		cmdArray = []string{"stat"}
		cmdArray = append(cmdArray, sv.cmdCheckArray...)
	case constants.OPENSEARCH:
		cmdArray = []string{"openssl"}
		cmdArray = append(cmdArray, sv.cmdCheckArray...)
	case constants.BASTION, constants.AUTOMATE, constants.CHEF_INFRA_SERVER:
		cmdArray = append(cmdArray, sv.cmdCheckArray...)
	default:
		sv.logger.Error("The Query parameter is not supported")
		return nil, errors.New(`The value ` + query + ` of query 'node_type' is not supported. The supported values are: bastion, automate, chef-infra-server, postgresql, opensearch.`)
	}

	for i := 0; i < len(cmdArray); i++ {
		checkResponse := sv.checkCommandVersion(cmdArray[i])
		if !checkResponse.Passed {
			serviceResponse.Passed = false
		}
		serviceResponseArray = append(serviceResponseArray, checkResponse)
	}
	osResponse, osName, err := sv.checkOsVersion(sv.osFilePath)
	if err != nil {
		sv.logger.Error("Error while getting the OS Version: ", osResponse.ErrorMsg)
	}
	if !osResponse.Passed {
		serviceResponse.Passed = false
	}
	kernelResponse, err := sv.checkKernelVersion(sv.kernelFilePath, osName)
	if err != nil {
		sv.logger.Error("Error while getting the Kernal Version: ", kernelResponse.ErrorMsg)
	}
	if !kernelResponse.Passed {
		serviceResponse.Passed = false
	}
	serviceResponseArray = append(serviceResponseArray, kernelResponse, osResponse)
	checks := make([]models.Checks, len(serviceResponseArray))
	for i, svcResp := range serviceResponseArray {
		checks[i] = *svcResp
	}
	serviceResponse.Checks = checks
	return serviceResponse, nil
}

func (sv *SoftwareVersionService) checkCommandVersion(cmdName string) *models.Checks {
	_, err := fiberutils.CheckPath(cmdName)
	if err != nil {
		sv.logger.Error("Error while checking command file path: ", err)
		return failureResponse(cmdName+AVAILABILITY, cmdName+" is not available", ENSURE+cmdName+" is available in $PATH on the node")
	}
	return successResponse(cmdName+AVAILABILITY, cmdName+" is available")
}

func (sv *SoftwareVersionService) checkOsVersion(osFilepath string) (*models.Checks, string, error) {
	osVersions := map[string][]string{
		"Red Hat":      {"7,8,9"},
		"Ubuntu":       {"16.04.x", "18.04.x", "20.04.x", "22.04.x"},
		"CentOS":       {"7"},
		"Amazon Linux": {"2"},
		"SUSE Linux":   {"12.5"},
		"Debian":       {"9", "10", "11", "12"},
	}
	checkResponse := &models.Checks{}
	var osName, osVersion, err = getosutils.GetOsVersion(osFilepath)
	if err != nil {
		sv.logger.Error("Enable to get OS Version as the file on the path does not exist: ", err)
		return failureResponse(LINUX_VERSION_CHECK, "Its not feasible to determine the Operating system version", "Please run automate on the supported platforms."), osName, err
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
	return checkResponse, osName, nil
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
	default:
		for _, str := range osVersions[osName] {
			if str == osVersion {
				correctVersion = true
				break
			}
		}
	}
	return correctVersion
}

func (sv *SoftwareVersionService) checkKernelVersion(kernelFilePath, osName string) (*models.Checks, error) {
	kernelVersion, err := getosutils.GetKernelVersion(kernelFilePath)
	if err != nil {
		sv.logger.Error("Enable to get OS Version as the file on the path does not exit: ", err)
		return failureResponse(KERNAL_VERSION_CHECK, "Its not feasible to determine the Kernal version of the system", "Please run automate on the supported platforms."), err
	}
	sv.logger.Debug("Got the kernal version:", kernelVersion)
	checkVersion, _ := strconv.ParseFloat(kernelVersion, 64)
	if strings.Contains(strings.ToLower(osName), strings.ToLower(AMAZON_LINUX)) {
		if checkVersion == KERNAL_SUPPORTED_VERSION_AMAZON_LINUX {
			return successResponse(KERNAL_VERSION_CHECK, "Linux kernal version is "+fmt.Sprintf("%.2f", checkVersion)), nil
		}
		return failureResponse(KERNAL_VERSION_CHECK, "Linux kernel version is not equal to 5.10", "Use a linux version whose kernel version is equal to 5.10"), nil
	}
	if checkVersion >= KERNAL_SUPPORTED_VERSION {
		return successResponse(KERNAL_VERSION_CHECK, "Linux kernal version is "+fmt.Sprintf("%.2f", checkVersion)), nil
	}
	return failureResponse(KERNAL_VERSION_CHECK, "Linux kernel version is lower than 3.2", "Use a linux version whose kernel version is greater than 3.2"), nil
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
