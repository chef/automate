package softwareversionservice

import (
	"io/ioutil"
	"os/exec"
	"regexp"
	"strconv"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
)

type ISoftwareVersionService interface {
	GetSoftwareVersionServices() (models.SoftwareVersionDetails, error)
}

type SoftwareVersionService struct {
}

func NewSoftwareVersionService() ISoftwareVersionService {
	return &SoftwareVersionService{}
}

const osFilepath = "/etc/os-release"

var cmdCheckArray = []string{"mkdir", "chown", "rm", "touch", "truncate", "echo", "sleep", "ls", "grep", "timestamp", "yum", "which", "source", "cp", "curl", "bash", "vm", "sysctl", "lmv", "cat", "sed", "mount", "pvcreate", "vgcreate", "lvcreate", "mv"}

func (sv *SoftwareVersionService) GetSoftwareVersionServices() (models.SoftwareVersionDetails, error) {
	serviceResponse := models.SoftwareVersionDetails{}
	serviceResponse.Passed = true
	serviceResponseArray := []models.Checks{}
	for i := 0; i < len(cmdCheckArray); i++ {
		checkResponse := checkCommandVersion(cmdCheckArray[i])
		if !checkResponse.Passed {
			serviceResponse.Passed = false
		}
		serviceResponseArray = append(serviceResponseArray, checkResponse)
	}
	osResponse, err := checkOsVersion(osFilepath)
	if err != nil {
        logger.NewLogrusStandardLogger().Error("Error while Os version = ",err)
		return models.SoftwareVersionDetails{}, err
	}
	if !osResponse.Passed {
		serviceResponse.Passed = false
	}
	serviceResponseArray = append(serviceResponseArray, osResponse)
	serviceResponse.Checks = serviceResponseArray
	return models.SoftwareVersionDetails{
		Passed: serviceResponse.Passed,
		Checks: serviceResponse.Checks,
	}, nil
}
func checkCommandVersion(cmdName string) models.Checks {
	_, err := exec.LookPath(cmdName)
	if err != nil {
		return models.Checks{
			Title:          cmdName + " availability",
			Passed:         false,
			SuccessMsg:    "",
			ErrorMsg:      cmdName + " is not available",
			ResolutionMsg: "Ensure " + cmdName + " is available in $PATH on the node",
		}
	}
	return models.Checks{
		Title:          cmdName + " availability",
		Passed:         true,
		SuccessMsg:    cmdName + " is available",
		ErrorMsg:      "",
		ResolutionMsg: "",
	}
}

func ReadFile(osFilepath string) ([]byte, error) {
	data, err := ioutil.ReadFile(osFilepath)
	if err != nil {
		logger.NewLogrusStandardLogger().Error("Error while reading the OS file from the path = ",err)
	}
	return data, nil
}

func GetOsVersion(osFilepath string) (string, string, error) {
	data, err := ReadFile(osFilepath)
	if err != nil {
		return "", "", err
	}
	var name, version string
	lines := strings.Split(string(data), "\n")

	for _, line := range lines {
		if strings.HasPrefix(line, "PRETTY_NAME=") {
			name = strings.TrimPrefix(line, "PRETTY_NAME=")
			name = strings.Trim(name, `"`)
		} else if strings.HasPrefix(line, "VERSION_ID=") {
			version = strings.TrimPrefix(line, "VERSION_ID=")
			version = strings.Trim(version, `"`)
		}
	}
	return name, version, nil
}

func checkOsVersion(osFilepath string) (models.Checks, error) {
	osVersions := map[string][]string{
		"Red Hat":      {"7,8,9"},
		"Ubuntu":       {"16.04.x", "18.04.x", "20.04.x", "22.04.x"},
		"Centos":       {"7"},
		"Amazon Linux": {"2"},
		"SUSE Linux":   {"12"},
	}
	checkresponse := models.Checks{}
	var name, version, err = GetOsVersion(osFilepath)
	if err != nil {
		return models.Checks{}, err
	}
	for key := range osVersions {
		if strings.Contains(name, key) {
			correctversion := checkOs(osVersions, version, key)
			if correctversion {
				checkresponse = models.Checks{
					Title:          key + " availability",
					Passed:         true,
					SuccessMsg:    key + " version is " + version,
					ErrorMsg:      "",
					ResolutionMsg: "",
				}
                break
			}
			checkresponse = models.Checks{
				Title:          key + " availability",
				Passed:         false,
				SuccessMsg:    "",
				ErrorMsg:      key + " version is not supported by automate",
				ResolutionMsg: "Ensure " + key + " correct version is installed on the node",
			}
            break
		}
		checkresponse = models.Checks{
			Title:          name + " availability",
			Passed:         false,
			SuccessMsg:    "",
			ErrorMsg:      name + " version is not supported by automate",
			ResolutionMsg: "Ensure " + name + " correct version is installed on the node",
		}
	}
	return checkresponse, nil
}

func checkOs(osVersions map[string][]string, version string, key string) bool {
	correctversion := false
	if key == "Ubuntu" {
		split := strings.Split(version, ".")
		checkversion := split[0] + "." + split[1] 
		re := regexp.MustCompile(checkversion)

		for _, str := range osVersions[key] {
			if re.MatchString(str) {
				correctversion = true
				break
			}
		}
	} else if key == "Red Hat" {
		checkversion, _ := strconv.ParseFloat(version, 64)
		if checkversion >= 7 {
			correctversion = true
		}
	} else {
		if osVersions[key][0] == version {
			correctversion = true
		}
	}
	return correctversion
}
