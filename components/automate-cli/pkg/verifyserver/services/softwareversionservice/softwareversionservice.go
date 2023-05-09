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
    GetSoftwareVersionServices(string) (models.SoftwareVersionDetails, error)
}

type SoftwareVersionService struct {
    cmdCheckArray []string
    osFilepath    string
}

func NewSoftwareVersionService() ISoftwareVersionService {
    return &SoftwareVersionService{
        cmdCheckArray: cmdCheckArray,
        osFilepath:    osFilepath,
    }
}

const osFilepath = "/etc/os-release"

var cmdCheckArray = []string{"mkdir", "useradd", "chown", "rm", "touch", "truncate", "echo", "sleep", "ls", "grep", "yum", "which", "cp", "curl", "bash", "sysctl", "cat", "sed", "mount", "pvcreate", "vgcreate", "lvcreate", "mv", "systemd", "wget", "exec"}

func (sv *SoftwareVersionService) GetSoftwareVersionServices(query string) (models.SoftwareVersionDetails, error) {
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
    } else {
        cmdArray = append(cmdArray, sv.cmdCheckArray...)
    }

    for i := 0; i < len(cmdArray); i++ {
        checkResponse := checkCommandVersion(cmdArray[i])
        if !checkResponse.Passed {
            serviceResponse.Passed = false
        }
        serviceResponseArray = append(serviceResponseArray, checkResponse)
    }
    osResponse, err := checkOsVersion(sv.osFilepath)
    if err != nil {
        logger.NewLogrusStandardLogger().Error("Error while getting OS version = ", err)
        return models.SoftwareVersionDetails{
            Passed: false,
            Checks: []models.Checks{
                {},
            },
        }, err
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
            Title:         cmdName + " availability",
            Passed:        false,
            SuccessMsg:    "",
            ErrorMsg:      cmdName + " is not available",
            ResolutionMsg: "Ensure " + cmdName + " is available in $PATH on the node",
        }
    }
    return models.Checks{
        Title:         cmdName + " availability",
        Passed:        true,
        SuccessMsg:    cmdName + " is available",
        ErrorMsg:      "",
        ResolutionMsg: "",
    }
}

func readFile(osFilepath string) ([]byte, error) {
    data, err := ioutil.ReadFile(osFilepath) // nosemgrep
    if err != nil {
        logger.NewLogrusStandardLogger().Error("Error while reading the OS file from the path = ", err)
        return []byte{}, err
    }
    return data, nil
}

func getOsVersion(osFilepath string) (string, string, error) {
    data, err := readFile(osFilepath)
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
    checkResponse := models.Checks{}
    var name, version, err = getOsVersion(osFilepath)
    if err != nil {
        return models.Checks{}, err
    }
    for key := range osVersions {
        if strings.Contains(name, key) {
            correctVersion := checkOs(osVersions, version, key)
            if correctVersion {
                checkResponse = models.Checks{
                    Title:         key + " availability",
                    Passed:        true,
                    SuccessMsg:    key + " version is " + version,
                    ErrorMsg:      "",
                    ResolutionMsg: "",
                }
                break
            }
            checkResponse = models.Checks{
                Title:         key + " availability",
                Passed:        false,
                SuccessMsg:    "",
                ErrorMsg:      key + " version is not supported by automate",
                ResolutionMsg: "Ensure " + key + " correct version is installed on the node",
            }
            break
        }
        checkResponse = models.Checks{
            Title:         name + " availability",
            Passed:        false,
            SuccessMsg:    "",
            ErrorMsg:      name + " version is not supported by automate",
            ResolutionMsg: "Ensure " + name + " correct version is installed on the node",
        }
    }
    return checkResponse, nil
}

func checkOs(osVersions map[string][]string, version string, key string) bool {
    correctVersion := false
    if key == "Ubuntu" {
        split := strings.Split(version, ".")
        checkVersion := split[0] + "." + split[1]
        re := regexp.MustCompile(checkVersion)

        for _, str := range osVersions[key] {
            if re.MatchString(str) {
                correctVersion = true
                break
            }
        }
    } else if key == "Red Hat" {
        checkVersion, _ := strconv.ParseFloat(version, 64)
        if checkVersion >= 7 {
            correctVersion = true
        }
    } else {
        if osVersions[key][0] == version {
            correctVersion = true
        }
    }
    return correctVersion
}