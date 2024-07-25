package main

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"os/exec"
	"strconv"
	"strings"
	"time"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/client"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

type VersionCommand struct {
	Command          string `json:"command"`
	Status           string `json:"status"`
	ErrorCode        int    `json:"error_code"`
	ErrorDescription string `json:"error_description"`
	ErrorCause       string `json:"error_cause"`
	ErrorStackTrace  string `json:"error_stack_trace"`
	ErrorRecovery    string `json:"error_recovery"`
	ErrorType        string `json:"error_type"`
	Result           struct {
		ClientVersion   string `json:"client_version"`
		ClientGitSha    string `json:"client_git_sha"`
		ManifestVersion string `json:"manifest_version"`
		ManifestGitSha  string `json:"manifest_git_sha"`
	} `json:"result"`
}

type LicenseResult struct {
	Result           LicenseStatus `json:"result"`
	ErrorType        string        `json:"error_type"`
	ErrorDescription string        `json:"error_description"`
}

type LicenseStatus struct {
	CustomerName   string         `json:"customer_name"`
	LicenseType    string         `json:"license_type"`
	LicenseId      string         `json:"license_id"`
	ExpirationDate ExpirationDate `json:"expiration_date"`
	GracePeriod    bool           `json:"grace_period"`
}

type ExpirationDate struct {
	Seconds int64 `json:"seconds"`
}

const commercial = "commercial"

type LExecutor interface {
	runCommandOnSingleAutomateNodeForErr(cmd *cobra.Command, args []string) (string, error)
}

type LicenseExecutor struct{}

func (e LicenseExecutor) runCommandOnSingleAutomateNodeForErr(cmd *cobra.Command, args []string) (string, error) {
	return RunLicenseCmdOnSingleAutomateNode(cmd, args)
}

func runTheCommandOnHAErr(cmd *cobra.Command, args []string, e LExecutor) error {
	output, err := e.runCommandOnSingleAutomateNodeForErr(cmd, args)
	if err != nil {
		return err
	}
	if output == "" {
		return nil
	}

	licenseResult := &LicenseResult{}
	err = json.Unmarshal([]byte(output), &licenseResult)
	if err != nil {
		return err
	}

	return checkLicenseExpiry(licenseResult)
}

func RunLicenseCmdOnSingleAutomateNode(cmd *cobra.Command, args []string) (string, error) {
	infra, err := getAutomateHAInfraDetails()
	if err != nil {
		return "", err
	}
	ips := infra.Outputs.AutomatePrivateIps.Value
	if len(ips) == 0 {
		return "", errors.New("No automate IPs are found")
	}
	sshConfig := &SSHConfig{
		sshUser:    infra.Outputs.SSHUser.Value,
		sshPort:    infra.Outputs.SSHPort.Value,
		sshKeyFile: infra.Outputs.SSHKeyFile.Value,
		hostIP:     ips[0],
		timeout:    10,
	}

	sshUtil := NewSSHUtil(sshConfig)

	script := "sudo chef-automate version --result-json /hab/version.json"
	_, err = sshUtil.connectAndExecuteCommandOnRemoteSuppressLog(script, true, true)
	if err != nil {
		return "", err
	}

	script = "sudo cat /hab/version.json"
	readVersion, err := sshUtil.connectAndExecuteCommandOnRemoteSuppressLog(script, true, true)
	if err != nil {
		return "", err
	}

	var versionCMD VersionCommand
	if err = json.Unmarshal([]byte(readVersion), &versionCMD); err != nil {
		return "", err
	}

	if !compareVersions(versionCMD.Result.ManifestVersion, "4.12.69") {
		return "", nil
	}

	script = "sudo chef-automate license status --result-json /hab/license.json"
	sshUtil.connectAndExecuteCommandOnRemoteSuppressLog(script, true, true)

	script = "sudo cat /hab/license.json"
	readLicense, err := sshUtil.connectAndExecuteCommandOnRemoteSuppressLog(script, true, true)
	if err != nil {
		return "", err
	}
	return readLicense, nil

}

func runTheCommandOnHAWarn(cmd *cobra.Command, args []string, e LExecutor) error {
	output, err := e.runCommandOnSingleAutomateNodeForErr(cmd, args)
	if err != nil {
		return err
	}

	if output == "" {
		return nil
	}

	licenseResult := &LicenseResult{}
	err = json.Unmarshal([]byte(output), &licenseResult)
	if err != nil {
		return err
	}

	err = warnIfLicenseNearExpiry(licenseResult)
	if err != nil {
		return err
	}
	return nil
}

func checkLicenseStatusForExpiry(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		if err := runTheCommandOnHAErr(cmd, args, LicenseExecutor{}); err != nil {
			return err
		}
	} else {
		allow, err := AllowLicenseEnforcement()
		if err != nil {
			return err
		}

		if allow {
			err := commandPrePersistent(cmd)
			if err != nil {
				return status.Wrap(err, status.CommandExecutionError, "unable to set command parent settings")
			}
			licenseResult, err := getLicenseResult()
			if err != nil {
				return err
			}

			err = checkLicenseExpiry(licenseResult)
			if err != nil {
				return err
			}
		}
	}
	return nil
}

func AllowLicenseEnforcement() (bool, error) {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return false, err
	}

	response, err := connection.ManifestVersion(context.Background(), &api.ManifestVersionRequest{})
	if err != nil {
		return false, status.Wrap(
			err,
			status.DeploymentServiceCallError,
			"Request for Chef Automate package manifest failed",
		)
	}

	return compareVersions(response.BuildTimestamp, "4.12.69"), nil
}

// compare version check the v1 version is greater than or equal to v2 version
// if v1 <= v2 => false
// if v1 > v2 => true
func compareVersions(v1, v2 string) bool {
	v2Slice := strings.Split(v2, ".")

	// write the logic
	if strings.Contains(v1, ".") {
		v1Slice := strings.Split(v1, ".")

		if toInt(v1Slice[0]) > toInt(v2Slice[0]) {
			return true
		} else if toInt(v1Slice[0]) < toInt(v2Slice[0]) {
			return false
		}

		if toInt(v1Slice[1]) > toInt(v2Slice[1]) {
			return true
		} else if toInt(v1Slice[1]) < toInt(v2Slice[1]) {
			return false
		}

		if toInt(v1Slice[2]) > toInt(v2Slice[2]) {
			return true
		}
	}
	// Check if v1 is timestamp and v2 is version
	return false
}

func toInt(str string) int {
	v1int, err := strconv.Atoi(str)
	if err != nil {
		fmt.Println("cannot parse v1Slice[0]")
	}
	return v1int
}

func WarnLicenseStatusForExpiry(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		if err := runTheCommandOnHAWarn(cmd, args, LicenseExecutor{}); err != nil {
			return err
		}
	} else {
		allow, err := AllowLicenseEnforcement()
		if err != nil {
			return err
		}

		if allow {
			err := commandPrePersistent(cmd)
			if err != nil {
				return status.Wrap(err, status.CommandExecutionError, "unable to set command parent settings")
			}

			licenseResult, err := getexpiredLicense()
			if err != nil {
				return err
			}

			err = warnIfLicenseNearExpiry(licenseResult)
			if err != nil {
				return err
			}
			return nil
		}
	}

	return nil
}

func getexpiredLicense() (*LicenseResult, error) {
	// Create a temporary file
	tmpFile, err := os.CreateTemp("/hab", "license-*.json")
	if err != nil {
		return nil, err
	}
	defer os.Remove(tmpFile.Name())

	cmd1 := exec.Command("chef-automate", "license", "status", "--result-json", tmpFile.Name())
	var stderr bytes.Buffer
	cmd1.Stderr = &stderr

	err = cmd1.Run()
	if err != nil {
		if strings.Contains(stderr.String(), "This license has expired") {
			return readFileAndMarshal(tmpFile.Name())
		} else {
			log.Printf("Command error output: %s", stderr.String())
			return nil, err
		}

	}

	return getLicenseResult()
}

func getLicenseResult() (*LicenseResult, error) {
	// Create a temporary file
	tmpFile, err := os.CreateTemp("", "license-*.json")
	if err != nil {
		return nil, err
	}
	defer os.Remove(tmpFile.Name())

	cmd1 := exec.Command("chef-automate", "license", "status", "--result-json", tmpFile.Name())
	var stderr bytes.Buffer
	cmd1.Stderr = &stderr

	if err := cmd1.Run(); err != nil {
		if strings.Contains(stderr.String(), "This license has expired") {
			return readFileAndMarshal(tmpFile.Name())
		} else {
			log.Printf("Command error output: %s", stderr.String())
			return nil, err
		}

	}

	return readFileAndMarshal(tmpFile.Name())
}

func readFileAndMarshal(fileName string) (*LicenseResult, error) {
	var result LicenseResult

	byteValue, err := os.ReadFile(fileName)
	if err != nil {
		return nil, err
	}

	defer os.Remove(fileName)
	json.Unmarshal(byteValue, &result)

	return &result, nil
}

func checkLicenseExpiry(licenseResult *LicenseResult) error {
	if licenseResult.Result.LicenseId == "" {
		if licenseResult.ErrorType != "" {
			return status.New(
				status.DeploymentServiceCallError,
				licenseResult.ErrorDescription,
			)
		}
		return status.New(
			status.LicenseError, "Your Progress® Chef® Automate™ license has expired or does not exist! You no longer have access to Chef Automate. Please contact the Account Team to upgrade to an Enterprise License.")
	}

	licenseValidDate := time.Unix(licenseResult.Result.ExpirationDate.Seconds, 0) // gives unix time stamp in utc
	//gracePeriodDuration := 60
	aboutToExpire := 60
	currentTime := time.Now()
	daysUntilExpiration := int(licenseValidDate.Sub(currentTime).Hours() / 24)
	//gracePeriodDate := licenseValidDate.AddDate(0, 0, gracePeriodDuration)
	licenseDate := licenseValidDate.Format("02-01-2006")
	//graceDate := gracePeriodDate.Format("02-01-2006")

	if daysUntilExpiration > aboutToExpire {
		// If the license is not about to expire within 60 days, do nothing.
		return nil
	}

	// If the license type is commercial, adding a grace period of 60 days
	if licenseResult.Result.LicenseType == commercial {
		if !licenseResult.Result.GracePeriod {
			if daysUntilExpiration <= aboutToExpire && daysUntilExpiration > 0 {
				// cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your Progress® Chef® Automate™ license is set to expire on %s! To avoid any future disruption to your DevOps processes, update your license! Please contact the Account Team or email us at chef-account-team@progress.com for further assistance.", licenseDate))
				return nil
			} else {
				return status.New(
					status.LicenseError, fmt.Sprintf("Your Progress® Chef® Automate™ license expired on %s and you no longer have access to Chef Automate! To get a new license, please contact the Account Team or email us at chef-account-team@progress.com.", licenseDate))
			}
		} else { // on grade period
			//	cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your Progress® Chef® Automate™ license expired on %s and you are currently on a limited extension period! To get a new license, please contact the Account Team or email us at chef-account-team@progress.com.", graceDate))
			return nil
		}
	} else { // for trail and internal licenses
		if daysUntilExpiration > 0 {
			// cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your Progress® Chef® Automate™ license is set to expire on %s! Please get in touch with the Account Team for further assistance.", licenseDate))
			return nil
		} else {
			return status.New(
				status.LicenseError, "Your Progress® Chef® Automate™ license has expired! You no longer have access to Chef Automate. Please contact the Account Team to upgrade to an Enterprise License.")
		}
	}

	return nil
}

func warnIfLicenseNearExpiry(licenseResult *LicenseResult) error {
	if licenseResult.Result.LicenseId == "" {
		if licenseResult.ErrorType != "" {
			return status.New(
				status.DeploymentServiceCallError,
				licenseResult.ErrorDescription,
			)
		}
		return status.New(
			status.LicenseError, "Your Progress® Chef® Automate™ license has expired or does not exist! You no longer have access to Chef Automate. Please contact the Account Team to upgrade to an Enterprise License.")
	}

	// Calculate the license valid date
	licenseValidDate := time.Unix(licenseResult.Result.ExpirationDate.Seconds, 0) // gives unix time stamp in utc
	//gracePeriodDuration := 60
	aboutToExpire := 60
	currentTime := time.Now()
	daysUntilExpiration := int(licenseValidDate.Sub(currentTime).Hours() / 24)
	//gracePeriodDate := licenseValidDate.AddDate(0, 0, gracePeriodDuration)
	//licenseDate := licenseValidDate.Format("02-01-2006")
	//graceDate := gracePeriodDate.Format("02-01-2006")

	if daysUntilExpiration > aboutToExpire {
		// If the license is not about to expire within 60 days, do nothing.
		return nil
	}
	// If the license type is commercial, adding a grace period of 60 days
	if licenseResult.Result.LicenseType == commercial {
		if !licenseResult.Result.GracePeriod {
			if daysUntilExpiration <= aboutToExpire && daysUntilExpiration > 0 {
				//cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your Progress® Chef® Automate™ license is set to expire on %s! To avoid any future disruption to your DevOps processes, update your license! Please contact the Account Team or email us at chef-account-team@progress.com for further assistance.", licenseDate))
				return nil
			} else {
				//cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your Progress® Chef® Automate™ license expired on %s and you no longer have access to Chef Automate! To get a new license, please contact the Account Team or email us at chef-account-team@progress.com.", licenseDate))
				return nil
			}
		} else {
			//cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your Progress® Chef® Automate™ license expired on %s and you are currently on a limited extension period! To get a new license, please contact the Account Team or email us at chef-account-team@progress.com.", graceDate))
			return nil
		}
	} else { // for trail and internal licenses
		if daysUntilExpiration > 0 {
			// cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your Progress® Chef® Automate™ license is set to expire on %s! Please get in touch with the Account Team for further assistance.", licenseDate))
			return nil
		} else {
			//cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn("Your Progress® Chef® Automate™ license has expired or does not exist! You no longer have access to Chef Automate. Please contact the Account Team to upgrade to an Enterprise License.")
			return nil
		}
	}
}
