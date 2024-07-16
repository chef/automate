package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"os/exec"
	"strings"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
)

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
	runCommandOnSingleAutomateNode(cmd *cobra.Command, args []string) (string, error)
}

type LicenseExecutor struct{}

func (e LicenseExecutor) runCommandOnSingleAutomateNode(cmd *cobra.Command, args []string) (string, error) {
	return RunLicenseCmdOnSingleAutomateNode(cmd, args)
}

func runTheCommandOnHA(cmd *cobra.Command, args []string, e LExecutor) error {
	output, err := e.runCommandOnSingleAutomateNode(cmd, args)
	if err != nil {
		return err
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
	script := "sudo chef-automate license status --result-json /hab/tmp/license.json"
	_, err = sshUtil.connectAndExecuteCommandOnRemoteSuppressLog(script, true, true)
	if err != nil {
		return "", err
	}
	script = "sudo cat /hab/tmp/license.json"
	readLicense, err := sshUtil.connectAndExecuteCommandOnRemoteSuppressLog(script, true, true)
	if err != nil {
		return "", err
	}
	return readLicense, nil
}

func checkLicenseStatusForExpiry(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		if err := runTheCommandOnHA(cmd, args, LicenseExecutor{}); err != nil {
			return err
		}
	} else {
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

	return nil
}

func WarnLicenseStatusForExpiry(cmd *cobra.Command, args []string) error {
	if isA2HARBFileExist() {
		if err := runTheCommandOnHA(cmd, args, LicenseExecutor{}); err != nil {
			return err
		}
	} else {
		err := commandPrePersistent(cmd)
		if err != nil {
			return status.Wrap(err, status.CommandExecutionError, "unable to set command parent settings")
		}

		licenseResult, err := getexpiredLicense()
		if err != nil {
			return err
		}

		warnIfLicenseNearExpiry(licenseResult)
		return nil
	}
	return nil
}

func getexpiredLicense() (*LicenseResult, error) {
	// Create a temporary file
	tmpFile, err := os.CreateTemp("", "license-*.json")
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
		log.Printf("Command error output: %s", stderr.String())
		return nil, err
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
			status.LicenseError,
			"Please apply a license.Please contact sales@chef.io to have your Chef Automate license.",
		)
	}

	licenseValidDate := time.Unix(licenseResult.Result.ExpirationDate.Seconds, 0) // gives unix time stamp in utc
	gracePeriodDuration := 60
	aboutToExpire := 60
	currentTime := time.Now()
	daysUntilExpiration := int(licenseValidDate.Sub(currentTime).Hours() / 24)
	gracePeriodDate := licenseValidDate.AddDate(0, 0, gracePeriodDuration)
	licenseDate := licenseValidDate.Format("02-01-2006")
	graceDate := gracePeriodDate.Format("02-01-2006")

	if daysUntilExpiration > aboutToExpire {
		// If the license is not about to expire within 60 days, do nothing.
		return nil
	}

	// If the license type is commercial, adding grace period of 60 days
	if licenseResult.Result.LicenseType == commercial {
		if !licenseResult.Result.GracePeriod {
			if daysUntilExpiration <= aboutToExpire && daysUntilExpiration > 0 {
				cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your Progress® Chef® Automate™ license is set to expire on %s! To avoid any future disruption to your DevOps processes, update your license! Please contact the Account Team or email us at chef-account-team@progress.com for further assistance.", licenseDate))
			} else {
				return status.New(
					status.LicenseError, fmt.Sprintf("Your Progress® Chef® Automate™ license expired on %s and you no longer have access to Chef Automate! To get a new license, please contact the Account Team or email us at chef-account-team@progress.com ", graceDate))
			}
		} else {
			cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your Progress® Chef® Automate™ license expired on %s and you are currently on a limited extension period! To get a new license, please contact the Account Team or email us at chef-account-team@progress.com", licenseDate))

		}
	} else { // for trail and internal licenses
		if daysUntilExpiration > 0 {
			cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your Progress® Chef® Automate™ license is set to expire on %s! Please get in touch with the Account Team for further assistance.", licenseDate))
		} else {
			return status.New(
				status.LicenseError, "Your Progress® Chef® Automate™ license has expired! You no longer have access to Chef Automate. Please contact the Account Team to upgrade to an Enterprise License.")
		}
	}

	return nil
}

func warnIfLicenseNearExpiry(licenseResult *LicenseResult) {
	// Calculate the license valid date
	licenseValidDate := time.Unix(licenseResult.Result.ExpirationDate.Seconds, 0) // gives unix time stamp in utc
	gracePeriodDuration := 60
	aboutToExpire := 60
	currentTime := time.Now()
	daysUntilExpiration := int(licenseValidDate.Sub(currentTime).Hours() / 24)
	gracePeriodDate := licenseValidDate.AddDate(0, 0, gracePeriodDuration)
	licenseDate := licenseValidDate.Format("02-01-2006")
	graceDate := gracePeriodDate.Format("02-01-2006")

	if daysUntilExpiration > aboutToExpire {
		// If the license is not about to expire within 60 days, do nothing.
		return
	}
	// If the license type is commercial, adding grace period of 60 days
	if licenseResult.Result.LicenseType == commercial {
		if !licenseResult.Result.GracePeriod {
			if daysUntilExpiration <= aboutToExpire && daysUntilExpiration > 0 {
				cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your Progress® Chef® Automate™ license is set to expire on %s! To avoid any future disruption to your DevOps processes, update your license! Please contact the Account Team or email us at chef-account-team@progress.com for further assistance.", licenseDate))
			} else {
				cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your Progress® Chef® Automate™ license expired on %s and you no longer have access to Chef Automate! To get a new license, please contact the Account Team or email us at chef-account-team@progress.com ", graceDate))
			}
		} else {
			cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your Progress® Chef® Automate™ license expired on %s and you are currently on a limited extension period! To get a new license, please contact the Account Team or email us at chef-account-team@progress.com", licenseDate))

		}
	} else { // for trail and internal licenses
		if daysUntilExpiration > 0 {
			cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your Progress® Chef® Automate™ license is set to expire on %s! Please get in touch with the Account Team for further assistance.", licenseDate))
		} else {
			cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn("Your Progress® Chef® Automate™ license has expired! You no longer have access to Chef Automate. Please contact the Account Team to upgrade to an Enterprise License.")
		}
	}
}
