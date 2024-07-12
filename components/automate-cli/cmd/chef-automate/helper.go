package main

import (
	"bytes"
	"encoding/json"
	"fmt"
	"log"
	"os"
	"os/exec"
	"time"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
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

func checkLicenseStatusForExpiry(cmd *cobra.Command, args []string) error {
	err := commandPrePersistent(cmd)
	if err != nil {
		return status.Wrap(err, status.CommandExecutionError, "unable to set command parent settings")
	}
	licenseResult, err := getLicenseResult(cmd)
	if err != nil {
		return err
	}

	err = checkLicenseExpiry(licenseResult)
	if err != nil {
		return err
	}

	return nil
}

func getLicenseResult(cmd *cobra.Command) (*LicenseResult, error) {
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

	output, err := os.ReadFile(tmpFile.Name())
	if err != nil {
		return nil, err
	}

	var licenseResult LicenseResult
	err = json.Unmarshal(output, &licenseResult)
	if err != nil {
		return nil, err
	}
	return &licenseResult, nil
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
		// If the trial and internal license is not about to expire within 60 days, do nothing.
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
