package main

import (
	"encoding/json"
	"fmt"
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

	fileName := "/tmp/license"

	cmd1 := exec.Command("chef-automate", "license", "status", "--result-json", fileName)
	// Not checking for error, as if the license is expired it will still return error and for commercial license a grace period is required
	cmd1.Output()

	licenseResult, err := readFileAndMarshal(fileName)
	return licenseResult, err
}

func WarnLicenseStatusForExpiry(cmd *cobra.Command, args []string) error {
	err := commandPrePersistent(cmd)
	if err != nil {
		return status.Wrap(err, status.CommandExecutionError, "unable to set command parent settings")
	}
	licenseResult, err := getLicenseResult(cmd)
	if err != nil {
		//fmt.Print(err)
		return err
	}
	warnIfLicenseNearExpiry(licenseResult)
	return nil
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

	// If the license type is commercial, adding grace period of 1 month
	if licenseValidDate.Before(time.Now()) {
		if licenseResult.Result.LicenseType == commercial {
			// Check if the license (including the grace period) is expired
			if !licenseResult.Result.GracePeriod {
				return status.New(
					status.LicenseError,
					"This license and grace period have expired. Please contact sales@chef.io to renew your Chef Automate license.",
				)
			} else {
				cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your license expired is expired,Please apply a new license.\n"))
			}

		} else {
			return status.New(
				status.LicenseError,
				"This license have expired. Please contact sales@chef.io to renew your Chef Automate license.",
			)
		}

	}
	return nil
}

func warnIfLicenseNearExpiry(licenseResult *LicenseResult) {
	// Calculate the license valid date
	licenseValidDate := time.Unix(licenseResult.Result.ExpirationDate.Seconds, 0)
	// Check if the license is expired
	if licenseValidDate.Before(time.Now()) {
		daysAgo := int(time.Since(licenseValidDate).Hours() / 24)
		if licenseResult.Result.LicenseType == commercial {
			// If the license is expired, check if it's within the grace period
			if licenseResult.Result.GracePeriod {
				// Warning during the grace period
				cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your license expired %d days ago,Please apply a license.Please contact sales@chef.io to have your Chef Automate license.\n", daysAgo))

			} else {
				// Warning if the grace period has ended
				cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your license expired %d ago and you are out of 30 days of grace period . Please apply a new license to continue using the software.", daysAgo))
			}
		} else {
			cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your license expired %d days ago,Please apply a license.Please contact sales@chef.io to have your Chef Automate license.\n", daysAgo))
		}
	}
}
