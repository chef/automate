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

func checkLicenseStatusForExpiry(cmd *cobra.Command, args []string) error {
	fileName := "/tmp/license"
	err := commandPrePersistent(cmd)
	if err != nil {
		return status.Wrap(err, status.CommandExecutionError, "unable to set command parent settings")
	}

	cmd1 := exec.Command("chef-automate", "license", "status", "--result-json", fileName)
	// Not checking for error, as if the license is expired it will still return error and for commercial license a grace period is required
	cmd1.Output()

	licenseResult, err := readFileAndMarshal(fileName)
	if err != nil {
		return err
	}

	err = checkLicenseExpiry(licenseResult)

	// if grace period active, executes below function to give warnings
	WarnIfLicenseNearExpiry(licenseResult)

	if err != nil {
		return err
	}

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
	commercial := "commercial"
	if licenseResult.Result.LicenseId == "" {
		if licenseResult.ErrorType != "" {
			return status.New(
				status.DeploymentServiceCallError,
				licenseResult.ErrorDescription,
			)
		}
		cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn("Please apply a license. Please contact sales@chef.io to have your Chef Automate license.")

	}
	licenseValidDate := time.Unix(licenseResult.Result.ExpirationDate.Seconds, 0) // gives unix time stamp in utc

	// If the license type is commercial, adding grace period of 1 month
	licenseResult.Result.GracePeriod = false
	if licenseResult.Result.LicenseType == commercial {
		if licenseValidDate.Before(time.Now()) {
			// License expired, check if within grace period
			// Adding grace period for 30 days i.e. one month
			gracePeriodEnd := licenseValidDate.AddDate(0, 0, 30)

			// checks if current date and time is before the end of grace period
			if gracePeriodEnd.After(time.Now()) {
				// if the condition is true make the grace_period as true
				licenseResult.Result.GracePeriod = true
				licenseValidDate = gracePeriodEnd
			}

		}

	}
	// Check if the license (including the grace period) is expired
	if licenseValidDate.Before(time.Now()) {
		return status.New(
			status.LicenseError,
			"This license and grace period have expired. Please contact sales@chef.io to renew your Chef Automate license.",
		)
	}

	return nil
}

func WarnIfLicenseNearExpiry(licenseResult *LicenseResult) {
	// Calculate the license valid date, which includes the grace period also
	licenseValidDate := time.Unix(licenseResult.Result.ExpirationDate.Seconds, 0)

	// Check if the license is expired
	if licenseValidDate.Before(time.Now()) {

		daysAgo := int(time.Since(licenseValidDate).Hours() / 24)

		// If the license is expired, check if it's within the grace period
		if licenseResult.Result.GracePeriod {
			// Calculate days left until the grace period ends
			daysLeft := int(time.Until(licenseValidDate).Hours() / 24)

			if daysLeft > 0 {
				// Warning during the grace period
				daysIntoGracePeriod := 30 - daysLeft
				cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your license expired %d days ago, but you are now in the grace period of %d. Please apply a new license.\n", daysAgo, daysIntoGracePeriod))
			} else {
				// Warning if the grace period has ended
				cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Your license expired %d ago and you are out of 30 days of grace period . Please apply a new license to continue using the software.", daysAgo))
			}
		}
	}
}
