package main

import (
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
	log.Println("****************** checkLicenseStatusForExpiry *********************")
	err := commandPrePersistent(cmd)
	if err != nil {
		return status.Wrap(err, status.CommandExecutionError, "unable to set command parent settings")
	}
	licenseResult, err := getLicenseResult(cmd)
	fmt.Printf("licenseResult: %v\n", licenseResult)
	fmt.Println(err)
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
	log.Println("********** welcome to getlicense fucntion************ ")
	fileName := "/tmp/license"

	cmd1 := exec.Command("chef-automate", "license", "status", "--result-json", fileName)
	// Not checking for error, as if the license is expired it will still return error and for commercial license a grace period is required
	log.Println("********** In getlicense fucntion************ ")
	output, err := cmd1.Output()
	log.Println("******** after *****************")
	fmt.Println(output)
	if err != nil {
		log.Printf("Error executing command: %v\n", err)
		//return nil, err
	}

	log.Println("Command executed successfully")
	log.Println("**********Marshal start************ ")
	licenseResult, err := readFileAndMarshal(fileName)
	log.Println("********** Marshal end************ ")
	return licenseResult, err
}

func WarnLicenseStatusForExpiry(cmd *cobra.Command, args []string) error {
	log.Println("****************** WarnLicenseStatusForExpiry *********************")
	//fmt.Printf("*****************")
	err := commandPrePersistent(cmd)
	//log.Println(err)
	if err != nil {

		return status.Wrap(err, status.CommandExecutionError, "unable to set command parent settings")
	}
	log.Println("***********cmdworking****************")
	licenseResult, err := getLicenseResult(cmd)
	if licenseResult.Result.LicenseId == "" {
		log.Println("Licence not available")
	}
	log.Println("***********LicenseResult****************")
	if err != nil {
		//fmt.Print(err)
		return err
	}
	//log.Println("*************warnIfLicenseNearExpiry**************")
	warnIfLicenseNearExpiry(licenseResult)
	//log.Println("*************End**************")
	return nil
}

func readFileAndMarshal(fileName string) (*LicenseResult, error) {
	log.Println("*****************welcome readmarshal starts ***************")
	var result LicenseResult

	byteValue, err := os.ReadFile(fileName)
	if err != nil {
		return nil, err
	}

	defer os.Remove(fileName)
	json.Unmarshal(byteValue, &result)
	log.Println("****************** readFileAndMarshal *********************")

	return &result, nil
}

func checkLicenseExpiry(licenseResult *LicenseResult) error {
	log.Println("****************** checkLicenseExpiry *********************")

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
	daysAgo := int(time.Since(licenseValidDate).Hours() / 24)

	gracePeriodDate := licenseValidDate.AddDate(0, 0, gracePeriodDuration)

	licenseDate := licenseValidDate.Format("02-01-2006")
	// If the license type is commercial, adding grace period of 1 month
	if licenseResult.Result.LicenseType == commercial {

		graceDate := gracePeriodDate.Format("02-01-2006")
		if !licenseResult.Result.GracePeriod {

			currentTime := time.Now()
			daysUntilExpiration := int(licenseValidDate.Sub(currentTime).Hours() / 24)

			if daysUntilExpiration <= aboutToExpire && daysUntilExpiration > 0 {
				cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Chef Automate license is expiring on %s. Your grace period will be %d days. Please update your license before %s to avoid any service disruption", licenseDate, gracePeriodDuration, graceDate))

			} else {
				status.New(
					status.LicenseError, fmt.Sprintf(`Your Chef Automate license has expired.

				Contact us to get a license. If you already have one, enter it now.`))
			}
		} else {
			cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Chef Automate license has expired on %s. We request you to get a new license by connecting with your account team. To avoid service disruption, please update a valid license by %s.", licenseDate, graceDate))

		}
	} else {
		if daysAgo > 0 {
			status.New(
				status.LicenseError, fmt.Sprintf("Chef Automate license has expired on %s. We request you to get a new license by connecting with your account team. To avoid service disruption, please update a valid license by %s.", licenseDate, licenseDate))

		}
	}

	return nil
}

func warnIfLicenseNearExpiry(licenseResult *LicenseResult) {
	// Calculate the license valid date
	licenseValidDate := time.Unix(licenseResult.Result.ExpirationDate.Seconds, 0)

	gracePeriodDuration := 60
	aboutToExpire := 60

	gracePeriodDate := licenseValidDate.AddDate(0, 0, gracePeriodDuration)
	fmt.Println("Warn function")
	fmt.Println(licenseResult)
	if licenseResult.Result.LicenseType == commercial {
		log.Println("****commercial*****")
		licenseDate := licenseValidDate.Format("02-01-2006")
		graceDate := gracePeriodDate.Format("02-01-2006")
		if !licenseResult.Result.GracePeriod {
			//daysAgo := int(time.Since(licenseValidDate).Hours() / 24)
			//fmt.Println("grace period in if case")
			//want to check if license is about to expire
			currentTime := time.Now()
			daysUntilExpiration := int(licenseValidDate.Sub(currentTime).Hours() / 24)

			if daysUntilExpiration <= aboutToExpire && daysUntilExpiration > 0 {
				cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Chef Automate license is expiring on %s. Your grace period will be %d days. Please update your license before %s to avoid any service disruption", licenseDate, gracePeriodDuration, graceDate))
				//want to check is both license and grace period r expired
			} else {
				cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf(`Your Chef Automate license has expired.

				Contact us to get a license. If you already have one, enter it now.`))
			}
			//want to check if license is expired and grace period is active.
		} else {

			cli.NewWriter(os.Stdout, os.Stderr, os.Stdin).Warn(fmt.Sprintf("Chef Automate license has expired on %s. We request you to get a new license by connecting with your account team. To avoid service disruption, please update a valid license by %s.", licenseDate, graceDate))
			fmt.Println("not commercial license")
		}

	}
	fmt.Println("****end*****")
}
