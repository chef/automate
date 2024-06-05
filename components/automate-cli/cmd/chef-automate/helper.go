package main

import (
	"fmt"
	"time"
)

// WarnIfLicenseNearExpiry checks the license status and warns if the license is near expiry or in the grace period.

func WarnIfLicenseNearExpiry(licens *LicenseResult) error {

	licenseResult, err := readFileAndMarshal(fileName)
	if err != nil {
		return err
	}
	if licenseResult.Result.GracePeriod {
		licenseValidDate := time.Unix(licenseResult.Result.ExpirationDate.Seconds, 0)
		gracePeriodDays := 30

		daysLeft := int(time.Until(licenseValidDate).Hours() / 24)

		switch {
		case daysLeft > 0 && daysLeft <= gracePeriodDays:
			daysLeftGracePeriod := gracePeriodDays - daysLeft
			fmt.Printf("Warning: Your license expired %d days ago, but you are now in the grace period of day %d. Please apply a new license.\n", daysLeft, daysLeftGracePeriod)
		case daysLeft == 0:
			fmt.Printf("Warning: Your license has expired today. Please apply a new license.\n")
		case daysLeft < 0:
			return fmt.Errorf("your license and grace period have expired. Please apply a new license to continue using Chef Automate.")
		}
	}
	return nil
}
