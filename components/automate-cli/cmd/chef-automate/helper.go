package main

import (
	"fmt"
	"time"
)

// WarnIfLicenseNearExpiry checks the license status and warns if the license is near expiry or in the grace period.

func WarnIfLicenseNearExpiry(licenseResult *LicenseResult){
    // Check if the license is in the grace period
    if licenseResult.Result.GracePeriod {
        licenseValidDate := time.Unix(licenseResult.Result.ExpirationDate.Seconds, 0)

        // Calculate days left until the grace period ends
        daysLeft := int(time.Until(licenseValidDate).Hours() / 24)

        if daysLeft > 0 {
            // Warning during the grace period
            writer.Warn("Your license expired %d days ago, but you are now in the grace period. Please apply a new license.\n", -daysLeft)
        } else {
            // Error if the grace period has ended
            writer.Warn("your license and grace period have expired. Please apply a new license to continue using the software.")
        }
    }
}
