package license

import (
	"context"
	"time"

	"github.com/chef/automate/api/interservice/license_control"
	"github.com/pkg/errors"
)

type ILicense interface {
	GetLicenseDetails(ctx context.Context) (*LicenseDetails, error)
}

type LicenseDetails struct {
	LicenseType string
	ExpiryDate  time.Time
	GracePeriod bool
}

type License struct {
	licenseClient license_control.LicenseControlServiceClient
}

// GetLicenseDetails implements ILicense.
func (l *License) GetLicenseDetails(ctx context.Context) (*LicenseDetails, error) {

	var licenseDetails *LicenseDetails
	var expiryDate time.Time
	var gracePeriod bool
	//Getting the license details for license service
	licenseDetailsResponse, err := l.licenseClient.Status(ctx, &license_control.StatusRequest{})
	if err != nil {
		return licenseDetails, errors.Wrap(err, "unable to get the status of the license")
	}

	if licenseDetailsResponse.LicenseId != "" {
		if licenseDetailsResponse.LicensedPeriod.End.GetSeconds() != int64(0) {
			expiryDate = time.Unix(licenseDetailsResponse.LicensedPeriod.End.GetSeconds(), 0).UTC()
		}
		gracePeriod = licenseDetailsResponse.GracePeriod
		licenseDetails = &LicenseDetails{
			LicenseType: licenseDetailsResponse.LicenseType,
			ExpiryDate:  expiryDate,
			GracePeriod: gracePeriod,
		}
	}

	return licenseDetails, nil

}

func NewLicenseClient(licenseClient license_control.LicenseControlServiceClient) ILicense {
	return &License{
		licenseClient: licenseClient,
	}

}
