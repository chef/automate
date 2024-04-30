package middleware

import (
	"context"
	"fmt"
	"time"

	"github.com/chef/automate/api/interservice/license_control"
	"github.com/pkg/errors"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

type licenseInterceptor struct {
	license_client license_control.LicenseControlServiceClient
	isError        bool
}

func NewLicenseInterceptor(license_client license_control.LicenseControlServiceClient) ILicenseInterceptor {
	return &licenseInterceptor{
		license_client: license_client,
		isError:        false,
	}
}

const (
	commercial = "commercial"
)

type LicenseStatus struct {
	LicenseDetails  *LicenseDetails
	DetailsValidity time.Time
}
type LicenseDetails struct {
	licenseType string
	expiryDate  time.Time
}

var licenseStatus *LicenseStatus

// UnaryInterceptor returns a grpc UnaryServerInterceptor that performs license check
func (l *licenseInterceptor) UnaryServerInterceptor() grpc.UnaryServerInterceptor {
	return func(
		ctx context.Context,
		req interface{},
		info *grpc.UnaryServerInfo,
		handler grpc.UnaryHandler) (interface{}, error) {

		//if the global variable license status containing details is nil or If its not nil checking the details validity.
		//If the details validity is before the current time refersh the license details
		if (licenseStatus != nil && licenseStatus.DetailsValidity.Before(time.Now())) || licenseStatus == nil {
			fmt.Println("Getting details again----------------")
			err := l.getLicenseDetails(ctx)
			if err != nil {
				return nil, errors.Wrap(err, "Unable to fetch license details")
			}
		}

		fmt.Println("-------------- licene we got " + licenseStatus.LicenseDetails.licenseType)
		if !l.isValidLicense() {
			return nil, status.Errorf(codes.PermissionDenied, "License is expired, please apply a new license")

		}

		return handler(ctx, req)

	}
}

func (l *licenseInterceptor) StreamServerInterceptor() grpc.StreamServerInterceptor {
	return func(req interface{}, ss grpc.ServerStream, info *grpc.StreamServerInfo, handler grpc.StreamHandler) error {

		i := interceptedServerStream{ServerStream: ss}
		return handler(req, &i)

	}
}

func (l *licenseInterceptor) getLicenseDetails(ctx context.Context) error {
	//Getting the license details for license service
	licenseDetailsResponse, err := l.license_client.Status(ctx, &license_control.StatusRequest{})
	if err != nil {
		return errors.Wrap(err, "unable to get the status of the license")
	}

	licenseDetails := &LicenseDetails{
		licenseType: licenseDetailsResponse.LicenseType,
		expiryDate:  time.Unix(licenseDetailsResponse.LicensedPeriod.End.GetSeconds(), 0).UTC(),
	}

	//Marking the validity of the license details for next 6 hours of time
	validity := time.Now().Add(time.Minute * time.Duration(2))

	licenseStatus = &LicenseStatus{
		LicenseDetails:  licenseDetails,
		DetailsValidity: validity,
	}

	return nil

}

func (l *licenseInterceptor) isValidLicense() bool {

	if licenseStatus.LicenseDetails == nil {
		return false
	}
	licenseValidDate := licenseStatus.LicenseDetails.expiryDate

	//If the license type is commercial, adding grace period of 1 week
	if licenseStatus.LicenseDetails.licenseType == commercial {
		//Adding grace period for 7 days i.e. one week
		licenseValidDate = licenseValidDate.AddDate(0, 0, 7)
	}

	if licenseValidDate.Before(time.Now()) {
		return false
	}

	return true
}
