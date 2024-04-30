package middleware

import (
	"context"
	"fmt"

	"github.com/chef/automate/api/interservice/license_control"
	"google.golang.org/grpc"
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

// UnaryInterceptor returns a grpc UnaryServerInterceptor that performs license check
func (l *licenseInterceptor) UnaryServerInterceptor() grpc.UnaryServerInterceptor {
	return func(
		ctx context.Context,
		req interface{},
		info *grpc.UnaryServerInfo,
		handler grpc.UnaryHandler) (interface{}, error) {

		// licenseDetails, err := l.license_client.Status(ctx, &license_control.StatusRequest{})
		// if err != nil {
		// 	return nil, errors.Wrap(err, "unable to get the status of the license")
		// }

		// expireTime := time.Unix(licenseDetails.LicensedPeriod.End.GetSeconds(), 0).UTC()
		// currentTime := time.Now()

		// if expireTime.Before(currentTime) {
		// 	return nil, status.Errorf(codes.FailedPrecondition, "License expired, please apply the new license")

		// }

		return nil, fmt.Errorf("unable to perform action license failed")

	}
}

func (l *licenseInterceptor) StreamServerInterceptor() grpc.StreamServerInterceptor {
	return func(req interface{}, ss grpc.ServerStream, info *grpc.StreamServerInfo, handler grpc.StreamHandler) error {

		return fmt.Errorf("unable to perform action license failed")

	}
}
