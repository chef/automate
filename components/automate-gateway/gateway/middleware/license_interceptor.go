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

func NewLicenseInterceptor() ILicenseInterceptor {
	return &licenseInterceptor{
		isError: false,
	}
}

// UnaryInterceptor returns a grpc UnaryServerInterceptor that performs license check
func (l *licenseInterceptor) UnaryServerInterceptor() grpc.UnaryServerInterceptor {
	return func(
		ctx context.Context,
		req interface{},
		info *grpc.UnaryServerInfo,
		handler grpc.UnaryHandler) (interface{}, error) {

		return nil, fmt.Errorf("unable to perform action license failed")

	}
}

func (l *licenseInterceptor) StreamServerInterceptor() grpc.StreamServerInterceptor {
	return func(req interface{}, ss grpc.ServerStream, info *grpc.StreamServerInfo, handler grpc.StreamHandler) error {

		return fmt.Errorf("unable to perform action license failed")

	}
}
