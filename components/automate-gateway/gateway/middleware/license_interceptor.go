package middleware

import (
	"context"
	"time"

	"github.com/chef/automate/api/interservice/license_control"
	license_middleware "github.com/chef/automate/components/automate-gateway/gateway/middleware/license"
	"github.com/pkg/errors"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

type licenseInterceptor struct {
	licenseClient license_middleware.ILicense
	licenseStatus *LicenseStatus
}

func NewLicenseInterceptor(licenseInterserviceClient license_control.LicenseControlServiceClient) ILicenseInterceptor {
	licenseClient := license_middleware.NewLicenseClient(licenseInterserviceClient)
	return &licenseInterceptor{
		licenseClient: licenseClient,
	}
}

const (
	commercial = "commercial"
)

// allowAPiList consists of every api endpoints which is allowed without validating license details
var allowApiList = []string{"/chef.automate.api.event_feed.EventFeedService/GetEventTypeCounts",
	"/chef.automate.api.telemetry.Telemetry/GetTelemetryConfiguration",
	"/chef.automate.api.license.License/GetStatus",
	"/chef.automate.api.deployment.Deployment/GetVersion",
	"/chef.automate.api.iam.v2.Policies/IntrospectAllProjects",
	"/chef.automate.api.license.License/RequestLicense",
	"/chef.automate.api.iam.v2.Authorization/IntrospectAll",
	"/chef.automate.api.license.License/ApplyLicense",
	"/chef.automate.api.iam.v2.Policies/ListProjects",
	"/chef.automate.api.iam.v2.Users/GetUser",
	"/chef.automate.api.user_settings.UserSettingsService/GetUserSettings",
	"/chef.automate.api.iam.v2.Rules/ApplyRulesStatus",
	"/chef.automate.api.iam.v2.Users/CreateUser",
	"/chef.automate.api.iam.v2.Users/GetUser",
	"/chef.automate.api.iam.v2.Teams/GetTeam",
	"/chef.automate.api.iam.v2.Teams/GetTeamMembership",
	"/chef.automate.api.iam.v2.Teams/AddTeamMembers",
	"/chef.automate.api.legacy.LegacyDataCollector/Status",
	"/chef.automate.api.iam.v2.Users/UpdateUser",
	"/chef.automate.api.iam.v2.Teams/GetTeam",
	"/chef.automate.api.iam.v2.Teams/GetTeamMembership",
	"/chef.automate.api.iam.v2.Policies/ListPolicyMembers"}

// refreshLicenseList consists of api endpoints after which the license details needs to be fetchef again
var refreshLicenseList = []string{"/chef.automate.api.license.License/RequestLicense", "/chef.automate.api.license.License/ApplyLicense", "/chef.automate.api.iam.v2.Tokens/CreateToken", "/chef.automate.api.iam.v2.Policies/AddPolicyMembers", "/chef.automate.api.Gateway/GetVersion", "/chef.automate.api.ingest.ChefIngester/GetVersion", "/chef.automate.api.cfgmgmt.ConfigMgmt/GetVersion", " /chef.automate.api.compliance.reporting.v1.ReportingService/GetVersion", "/chef.automate.api.iam.v2.Users/DeleteUser"}

type LicenseStatus struct {
	//LicenseDetails information regarding license
	LicenseDetails *license_middleware.LicenseDetails
	//Till When the details are valid
	DetailsValidity time.Time
	//LicenseDetailsRefresh hard refresh details even if the DetailsValidity is valid
	LicenseDetailsRefresh bool
}

// UnaryInterceptor returns a grpc UnaryServerInterceptor that performs license check
func (l *licenseInterceptor) UnaryServerInterceptor() grpc.UnaryServerInterceptor {
	return func(
		ctx context.Context,
		req interface{},
		info *grpc.UnaryServerInfo,
		handler grpc.UnaryHandler) (interface{}, error) {

		if contains(refreshLicenseList, info.FullMethod) {
			l.licenseStatus = &LicenseStatus{
				LicenseDetailsRefresh: true,
			}
		}

		if contains(allowApiList, info.FullMethod) {
			h, err := handler(ctx, req)
			return h, err
		}

		err := l.refreshLicenseDetails(ctx)
		if err != nil {
			return nil, errors.Wrap(err, "unable to refresh details")
		}

		if !l.isValidLicense() {
			return nil, status.Errorf(codes.PermissionDenied, "License is expired, please apply a new license")
		}

		h, err := handler(ctx, req)

		return h, err

	}
}

func (l *licenseInterceptor) StreamServerInterceptor() grpc.StreamServerInterceptor {
	return func(req interface{}, ss grpc.ServerStream, info *grpc.StreamServerInfo, handler grpc.StreamHandler) error {

		if contains(refreshLicenseList, info.FullMethod) {
			l.licenseStatus = &LicenseStatus{
				LicenseDetailsRefresh: true,
			}
		}

		if contains(allowApiList, info.FullMethod) {
			i := interceptedServerStream{ctx: context.Background(), ServerStream: ss}
			return handler(req, &i)
		}

		err := l.refreshLicenseDetails(ss.Context())
		if err != nil {
			return errors.Wrap(err, "unable to refresh details")
		}

		if !l.isValidLicense() {
			return status.Errorf(codes.PermissionDenied, "License is expired, please apply a new license")
		}

		i := interceptedServerStream{ctx: context.Background(), ServerStream: ss}
		return handler(req, &i)

	}
}

func (l *licenseInterceptor) getLicenseDetails(ctx context.Context) error {
	licenseDetails, err := l.licenseClient.GetLicenseDetails(ctx)
	if err != nil {
		return errors.Wrap(err, "Unable to fetch license details")
	}

	//Marking the validity of the license details for next 6 hours of time
	validity := time.Now().Add(time.Hour * time.Duration(6))

	l.licenseStatus = &LicenseStatus{
		LicenseDetails:  licenseDetails,
		DetailsValidity: validity,
	}

	return nil

}

func (l *licenseInterceptor) isValidLicense() bool {

	if l.licenseStatus.LicenseDetails == nil {
		return false
	}
	licenseValidDate := l.licenseStatus.LicenseDetails.ExpiryDate

	//If the license type is commercial, adding grace period of 1 week
	if l.licenseStatus.LicenseDetails.LicenseType == commercial {
		//Adding grace period for 60 days i.e. one week
		licenseValidDate = licenseValidDate.AddDate(0, 0, 60)
	}

	if licenseValidDate.Before(time.Now()) {
		return false
	}

	return true
}

func (l *licenseInterceptor) refreshLicenseDetails(ctx context.Context) error {
	// Always refresh license details if the cache is invalid or expired
	if l.licenseStatus == nil ||
		l.licenseStatus.DetailsValidity.Before(time.Now()) ||
		l.licenseStatus.LicenseDetailsRefresh ||
		!l.isValidLicense() {
		err := l.getLicenseDetails(ctx)
		if err != nil {
			return errors.Wrap(err, "Unable to fetch license details")
		}
	}
	return nil
}

func contains(listOfApis []string, functionName string) bool {

	for _, name := range listOfApis {
		if name == functionName {
			return true
		}
	}

	return false

}
