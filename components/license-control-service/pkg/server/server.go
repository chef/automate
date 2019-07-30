package server

import (
	"context"
	"fmt"
	"strings"

	"github.com/golang/protobuf/ptypes"
	"github.com/golang/protobuf/ptypes/timestamp"
	spanlog "github.com/opentracing/opentracing-go/log"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	lc "github.com/chef/automate/api/interservice/license_control"
	"github.com/chef/automate/components/license-control-service/pkg/keys"
	"github.com/chef/automate/components/license-control-service/pkg/storage"
	"github.com/chef/automate/lib/grpc/health"
	lic "github.com/chef/automate/lib/license"
	"github.com/chef/automate/lib/tracing"
)

// LicenseControlServer is our structure representing the gRPC server backend.
type LicenseControlServer struct {
	TelemetryEnabled bool
	TelemetryURL     string
	storage          storage.CurrentBackend
	licenseParser    *keys.LicenseParser

	health *health.Service
}

type dateRange struct {
	start *timestamp.Timestamp
	end   *timestamp.Timestamp
}

// We return this error when we can't parse or validate license data
// we receive.
var errInvalidLicenseData = errors.New("invalid license data")

// NewLicenseControlServer returns a new instance of our LicenseControlServer.
func NewLicenseControlServer(ctx context.Context, backend storage.CurrentBackend,
	licenseParser *keys.LicenseParser, config *Config) *LicenseControlServer {
	return &LicenseControlServer{
		licenseParser:    licenseParser,
		TelemetryEnabled: config.TelemetryEnabled,
		TelemetryURL:     config.URL,
		health:           health.NewService(),
		storage:          backend,
	}
}

// Policy sends the constructed Policy. Currently we don't process any request input.
func (s *LicenseControlServer) Policy(ctx context.Context, req *lc.PolicyRequest) (*lc.PolicyResponse, error) {
	span, ctx := tracing.StartSpanFromContext(ctx, "policy") // nolint: ineffassign
	defer span.Finish()

	licenseData, licenseMetadata, err := s.storage.GetLicense(ctx)
	switch err.(type) {
	case *storage.NoLicenseError:
		return &lc.PolicyResponse{}, nil
	case nil:
		lic, err := s.parseLicense(ctx, licenseData)
		if err != nil {
			return nil, status.Errorf(codes.Internal, "invalid license in license store: %s", err.Error())
		}
		return &lc.PolicyResponse{Policy: generatePolicy(ctx, lic, licenseMetadata)}, nil
	default:
		return nil, status.Errorf(codes.Internal, "failed to retrieve error from storage backend: %s", err.Error())
	}
}

// License returns the license information
func (s *LicenseControlServer) License(ctx context.Context, req *lc.LicenseRequest) (*lc.LicenseResponse, error) {
	span, ctx := tracing.StartSpanFromContext(ctx, "license") // nolint: ineffassign
	defer span.Finish()

	licenseData, _, err := s.storage.GetLicense(ctx)
	switch err.(type) {
	case *storage.NoLicenseError:
		return &lc.LicenseResponse{}, nil
	case nil:
		lic, err := s.parseLicense(ctx, licenseData)
		if err != nil {
			return nil, status.Errorf(codes.Internal, "invalid license in license store: %s", err.Error())
		}
		return &lc.LicenseResponse{License: lic}, nil
	default:
		return nil, status.Errorf(codes.Internal, "failed to retrieve error from storage backend: %s", err.Error())
	}
}

// Status returns the LicenseControlServer status information
func (s *LicenseControlServer) Status(ctx context.Context, req *lc.StatusRequest) (*lc.StatusResponse, error) {
	span, ctx := tracing.StartSpanFromContext(ctx, "license_status") // nolint: ineffassign
	defer span.Finish()

	licenseData, licenseMetadata, err := s.storage.GetLicense(ctx)
	switch err.(type) {
	case *storage.NoLicenseError:
		return &lc.StatusResponse{}, nil
	case nil:
		lic, err := s.parseLicense(ctx, licenseData)
		if err != nil {
			return nil, status.Errorf(codes.Internal, "invalid license in license store: %s", err.Error())
		}

		policy := generatePolicy(ctx, lic, licenseMetadata)
		licensedPeriod := licensedPeriod(lic)
		return &lc.StatusResponse{
			LicenseId:    lic.Id,
			CustomerName: lic.Customer,
			ConfiguredAt: policy.ConfiguredAt,
			LicensedPeriod: &lc.DateRange{
				Start: licensedPeriod.start,
				End:   licensedPeriod.end,
			},
		}, nil
	default:
		return nil, status.Errorf(codes.Internal, "failed to retrieve error from storage backend: %s", err.Error())
	}
}

// Update updates the LicenseControlServer with the new license information
func (s *LicenseControlServer) Update(ctx context.Context, req *lc.UpdateRequest) (*lc.UpdateResponse, error) {
	span, ctx := tracing.StartSpanFromContext(ctx, "load_license")
	defer span.Finish()

	// load/parse A2 License
	licData := strings.TrimSpace(req.LicenseData)
	license, err := s.parseLicense(ctx, licData)
	if err != nil {
		log.WithError(err).Warn("Error parsing license")
		// Note: it might be helpful for callers to be able to
		// programmatically distinguish between bad context
		// and bad signature, to do that properly we should
		// modify the underlying license library to return
		// typed errors.
		// https://godoc.org/google.golang.org/grpc/codes
		return nil, status.Errorf(codes.InvalidArgument, err.Error())
	}

	licensedPeriod := licensedPeriod(license)
	logctx := log.WithFields(log.Fields{
		"license_id": license.Id,
		"expired_at": licensedPeriod.end,
	})

	span.LogFields(
		spanlog.String("submitted_license", licData),
		spanlog.Object("loaded_license", license),
		spanlog.Object("licensed_period", licensedPeriod),
	)

	if !req.Force && ptypes.TimestampNow().GetSeconds() > licensedPeriod.end.GetSeconds() {
		msg := fmt.Sprintf("Rejecting license ID %v, expired at %v; set force=true to override",
			license.Id, licensedPeriod.end)
		logctx.Warn(msg)
		return nil, status.Errorf(codes.InvalidArgument, msg)
	}

	logctx.Info("Applying license")
	err = s.persistLicenseToken(ctx, licData)
	switch err.(type) {
	case nil:
		return &lc.UpdateResponse{
			Updated: true,
			Message: fmt.Sprintf("Policy configured from license ID %s", license.Id),
		}, nil
	case *storage.RetriableBackendError:
		return nil, status.Error(codes.Aborted, err.Error())
	default:
		logctx.WithError(err).Error("Unable to persist license")
		return nil, status.Errorf(codes.Internal, "unable to persist license: %s", err.Error())
	}
}

//Telemetry endpoint to return telemetry configuration
func (s *LicenseControlServer) Telemetry(ctx context.Context, req *lc.TelemetryRequest) (*lc.TelemetryResponse, error) {
	return &lc.TelemetryResponse{TelemetryEnabled: s.TelemetryEnabled, TelemetryUrl: s.TelemetryURL}, nil
}

func (s *LicenseControlServer) persistLicenseToken(ctx context.Context, licenseToken string) error {
	span, ctx := tracing.StartSpanFromContext(ctx, "persist_license_token")
	defer span.Finish()

	return s.storage.SetLicense(ctx, licenseToken)
}

func (s *LicenseControlServer) parseLicense(ctx context.Context, licenseData string) (*lic.License, error) {
	span, ctx := tracing.StartSpanFromContext(ctx, "parse_license") // nolint: ineffassign
	defer span.Finish()

	license, err := s.licenseParser.Parse(licenseData)
	if err != nil {
		log.WithFields(log.Fields{
			"license_data": licenseData,
			"error":        err,
		}).Warn("unable to load license")
		return nil, errInvalidLicenseData
	}
	return license, nil
}

func licensedPeriod(license *lic.License) dateRange {
	var endDate *timestamp.Timestamp
	startDate := ptypes.TimestampNow()
	for _, l := range license.Entitlements {
		if l.GetStart().GetSeconds() < startDate.GetSeconds() {
			startDate = l.Start
		}
		if l.GetEnd().GetSeconds() > endDate.GetSeconds() {
			endDate = l.GetEnd()
		}
	}
	return dateRange{
		start: startDate,
		end:   endDate,
	}
}

// Generate a Policy based on a given license. Decoding will be attempted with
// all available public keys until one is found or the list is exhausted.
func generatePolicy(ctx context.Context, license *lic.License, metadata keys.LicenseMetadata) *lc.Policy {
	span, ctx := tracing.StartSpanFromContext(ctx, "generate_policy") // nolint: ineffassign
	defer span.Finish()

	return &lc.Policy{
		LicenseId:    license.Id,
		Valid:        true, // Always true if license and signature check out
		Capabilities: buildCapabilities(ctx, license.Entitlements),
		Rules:        buildRules(ctx, license.Entitlements),
		ConfiguredAt: &timestamp.Timestamp{Seconds: metadata.ConfiguredAt.Unix()},
	}
}

// Convert license entitlements to policy capabilities, to preserve the service
// boundary. (policy is the "published language" of this service, not license.)
func buildCapabilities(ctx context.Context, es []*lic.Entitlement) []*lc.Capability {
	span, ctx := tracing.StartSpanFromContext(ctx, "build_capabilities") // nolint: ineffassign
	defer span.Finish()
	cs := make([]*lc.Capability, 0, len(es))
	for _, c := range es {
		cs = append(cs, &lc.Capability{
			Name:    c.Name,
			Measure: c.Measure,
			Limit:   c.Limit,
			Start:   c.Start,
			End:     c.End,
		})
	}

	return cs
}

// Compile business rules for client behavior based on the configured license
func buildRules(ctx context.Context, entitlements []*lic.Entitlement) map[string]string {
	span, ctx := tracing.StartSpanFromContext(ctx, "build_rules") // nolint: ineffassign
	defer span.Finish()

	rules := make(map[string]string)

	// stand-in for actual business logic
	if len(entitlements) > 0 {
		rules["san_dimas_high_school_football"] = "true"
	}

	return rules
}
