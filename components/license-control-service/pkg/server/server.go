package server

import (
	"context"
	"fmt"
	"io/ioutil"
	"os"
	"strings"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/golang/protobuf/ptypes/timestamp"
	spanlog "github.com/opentracing/opentracing-go/log"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	lc "github.com/chef/automate/api/interservice/license_control"
	"github.com/chef/automate/components/license-control-service/pkg/keys"
	"github.com/chef/automate/lib/grpc/health"
	lic "github.com/chef/automate/lib/license"
	"github.com/chef/automate/lib/tracing"
)

// LicenseControlServer is our structure representing the gRPC server backend.
type LicenseControlServer struct {
	license        *lic.License
	policy         *lc.Policy
	publicKeys     map[string][]byte
	licensedPeriod *dateRange
	config         *Config
	health         *health.Service
}

type dateRange struct {
	start *timestamp.Timestamp
	end   *timestamp.Timestamp
}

// We return this error when we can't parse or validate license data
// we receive.
var errInvalidLicenseData = errors.New("Invalid license data")

// NewLicenseControlServer returns a new instance of our LicenseControlServer.
func NewLicenseControlServer(ctx context.Context, config *Config) *LicenseControlServer {
	publicKeys := keys.LoadPublicKeys(keys.BuiltinKeyData)

	srv := &LicenseControlServer{
		publicKeys: publicKeys,
		config:     config,
		health:     health.NewService(),
	}

	if _, err := os.Stat(config.LicenseTokenPath); err == nil {
		err := srv.restoreLicense(ctx, config.LicenseTokenPath)
		if err != nil {
			log.WithFields(
				log.Fields{
					"error":        err,
					"license_path": config.LicenseTokenPath,
				},
			).Warn("Unable to load license from file, proceeding with no license.")
			// We are explicitly not returning early. We want the service to load
			// without a license. In this case, it is expected that if we encounter
			// an error loading the license we will just act as if we don't have a
			// license at all.
		}
	}

	return srv
}

func (s *LicenseControlServer) restoreLicense(
	ctx context.Context, licenseTokenPath string,
) error {
	span, ctx := tracing.StartSpanFromContext(ctx, "restore_license")
	defer span.Finish()

	span.LogFields(
		spanlog.String("license_path", licenseTokenPath),
	)

	f, err := ioutil.ReadFile(licenseTokenPath)
	if err != nil {
		return err
	}

	cleanedFile := strings.TrimSpace(string(f))
	license, policy, licensedPeriod, err := s.loadLicenseData(ctx, cleanedFile)
	if err != nil {
		return err
	}

	s.cacheState(ctx, license, policy, licensedPeriod)

	return nil
}

func (s *LicenseControlServer) cacheState(
	ctx context.Context, license *lic.License, policy *lc.Policy, licensedPeriod *dateRange,
) bool {
	span, ctx := tracing.StartSpanFromContext(ctx, "cache_state") // nolint: ineffassign
	defer span.Finish()

	s.license = license
	s.policy = policy
	s.licensedPeriod = licensedPeriod

	span.LogFields(
		spanlog.Object("license", license),
		spanlog.Object("policy", policy),
		spanlog.Object("license_period", licensedPeriod),
	)

	return true
}

func (s *LicenseControlServer) persistLicenseToken(ctx context.Context, licenseToken string) error {
	span, ctx := tracing.StartSpanFromContext(ctx, "persist_license_token") // nolint: ineffassign
	defer span.Finish()

	// Include newline to ensure compatibility with generated tokens
	return ioutil.WriteFile(s.config.LicenseTokenPath, []byte(licenseToken+"\n"), 0644)
}

func (s *LicenseControlServer) loadLicenseData(ctx context.Context,
	licenseData string) (*lic.License, *lc.Policy, *dateRange, error) {
	span, ctx := tracing.StartSpanFromContext(ctx, "load_license_data")
	defer span.Finish()

	logWarn := func(step string, err error) {
		log.WithFields(log.Fields{
			"license_data": licenseData,
			"error":        err,
			"step":         step,
		},
		).Warn("unable to load license")
	}

	publicKeySha256, err := lic.GetKeySha256(licenseData)
	if err != nil {
		logWarn("GetKeySha256", err)
		return nil, nil, nil, errInvalidLicenseData
	}

	readSpan := tracing.StartSpan("read_license", tracing.ChildOf(span.Context()))
	license, err := lic.Read(licenseData, s.publicKeys[publicKeySha256])
	readSpan.Finish()
	if err != nil {
		logWarn("Read", err)
		return nil, nil, nil, errInvalidLicenseData
	}

	policy := generatePolicy(ctx, license)
	licensedPeriod := determineLicensedPeriod(ctx, license)

	return license, policy, licensedPeriod, nil
}

func determineLicensedPeriod(ctx context.Context, license *lic.License) *dateRange {
	span, ctx := tracing.StartSpanFromContext(ctx, "determine_licensed_period") // nolint: ineffassign
	defer span.Finish()

	var endDate *timestamp.Timestamp

	// Initialize startDate at now, because otherwise we are starting at nil
	// which is equivalent to 1970
	startDate := ptypes.TimestampNow()

	for _, l := range license.Entitlements {
		if l.Start.GetSeconds() < startDate.GetSeconds() {
			startDate = l.Start
		}

		if l.End.GetSeconds() > endDate.GetSeconds() {
			endDate = l.End
		}
	}

	return &dateRange{start: startDate, end: endDate}
}

// Generate a Policy based on a given license. Decoding will be attempted with
// all available public keys until one is found or the list is exhausted.
func generatePolicy(ctx context.Context, license *lic.License) *lc.Policy {
	span, ctx := tracing.StartSpanFromContext(ctx, "generate_policy") // nolint: ineffassign
	defer span.Finish()

	return &lc.Policy{
		LicenseId:    license.Id,
		Valid:        true, // Always true if license and signature check out
		Capabilities: buildCapabilities(ctx, license.Entitlements),
		Rules:        buildRules(ctx, license.Entitlements),
		ConfiguredAt: &timestamp.Timestamp{Seconds: time.Now().Unix()},
	}
}

// Policy sends the constructed Policy. Currently we don't process any request input.
func (s *LicenseControlServer) Policy(ctx context.Context, req *lc.PolicyRequest) (*lc.PolicyResponse, error) {
	span, ctx := tracing.StartSpanFromContext(ctx, "policy") // nolint: ineffassign
	defer span.Finish()

	return &lc.PolicyResponse{Policy: s.policy}, nil
}

// License returns the license information
func (s *LicenseControlServer) License(ctx context.Context, req *lc.LicenseRequest) (*lc.LicenseResponse, error) {
	span, ctx := tracing.StartSpanFromContext(ctx, "license") // nolint: ineffassign
	defer span.Finish()

	return &lc.LicenseResponse{License: s.license}, nil
}

// Status returns the LicenseControlServer status information
func (s *LicenseControlServer) Status(ctx context.Context, req *lc.StatusRequest) (*lc.StatusResponse, error) {
	span, ctx := tracing.StartSpanFromContext(ctx, "license_status") // nolint: ineffassign
	defer span.Finish()

	response := &lc.StatusResponse{}

	if s.policy != nil {
		response.ConfiguredAt = s.policy.ConfiguredAt
	}

	if s.license != nil {
		response.LicenseId = s.license.Id

		response.LicensedPeriod = &lc.DateRange{
			Start: s.licensedPeriod.start,
			End:   s.licensedPeriod.end,
		}

		response.CustomerName = s.license.Customer
	}

	return response, nil
}

// Update updates the LicenseControlServer with the new license information
func (s *LicenseControlServer) Update(ctx context.Context, req *lc.UpdateRequest) (*lc.UpdateResponse, error) {
	span, ctx := tracing.StartSpanFromContext(ctx, "load_license")
	defer span.Finish()

	// load/parse A2 License
	licData := strings.TrimSpace(req.LicenseData)
	license, policy, licensedPeriod, err := s.loadLicenseData(ctx, licData)
	if err != nil {
		log.WithFields(
			log.Fields{"error": err.Error()},
		).Warn("Error parsing license")

		// Note: it might be helpful for callers to be able to
		// programmatically distinguish between bad context
		// and bad signature, to do that properly we should
		// modify the underlying license library to return
		// typed errors.
		// https://godoc.org/google.golang.org/grpc/codes
		return nil, status.Errorf(codes.InvalidArgument, err.Error())
	}

	span.LogFields(
		spanlog.String("submitted_license", licData),
		spanlog.Object("loaded_license", license),
		spanlog.Object("loaded_policy", policy),
		spanlog.Object("licensed_period", licensedPeriod),
	)

	if !req.Force && ptypes.TimestampNow().GetSeconds() > licensedPeriod.end.GetSeconds() {
		msg := fmt.Sprintf("Rejecting license ID %v, expired at %v; set force=true to override",
			license.Id, licensedPeriod.end)
		log.WithFields(
			log.Fields{
				"license_id": license.Id,
				"expired_at": licensedPeriod.end,
			},
		).Warn(msg)

		return nil, status.Errorf(codes.InvalidArgument, msg)
	} else {
		msg := fmt.Sprintf("Applying license ID %v, expired at %v", license.Id, licensedPeriod.end)
		log.WithFields(
			log.Fields{
				"license_id": license.Id,
				"expired_at": licensedPeriod.end,
			},
		).Warn(msg)
	}

	// If a license is already loaded, and its ID matches the submitted license
	if s.license != nil && license.Id == s.license.Id {
		log.WithFields(
			log.Fields{"license_id": s.license.Id},
		).Warn("Submitted license already configured")

		// Unless force is true...
		if !req.Force {
			return &lc.UpdateResponse{
					Updated:   false,
					Message:   fmt.Sprintf("License ID %s already set", license.Id),
					Duplicate: true,
				},
				nil
		}

		log.WithFields(
			log.Fields{"license_id": s.license.Id},
		).Warn("License reconfigure forced")
	}

	// Persist the license to disk
	if err := s.persistLicenseToken(ctx, licData); err != nil {
		log.WithFields(
			log.Fields{
				"license_id": license.Id,
				"error":      err,
			},
		).Error("Unable to persist license")

		return nil, err
	}

	s.cacheState(ctx, license, policy, licensedPeriod)

	return &lc.UpdateResponse{
			Updated: true,
			Message: fmt.Sprintf("Policy configured from license ID %s", s.license.Id),
		},
		nil
}

//Telemetry endpoint to return telemetry configuration
func (s *LicenseControlServer) Telemetry(ctx context.Context, req *lc.TelemetryRequest) (*lc.TelemetryResponse, error) {

	return &lc.TelemetryResponse{TelemetryEnabled: s.config.TelemetryEnabled, TelemetryUrl: s.config.URL}, nil
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
