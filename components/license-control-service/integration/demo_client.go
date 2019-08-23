package main

import (
	"context"
	"flag"
	"fmt"
	"strings"
	"time"

	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/metadata"
	"google.golang.org/grpc/status"

	lc "github.com/chef/automate/api/interservice/license_control"
	"github.com/chef/automate/components/license-control-service/pkg/server"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/license"
	"github.com/chef/automate/lib/tls/certs"
)

// After handling the policy response this will hold our feature configuration
var splinesFeatures map[string]feature

// Configured features are described with this type
type feature struct {
	name     string
	measure  string
	limit    int64
	required bool
}

// Defined business rules for handling policy-derived behavior
var policyRules map[string]string

// Our gRPC server config
var config server.Config
var licenseToken string

func init() {
	config = server.Config{
		TLSConfig: certs.TLSConfig{},
	}

	flag.StringVar(
		&config.Host,
		"license_control_service_host",
		"0.0.0.0",
		"gRPC server hostname",
	)
	flag.IntVar(
		&config.Port,
		"license_control_service_port",
		10124,
		"gRPC server port",
	)
	flag.StringVar(
		&config.TLSConfig.CertPath,
		"cert",
		"",
		"gRPC server TLS certificate path",
	)
	flag.StringVar(
		&config.TLSConfig.KeyPath,
		"key",
		"",
		"gRPC server TLS key path",
	)
	flag.StringVar(
		&config.TLSConfig.RootCACertPath,
		"root_cert",
		"",
		"gRPC server TLS root certificate path",
	)
	flag.StringVar(
		&licenseToken,
		"update_license",
		"",
		"License to update license-control-service with",
	)
	flag.Parse()

	serviceCerts, err := config.TLSConfig.ReadCerts()
	if err != nil {
		log.WithFields(log.Fields{
			"err":       err.Error(),
			"cert":      &config.TLSConfig.CertPath,
			"key":       &config.TLSConfig.KeyPath,
			"root-cert": &config.TLSConfig.RootCACertPath,
		}).Fatal("failed loading keys/certs")
	}
	config.ServiceCerts = serviceCerts
}

func main() {
	// Setup our gRPC connection factory
	connFactory := secureconn.NewFactory(*config.ServiceCerts)

	conn, err := connFactory.Dial("license-control-service", config.ListenAddress())

	if err != nil {
		log.WithFields(
			log.Fields{
				"license_control_service_address": config.ListenAddress(),
				"error":                           err,
			},
		).Fatal("Unable to connect to license-control-service")
	}
	defer conn.Close() // nolint: errcheck

	fmt.Printf("\n\t\t\t===== Before loading a new license ======\n\n")

	fetchStatus(conn)

	fetchPolicy(conn)

	fetchLicense(conn)

	fmt.Printf("\n\t\t\t========== Pushing our license ==========\n\n")

	putLicense(conn, licenseToken)

	fmt.Printf("\n\t\t\t===== After loading the new license =====\n\n")

	fetchStatus(conn)

	fetchPolicy(conn)

	fetchLicense(conn)
}

func fetchPolicy(conn *grpc.ClientConn) {
	policy := requestPolicy(lc.NewLicenseControlClient(conn))

	if policy == nil {
		log.Warn("Received empty policy response")
		return
	}

	handleCapabilities(policy.Capabilities)

	handleRules(policy)

	// TODO: take rules into account
	if !policy.Valid {
		log.Fatal("Configured license is invalid")
	}
}

func requestPolicy(client lc.LicenseControlClient) *lc.Policy {
	md := metadata.Pairs("user-agent", "splines")

	ctx := metadata.NewOutgoingContext(context.Background(), md)

	response, err := client.Policy(ctx, &lc.PolicyRequest{})
	if err != nil {
		log.WithFields(log.Fields{"error": err}).Fatal("Request failed")
	}

	return response.Policy
}

func handleCapabilities(caps []*lc.Capability) {
	// Required and optional capabilities for the Splines application
	splinesFeatureSet := map[string]bool{
		"ReticulateSplines": true,  // core capability, non-optional
		"EasterEgg":         false, // can run with or without this
	}

	// Maps have to be initialized before use, and we can't do that at the global
	// scope, but we need to declare it in global so it's accessible everywhere.
	// (So yes it's weird, but that's Go for you.)
	splinesFeatures = make(map[string]feature)

	// Review each of the capabilities provided by the policy, if we recognize
	// it, register it in splinesFeatures. If it's unrecognized, ignore it.
	for _, cap := range caps {
		required, ok := splinesFeatureSet[cap.Name]

		logFields := log.WithFields(
			log.Fields{
				"feature":  cap.Name,
				"measure":  cap.Measure,
				"limit":    cap.Limit,
				"required": required,
			},
		)

		if ok {
			splinesFeatures[cap.Name] = feature{
				name:     cap.Name,
				measure:  cap.Measure,
				limit:    cap.Limit,
				required: required,
			}

			logFields.Info("Feature capability detected")
		} else {
			logFields.Warn("Feature capability unrecognized")
		}
	}
}

func handleRules(policy *lc.Policy) {
	policyRules = make(map[string]string)

	prefix := "splines_"

	for rule, setting := range policy.Rules {
		if strings.HasPrefix(rule, prefix) {
			policyRules[strings.TrimPrefix(rule, prefix)] = setting
		}
	}
}

func fetchStatus(conn *grpc.ClientConn) {
	response := requestStatus(lc.NewLicenseControlClient(conn))

	var configuredAt string
	if response.ConfiguredAt != nil {
		configuredAt = time.Unix(response.ConfiguredAt.Seconds, 0).UTC().Format(time.RFC3339)
	}

	var licensedPeriodStart string
	var licensedPeriodEnd string
	if response.LicensedPeriod != nil {
		if response.LicensedPeriod.Start != nil {
			licensedPeriodStart = time.Unix(response.LicensedPeriod.Start.Seconds, 0).UTC().Format(time.RFC3339)
		}

		if response.LicensedPeriod.End != nil {
			licensedPeriodEnd = time.Unix(response.LicensedPeriod.End.Seconds, 0).UTC().Format(time.RFC3339)
		}
	}

	log.WithFields(
		log.Fields{
			"license_id":            response.LicenseId,
			"configured_at":         configuredAt,
			"licensed_period_start": licensedPeriodStart,
			"licensed_period_end":   licensedPeriodEnd,
			"customer_name":         response.CustomerName,
		},
	).Info("Service status")
}

func requestStatus(client lc.LicenseControlClient) *lc.StatusResponse {
	md := metadata.Pairs("user-agent", "splines")

	ctx := metadata.NewOutgoingContext(context.Background(), md)

	response, err := client.Status(ctx, &lc.StatusRequest{})
	if err != nil {
		log.WithFields(log.Fields{"error": err}).Fatal("Request failed")
	}

	return response
}

func fetchLicense(conn *grpc.ClientConn) {
	license := requestLicense(lc.NewLicenseControlClient(conn))

	if license == nil {
		log.Warn("No license loaded")
		return
	}

	generationDate := time.Unix(license.GenerationDate.Seconds, 0).UTC().Format(time.RFC3339)

	log.WithFields(
		log.Fields{
			"license_id":      license.Id,
			"generation_date": generationDate,
			"customer":        license.Customer,
			"customer_id":     license.CustomerId,
		},
	).Info("Service status")
}

func requestLicense(client lc.LicenseControlClient) *license.License {
	md := metadata.Pairs("user-agent", "splines")

	ctx := metadata.NewOutgoingContext(context.Background(), md)

	response, err := client.License(ctx, &lc.LicenseRequest{})
	if err != nil {
		log.WithFields(log.Fields{"error": err}).Fatal("Request failed")
	}

	return response.License
}

func putLicense(conn *grpc.ClientConn, license string) (ok bool) {
	if license == "" {
		log.Warn("No license given; use --update_license to define")
		return
	}

	response := requestUpdate(lc.NewLicenseControlClient(conn), license)

	log.WithFields(
		log.Fields{
			"updated": response.Updated,
			"message": response.Message,
		},
	).Info("Pushed new license")

	return true
}

func requestUpdate(client lc.LicenseControlClient, license string) *lc.UpdateResponse {
	md := metadata.Pairs("user-agent", "splines")

	ctx := metadata.NewOutgoingContext(context.Background(), md)

	response, err := client.Update(ctx, &lc.UpdateRequest{LicenseData: license})

	if err != nil {
		c := status.Code(err)
		log.WithFields(
			log.Fields{
				"error_code": c.String(),
				"error":      err.Error(),
			}).Fatal("Request failed")
	}

	return response
}
