package gateway

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strconv"
	"strings"

	"github.com/grpc-ecosystem/grpc-gateway/runtime"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/metadata"
	"google.golang.org/grpc/reflection"

	// PB-generated imports
	pb_apps "github.com/chef/automate/api/external/applications"
	pb_cfgmgmt "github.com/chef/automate/api/external/cfgmgmt"
	pb_ingest "github.com/chef/automate/api/external/ingest"
	pb_secrets "github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/authn"
	cfgmgmt_request "github.com/chef/automate/api/interservice/cfgmgmt/request"
	swagger "github.com/chef/automate/components/automate-gateway/api"
	pb_teams "github.com/chef/automate/components/automate-gateway/api/auth/teams"
	pb_tokens "github.com/chef/automate/components/automate-gateway/api/auth/tokens"
	pb_users "github.com/chef/automate/components/automate-gateway/api/auth/users"
	pb_authz "github.com/chef/automate/components/automate-gateway/api/authz"
	policy "github.com/chef/automate/components/automate-gateway/api/authz/policy"
	pb_profiles "github.com/chef/automate/components/automate-gateway/api/compliance/profiles"
	pb_cc_reporting "github.com/chef/automate/components/automate-gateway/api/compliance/reporting"
	pb_cc_stats "github.com/chef/automate/components/automate-gateway/api/compliance/reporting/stats"
	pb_cc_jobs "github.com/chef/automate/components/automate-gateway/api/compliance/scanner/jobs"
	pb_deployment "github.com/chef/automate/components/automate-gateway/api/deployment"
	pb_eventfeed "github.com/chef/automate/components/automate-gateway/api/event_feed"
	pb_gateway "github.com/chef/automate/components/automate-gateway/api/gateway"
	pb_iam_v2beta "github.com/chef/automate/components/automate-gateway/api/iam/v2beta"
	pb_legacy "github.com/chef/automate/components/automate-gateway/api/legacy"
	pb_license "github.com/chef/automate/components/automate-gateway/api/license"
	pb_nodes "github.com/chef/automate/components/automate-gateway/api/nodes"
	pb_nodes_manager "github.com/chef/automate/components/automate-gateway/api/nodes/manager"
	pb_notifications "github.com/chef/automate/components/automate-gateway/api/notifications"
	pb_telemetry "github.com/chef/automate/components/automate-gateway/api/telemetry"
	policyv2 "github.com/chef/automate/components/automate-gateway/authz/policy_v2"
	"github.com/chef/automate/components/compliance-service/api/profiles"
	"github.com/chef/automate/components/compliance-service/api/reporting"

	// handlers
	"github.com/chef/automate/components/automate-gateway/handler"
	handler_compliance "github.com/chef/automate/components/automate-gateway/handler/compliance"
	handler_policies "github.com/chef/automate/components/automate-gateway/handler/iam/v2beta/policy"
	handler_rules "github.com/chef/automate/components/automate-gateway/handler/iam/v2beta/rules"
	handler_teams "github.com/chef/automate/components/automate-gateway/handler/iam/v2beta/teams"
	handler_tokens "github.com/chef/automate/components/automate-gateway/handler/iam/v2beta/tokens"
	handler_users "github.com/chef/automate/components/automate-gateway/handler/iam/v2beta/users"

	// anything else
	"github.com/chef/automate/components/automate-gateway/gateway/middleware"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/grpc/service_authn"
)

// RegisterGRPCServices registers all grpc services in the passed *grpc.Server
func (s *Server) RegisterGRPCServices(grpcServer *grpc.Server) error {
	clients := s.clientsFactory

	pb_gateway.RegisterGatewayServer(grpcServer, handler.NewGatewayServer())

	deploymentClient, err := clients.DeploymentServiceClient()
	if err != nil {
		return errors.Wrap(err, "create client for deployment service")
	}
	pb_deployment.RegisterDeploymentServer(grpcServer,
		handler.NewDeploymentServer(deploymentClient))

	licenseClient, err := clients.LicenseControlClient()
	if err != nil {
		log.WithFields(log.Fields{
			"service": "license-control-service",
			"error":   err,
		}).Warn("Could not create license-control-service client")
	}
	pb_telemetry.RegisterTelemetryServer(grpcServer,
		handler.NewTelemetryServer(licenseClient, deploymentClient))

	pb_license.RegisterLicenseServer(grpcServer, handler.NewLicenseServer(
		licenseClient,
		deploymentClient,
		s.trialLicenseURL,
	))

	cfgMgmtClient, err := clients.CfgMgmtClient()
	if err != nil {
		log.WithFields(log.Fields{
			"service": "ConfigMgmt",
			"error":   err,
		}).Fatal("Could not create client")
	}
	pb_cfgmgmt.RegisterConfigMgmtServer(grpcServer,
		handler.NewCfgMgmtServer(cfgMgmtClient))

	notifier, err := clients.Notifier()
	if err != nil {
		log.WithFields(log.Fields{
			"service": "Notifications",
			"error":   err,
		}).Fatal("Could not create client")
	}

	ccrIngester, err := clients.ChefIngesterClient()
	if err != nil {
		return errors.Wrap(err, "create client for ChefIngester service")
	}

	//TODO: We want to try to get rid of this:
	//      See https://github.com/chef/automate/issues/250
	chefIngestServer := handler.NewChefIngestServer(s.automateURL, ccrIngester, notifier)
	pb_ingest.RegisterChefIngesterServer(grpcServer, chefIngestServer)

	ingestStatusClient, err := clients.IngestStatusClient()
	if err != nil {
		return errors.Wrap(err, "create client for IngestStatus service")
	}

	pb_legacy.RegisterLegacyDataCollectorServer(grpcServer,
		handler.NewLegacyIngestServer(ingestStatusClient))

	notificationsClient, err := clients.NotificationsClient()
	if err != nil {
		return errors.Wrap(err, "create client for Notifications service")
	}
	pb_notifications.RegisterNotificationsServer(grpcServer,
		handler.NewNotificationsServer(notificationsClient))

	authzClient, err := clients.AuthorizationClient()
	if err != nil {
		return errors.Wrap(err, "create client for authz service")
	}
	authzV2Client, err := clients.AuthorizationV2Client()
	if err != nil {
		return errors.Wrap(err, "create client for authzV2 service")
	}
	pb_authz.RegisterAuthorizationServer(grpcServer,
		handler.NewAuthzServer(authzClient, s.authorizer))

	policiesClient, err := clients.PoliciesClient()
	if err != nil {
		return errors.Wrap(err, "create policies client for authz-service")
	}
	projectsClient, err := clients.ProjectsClient()
	if err != nil {
		return errors.Wrap(err, "create projects client for authz-service")
	}
	pb_iam_v2beta.RegisterPoliciesServer(grpcServer,
		handler_policies.NewServer(policiesClient, projectsClient, authzV2Client))
	pb_iam_v2beta.RegisterRulesServer(grpcServer, handler_rules.NewServer(projectsClient))

	tokensMgmtClient, err := clients.TokensMgmtClient()
	if err != nil {
		return errors.Wrap(err, "create client for tokens mgmt service")
	}
	pb_tokens.RegisterTokensMgmtServer(grpcServer, handler.NewTokensMgmtServer(tokensMgmtClient))
	// IAM v2 uses the same client
	pb_iam_v2beta.RegisterTokensServer(grpcServer, handler_tokens.NewServer(tokensMgmtClient))

	usersMgmtClient, err := clients.UsersMgmtClient()
	if err != nil {
		return errors.Wrap(err, "create client for users mgmt service")
	}
	pb_users.RegisterUsersMgmtServer(grpcServer, handler.NewUsersMgmtServer(usersMgmtClient))
	// IAM v2 uses the same client
	pb_iam_v2beta.RegisterUsersServer(grpcServer, handler_users.NewServer(usersMgmtClient))

	teamsV1Client, err := clients.TeamsV1Client()
	if err != nil {
		return errors.Wrap(err, "create V1 client for teams service")
	}
	pb_teams.RegisterTeamsServer(grpcServer, handler.NewTeamsServer(teamsV1Client))

	teamsV2Client, err := clients.TeamsV2Client()
	if err != nil {
		return errors.Wrap(err, "create V2 client for teams service")
	}
	pb_iam_v2beta.RegisterTeamsServer(grpcServer, handler_teams.NewServer(teamsV2Client))

	secretsClient, err := clients.SecretClient()
	if err != nil {
		return errors.Wrap(err, "create client for secret service")
	}
	pb_secrets.RegisterSecretsServiceServer(grpcServer, handler.NewSecretsHandler(secretsClient))

	applicationsClient, err := clients.ApplicationsClient()
	if err != nil {
		return errors.Wrap(err, "create client for applications service")
	}
	pb_apps.RegisterApplicationsServiceServer(grpcServer,
		handler.NewApplicationsHandler(applicationsClient))

	jobsClient, err := clients.ComplianceJobsServiceClient()
	if err != nil {
		return errors.Wrap(err, "create client for compliances jobs service")
	}
	pb_cc_jobs.RegisterJobsServiceServer(grpcServer,
		handler_compliance.NewJobsHandler(jobsClient))

	nodesClient, err := clients.NodesClient()
	if err != nil {
		return errors.Wrap(err, "create client for nodes service")
	}
	// give nodes handler access to the jobs client so it can auto-trigger detect jobs when adding nodes
	pb_nodes.RegisterNodesServiceServer(grpcServer,
		handler.NewNodesHandler(nodesClient, jobsClient))

	nodesManagerClient, err := clients.NodeManagerClient()
	if err != nil {
		return errors.Wrap(err, "create client for nodes manager service")
	}
	// give nodemanager handler access to the jobs client
	// so it can auto-trigger detect jobs when adding manager nodes
	pb_nodes_manager.RegisterNodeManagerServiceServer(grpcServer,
		handler.NewNodeManagerHandler(nodesManagerClient, jobsClient))

	profilesClient, err := clients.ComplianceProfilesServiceClient()
	if err != nil {
		return errors.Wrap(err, "create client for compliance profiles service")
	}
	pb_profiles.RegisterProfilesServiceServer(grpcServer,
		handler_compliance.NewProfilesHandler(profilesClient))

	complianceReportingClient, err := clients.ComplianceReportingServiceClient()
	if err != nil {
		return errors.Wrap(err, "create client for compliance reporting service")
	}
	versionClient, err := clients.ComplianceVersionServiceClient()
	if err != nil {
		return errors.Wrap(err, "create client for compliance version service")
	}
	pb_cc_reporting.RegisterReportingServiceServer(grpcServer,
		handler_compliance.NewReportingHandler(complianceReportingClient, versionClient, jobsClient))

	feedClient, err := clients.FeedClient()
	if err != nil {
		return errors.Wrap(err, "create client for feed service")
	}

	pb_eventfeed.RegisterEventFeedServer(grpcServer,
		handler.NewEventFeedServer(cfgMgmtClient, feedClient))

	statsClient, err := clients.ComplianceStatsServiceClient()
	if err != nil {
		return errors.Wrap(err, "create client for compliance stats service")
	}
	pb_cc_stats.RegisterStatsServiceServer(grpcServer,
		handler_compliance.NewStatsHandler(statsClient))

	chefIngesterJobSchedulerClient, err := clients.ChefIngesterJobSchedulerClient()
	if err != nil {
		return errors.Wrap(err, "create client for chef ingest jobs scheduler")
	}

	chefIngestJobSchedulerServer := handler.NewChefIngestJobSchedulerServer(chefIngesterJobSchedulerClient)
	pb_ingest.RegisterJobSchedulerServer(grpcServer, chefIngestJobSchedulerServer)

	// Reflection to be able to make grpcurl calls
	reflection.Register(grpcServer)

	return nil
}

// returns all the openapi/swagger json definitions for each service
func openAPIServicesHandler() http.Handler {
	return http.HandlerFunc(swagger.Swagger.HandleSwaggerSpec)
}

type registerFunc func(context.Context, *runtime.ServeMux, string, []grpc.DialOption) error

// unversionedRESTMux returns all endpoints for the GRPC rest gateway
func unversionedRESTMux(grpcURI string, dopts []grpc.DialOption) (http.Handler, error) {
	return muxFromRegisterMap(grpcURI, dopts, map[string]registerFunc{
		"event feed":           pb_eventfeed.RegisterEventFeedHandlerFromEndpoint,
		"config management":    pb_cfgmgmt.RegisterConfigMgmtHandlerFromEndpoint,
		"chef ingestion":       pb_ingest.RegisterChefIngesterHandlerFromEndpoint,
		"deployment":           pb_deployment.RegisterDeploymentHandlerFromEndpoint,
		"ingest job scheduler": pb_ingest.RegisterJobSchedulerHandlerFromEndpoint,
		"notifications":        pb_notifications.RegisterNotificationsHandlerFromEndpoint,
		"gateway":              pb_gateway.RegisterGatewayHandlerFromEndpoint,
		"legacy":               pb_legacy.RegisterLegacyDataCollectorHandlerFromEndpoint,
		"license":              pb_license.RegisterLicenseHandlerFromEndpoint,
		"auth tokens":          pb_tokens.RegisterTokensMgmtHandlerFromEndpoint,
		"auth users":           pb_users.RegisterUsersMgmtHandlerFromEndpoint,
		"authz":                pb_authz.RegisterAuthorizationHandlerFromEndpoint,
		"secrets":              pb_secrets.RegisterSecretsServiceHandlerFromEndpoint,
		"cc_reporting":         pb_cc_reporting.RegisterReportingServiceHandlerFromEndpoint,
		"cc_stats":             pb_cc_stats.RegisterStatsServiceHandlerFromEndpoint,
		"cc_jobs":              pb_cc_jobs.RegisterJobsServiceHandlerFromEndpoint,
		"nodes":                pb_nodes.RegisterNodesServiceHandlerFromEndpoint,
		"profiles":             pb_profiles.RegisterProfilesServiceHandlerFromEndpoint,
		"teams-service":        pb_teams.RegisterTeamsHandlerFromEndpoint,
		"node manager":         pb_nodes_manager.RegisterNodeManagerServiceHandlerFromEndpoint,
		"telemetry":            pb_telemetry.RegisterTelemetryHandlerFromEndpoint,
	})
}

func versionedRESTMux(grpcURI string, dopts []grpc.DialOption, toggles gwRouteFeatureFlags) (http.Handler, error) {
	endpointMap := map[string]registerFunc{
		"policies v2beta": pb_iam_v2beta.RegisterPoliciesHandlerFromEndpoint,
		"users v2beta":    pb_iam_v2beta.RegisterUsersHandlerFromEndpoint,
		"tokens v2beta":   pb_iam_v2beta.RegisterTokensHandlerFromEndpoint,
		"teams v2beta":    pb_iam_v2beta.RegisterTeamsHandlerFromEndpoint,
		"rules v2beta":    pb_iam_v2beta.RegisterRulesHandlerFromEndpoint,
	}
	if toggles["applications"] {
		endpointMap["apps beta"] = pb_apps.RegisterApplicationsServiceHandlerFromEndpoint
	}
	return muxFromRegisterMap(grpcURI, dopts, endpointMap)
}

func muxFromRegisterMap(grpcURI string, dopts []grpc.DialOption, localEndpoints map[string]registerFunc) (*runtime.ServeMux, error) {
	opts := []runtime.ServeMuxOption{
		runtime.WithIncomingHeaderMatcher(headerMatcher),
		runtime.WithMarshalerOption(runtime.MIMEWildcard, &runtime.JSONPb{OrigName: true, EmitDefaults: true}),
		runtime.WithMetadata(middleware.CertificatePasser),
	}
	gwmux := runtime.NewServeMux(opts...)
	ctx := context.Background()

	// register each endpoint with runtime.ServeMux
	for ep, register := range localEndpoints {
		log.Infof("Register %s to REST Gateway %s", ep, grpcURI)
		if err := register(ctx, gwmux, grpcURI, dopts); err != nil {
			return nil, errors.Wrapf(err, "cannot serve %s api", ep)
		}
	}
	return gwmux, nil
}

type ProfileRequest struct {
	Name    string
	Version string
	Owner   string
}

func (s *Server) ProfileCreateHandler(w http.ResponseWriter, r *http.Request) {
	ctx, cancel := context.WithCancel(r.Context())
	defer cancel()

	var fileData []byte
	var cType, profileName, profileVersion string
	contentTypeString := strings.Split(r.Header.Get("Content-type"), ";")
	switch contentTypeString[0] {
	case "application/json":
		cType = r.Header.Get("Content-type")
		decoder := json.NewDecoder(r.Body)
		var t ProfileRequest
		err := decoder.Decode(&t)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		profileName = t.Name
		profileVersion = t.Version
	case "multipart/form-data":
		var content bytes.Buffer
		file, _, err := r.FormFile("file")
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		defer file.Close()

		_, err = io.Copy(&content, file)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}

		fileData = content.Bytes()
		cType = r.URL.Query().Get("contentType")
	default: // no match
		http.Error(w, "invalid content-type header", http.StatusBadRequest)
		return
	}
	owner := r.URL.Query().Get("owner")

	const (
		actionV1 = "upload"
		actionV2 = "compliance:profiles:create"
	)
	resourceV1 := fmt.Sprintf("compliance:profiles:storage:%s", owner)
	resourceV2 := fmt.Sprintf("compliance:profiles:%s", owner)
	ctx, err := s.authRequest(r, resourceV1, actionV1, resourceV2, actionV2)
	if err != nil {
		http.Error(w, err.Error(), http.StatusForbidden)
		return
	}

	profilesClient, err := s.clientsFactory.ComplianceProfilesServiceClient()
	if err != nil {
		http.Error(w, "grpc service for compliance unavailable", http.StatusServiceUnavailable)
		return
	}

	stream, err := profilesClient.Create(ctx)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	request := profiles.ProfilePostRequest{
		Owner: owner,
		Chunk: &profiles.Chunk{Data: fileData},
		Meta: &profiles.Metadata{
			ContentType: cType,
			Name:        profileName,
			Version:     profileVersion,
		},
	}
	err = stream.Send(&request)
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	reply, err := stream.CloseAndRecv()
	if err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}
	data, err := json.Marshal(reply)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	w.Write(data)
}

func (s *Server) ProfileTarHandler(w http.ResponseWriter, r *http.Request) {
	ctx, cancel := context.WithCancel(r.Context())
	defer cancel()

	var profileOwner, profileName, profileVersion string

	url := r.URL.Path
	splitUrl := strings.Split(url, "/")
	if len(splitUrl) > 4 {
		profileOwner = splitUrl[3]
		profileName = splitUrl[4]
	}
	if len(splitUrl) > 6 {
		if splitUrl[5] == "version" {
			profileVersion = splitUrl[6]
		}
	}

	if len(profileOwner) == 0 || len(profileName) == 0 {
		decoder := json.NewDecoder(r.Body)
		var t ProfileRequest
		err := decoder.Decode(&t)
		if err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		profileName = t.Name
		profileVersion = t.Version
		profileOwner = t.Owner
	}
	log.Infof("processing profile tar request for owner: %s, name: %s %s", profileOwner, profileName, profileVersion)

	var resourceV1, actionV1, resourceV2, actionV2 string
	actionV1 = "read"
	if len(profileOwner) > 0 {
		resourceV1 = fmt.Sprintf("compliance:profiles:storage:%s", profileOwner)
		actionV2 = "compliance:profiles:get"
		resourceV2 = fmt.Sprintf("compliance:profiles:%s", profileOwner)
	} else {
		resourceV1 = "compliance:profiles:market"
		actionV2 = "compliance:marketProfiles:get"
		resourceV2 = "compliance:profiles:market"
	}

	ctx, err := s.authRequest(r, resourceV1, actionV1, resourceV2, actionV2)
	if err != nil {
		http.Error(w, err.Error(), http.StatusForbidden)
		return
	}

	profilesClient, err := s.clientsFactory.ComplianceProfilesServiceClient()
	if err != nil {
		http.Error(w, "grpc service for compliance unavailable", http.StatusServiceUnavailable)
		return
	}

	stream, err := profilesClient.ReadTar(ctx, &profiles.ProfileDetails{Name: profileName, Version: profileVersion, Owner: profileOwner})
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	for {
		data, err := stream.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			http.Error(w, err.Error(), http.StatusNotFound)
			return
		}
		contentLength := strconv.Itoa(len(data.GetData()))
		w.Header().Set("Content-Length", contentLength)
		w.Header().Set("Content-Type", "application/x-gzip")
		w.Header().Set("Accept-Ranges", "bytes")
		w.Write(data.GetData())
	}
}

func (s *Server) ReportExportHandler(w http.ResponseWriter, r *http.Request) {
	ctx, cancel := context.WithCancel(r.Context())
	defer cancel()

	// Node: from glancing at the code, I don't believe this is using query.Id, so
	// we can't be more specific. Also, we can do this before looking at the
	// request body.
	const (
		resource = "compliance:reporting:reports" // v1 and v2? OK...
		actionV1 = "export"
		actionV2 = "compliance:reports:export"
	)

	ctx, err := s.authRequest(r, resource, actionV1, resource, actionV2)
	if err != nil {
		http.Error(w, err.Error(), http.StatusForbidden)
		return
	}

	decoder := json.NewDecoder(r.Body)
	var query reporting.Query
	if err := decoder.Decode(&query); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	reportingClient, err := s.clientsFactory.ComplianceReportingServiceClient()
	if err != nil {
		http.Error(w, "grpc service for compliance unavailable", http.StatusServiceUnavailable)
		return
	}

	stream, err := reportingClient.Export(ctx, &query)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	for {
		data, err := stream.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			http.Error(w, err.Error(), http.StatusNotFound)
			return
		}
		w.Write(data.GetContent())
	}
}

func (s *Server) configMgmtNodeExportHandler(w http.ResponseWriter, r *http.Request) {
	ctx, cancel := context.WithCancel(r.Context())
	defer cancel()

	const (
		actionV1   = "read"
		resourceV1 = "cfgmgmt:nodes"
		actionV2   = "infra:nodes:list"
		resourceV2 = "infra:nodes"
	)

	ctx, err := s.authRequest(r, resourceV1, actionV1, resourceV2, actionV2)
	if err != nil {
		http.Error(w, err.Error(), http.StatusForbidden)
		return
	}

	decoder := json.NewDecoder(r.Body)
	var nodeExportRequest cfgmgmt_request.NodeExport
	if err := decoder.Decode(&nodeExportRequest); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	cfgMgmtClient, err := s.clientsFactory.CfgMgmtClient()
	if err != nil {
		http.Error(w, "grpc service for config mgmt unavailable", http.StatusServiceUnavailable)
		return
	}

	stream, err := cfgMgmtClient.NodeExport(ctx, &nodeExportRequest)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	for {
		data, err := stream.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			http.Error(w, err.Error(), http.StatusNotFound)
			return
		}
		w.Write(data.GetContent())
	}
}

func init() {
	// Register streaming methods for introspection.
	// - Almost all calls of this method are in *.pb.go files, auto-generated from proto files.
	//   However, streaming endpoints do not have this metadata in the proto files so doing it manually here.
	// - These values originate from the ProfileCreateHandler method elsewhere in this file.
	// - There is nothing special about the "streaming::" notation; it just needs to be
	//   the same notation used in an introspection path in the front-end.
	policy.MapMethodTo("UNUSED_METHOD", "compliance:profiles:storage", "upload", "POST", "streaming::/compliance/profiles",
		func(unexpandedResource string, input interface{}) string { return unexpandedResource })
	policyv2.MapMethodTo("UNUSED_METHOD", "compliance:profiles", "compliance:profiles:create", "POST", "streaming::/compliance/profiles",
		func(unexpandedResource string, input interface{}) string { return unexpandedResource })
}

func (s *Server) authRequest(r *http.Request,
	resourceV1, actionV1, resourceV2, actionV2 string,
) (context.Context, error) {
	subjects := []string{}
	// Create a context with the request headers metadata. Normally grpc-gateway
	// does this, but since this is being used in a custom handler we've got do
	// it ourselves.
	md := metadataFromRequest(r)
	ctx := metadata.NewOutgoingContext(r.Context(), md)

	// Handle certificate based authn:
	//
	// If the request has a verified TLS certificate and the request did NOT
	// originate from the automate load balancer then generate the policy
	// subject from the certificate's 'Common Name' and skip token based authn.
	//
	// This intentionally skips all requests that originate from the automate load
	// balancer. If we wish to extend certificate based authn to custom handlers
	// from the automate load balancer we'll need to decode the x-client-cert from
	// the request metadata to build the policy subject from it.
	if tls := r.TLS; tls != nil {
		if len(tls.VerifiedChains) > 0 && len(tls.VerifiedChains[0]) > 0 {
			sub, ok := service_authn.ServiceSubjectFromCert(tls.VerifiedChains[0][0])
			if ok {
				if !strings.HasPrefix(sub, "tls:service:automate-load-balancer:") {
					subjects = append(subjects, sub)
				}
			}
		}
	}

	if len(subjects) < 1 {
		authnClient, err := s.clientsFactory.AuthenticationClient()
		if err != nil {
			return nil, errors.Wrap(err, "authn-service unavailable")
		}

		authnResp, err := authnClient.Authenticate(ctx, &authn.AuthenticateRequest{})
		if err != nil {
			return nil, errors.Wrap(err, "authn-service error")
		}

		subjects = append(authnResp.Teams, authnResp.Subject)
	}

	if len(subjects) < 1 {
		return nil, errors.New("no policy subject detected in headers or verified certificates")
	}

	projects := auth_context.ProjectsFromMetadata(md)

	authzResp, err := s.authorizer.IsAuthorized(ctx, subjects, resourceV1, actionV1, resourceV2, actionV2, projects)
	if err != nil {
		return nil, errors.Wrap(err, "authz-service error")
	}

	// err is nil if authorization succeeded
	// Note: if we need all the auth info, use auth_context.NewOutgoingContext
	return auth_context.NewOutgoingProjectsContext(authzResp.Ctx()), authzResp.Err()
}
