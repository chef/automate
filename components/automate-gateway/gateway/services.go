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

	grpc_prometheus "github.com/grpc-ecosystem/go-grpc-prometheus"
	"github.com/grpc-ecosystem/grpc-gateway/runtime"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/metadata"
	"google.golang.org/grpc/reflection"

	// PB-generated imports
	pb_apps "github.com/chef/automate/api/external/applications"
	pb_cfgmgmt "github.com/chef/automate/api/external/cfgmgmt"
	pb_data_feed "github.com/chef/automate/api/external/data_feed"
	pb_data_lifecycle "github.com/chef/automate/api/external/data_lifecycle"
	pb_infra_proxy "github.com/chef/automate/api/external/infra_proxy"
	pb_ingest "github.com/chef/automate/api/external/ingest"
	pb_nodes "github.com/chef/automate/api/external/nodes"
	pb_nodes_manager "github.com/chef/automate/api/external/nodes/manager"
	pb_secrets "github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/authn"
	cfgmgmt_request "github.com/chef/automate/api/interservice/cfgmgmt/request"
	deploy_api "github.com/chef/automate/api/interservice/deployment"
	swagger "github.com/chef/automate/components/automate-gateway/api"
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
	pb_iam_v2 "github.com/chef/automate/components/automate-gateway/api/iam/v2"
	pb_legacy "github.com/chef/automate/components/automate-gateway/api/legacy"
	pb_license "github.com/chef/automate/components/automate-gateway/api/license"
	pb_notifications "github.com/chef/automate/components/automate-gateway/api/notifications"
	pb_telemetry "github.com/chef/automate/components/automate-gateway/api/telemetry"
	policyv2 "github.com/chef/automate/components/automate-gateway/authz/policy_v2"
	"github.com/chef/automate/components/compliance-service/api/profiles"
	"github.com/chef/automate/components/compliance-service/api/reporting"

	// handlers
	"github.com/chef/automate/components/automate-gateway/handler"
	handler_compliance "github.com/chef/automate/components/automate-gateway/handler/compliance"
	handler_data_lifecycle "github.com/chef/automate/components/automate-gateway/handler/data_lifecycle"
	handler_policies "github.com/chef/automate/components/automate-gateway/handler/iam/v2/policy"
	handler_rules "github.com/chef/automate/components/automate-gateway/handler/iam/v2/rules"
	handler_teams "github.com/chef/automate/components/automate-gateway/handler/iam/v2/teams"
	handler_tokens "github.com/chef/automate/components/automate-gateway/handler/iam/v2/tokens"
	handler_users "github.com/chef/automate/components/automate-gateway/handler/iam/v2/users"
	handler_infra_proxy "github.com/chef/automate/components/automate-gateway/handler/infra_proxy"

	// anything else
	"github.com/chef/automate/components/automate-gateway/gateway/middleware"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/grpc/service_authn"
)

type ServiceStatus struct {
	Service string `json:"service"`
	Status  string `json:"status"`
}

type DeploymentStatus struct {
	Ok              bool            `json:"ok"`
	ServiceStatuses []ServiceStatus `json:"service_status"`
}

// RegisterGRPCServices registers all grpc services in the passed *grpc.Server
// nolint: gocyclo
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

	trialLicenseURL, err := s.Config.trialLicenseURL()
	if err != nil {
		return err
	}

	pb_license.RegisterLicenseServer(grpcServer, handler.NewLicenseServer(
		licenseClient,
		deploymentClient,
		trialLicenseURL,
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

	eventFeedClient, err := clients.FeedClient()
	if err != nil {
		log.WithFields(log.Fields{
			"service": "EventFeed",
			"error":   err,
		}).Fatal("Could not create client")
	}

	pb_eventfeed.RegisterEventFeedServer(grpcServer,
		handler.NewEventFeedServer(cfgMgmtClient, eventFeedClient))

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
	automateURL, err := s.Config.automateURL()
	if err != nil {
		return errors.Wrap(err, "create ingester service")
	}
	chefIngestServer := handler.NewChefIngestServer(automateURL, ccrIngester, notifier)
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
	pb_iam_v2.RegisterPoliciesServer(grpcServer,
		handler_policies.NewServer(policiesClient, projectsClient, authzV2Client))
	pb_iam_v2.RegisterRulesServer(grpcServer, handler_rules.NewServer(projectsClient))

	tokensMgmtClient, err := clients.TokensMgmtClient()
	if err != nil {
		return errors.Wrap(err, "create client for tokens mgmt service")
	}
	pb_tokens.RegisterTokensMgmtServer(grpcServer, handler.NewTokensMgmtServer(tokensMgmtClient))
	// IAM v2 uses the same client
	pb_iam_v2.RegisterTokensServer(grpcServer, handler_tokens.NewServer(tokensMgmtClient))

	usersMgmtClient, err := clients.UsersMgmtClient()
	if err != nil {
		return errors.Wrap(err, "create client for users mgmt service")
	}
	pb_users.RegisterUsersMgmtServer(grpcServer, handler.NewUsersMgmtServer(usersMgmtClient))
	// IAM v2 uses the same client
	pb_iam_v2.RegisterUsersServer(grpcServer, handler_users.NewServer(usersMgmtClient))

	teamsV2Client, err := clients.TeamsClient()
	if err != nil {
		return errors.Wrap(err, "create V2 client for teams service")
	}
	pb_iam_v2.RegisterTeamsServer(grpcServer, handler_teams.NewServer(teamsV2Client))

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

	ingestPurgeClient, err := clients.PurgeClient("ingest-service")
	if err != nil {
		return errors.Wrap(err, "create purge client for ingest-service")
	}

	compliancePurgeClient, err := clients.PurgeClient("compliance-service")
	if err != nil {
		return errors.Wrap(err, "create purge client for compliance-service")
	}

	eventFeedPurgeClient, err := clients.PurgeClient("event-feed-service")
	if err != nil {
		return errors.Wrap(err, "create purge client for event-feed-service")
	}

	dataLifecycleServer := handler_data_lifecycle.NewServer(
		chefIngesterJobSchedulerClient,
		ingestPurgeClient,
		compliancePurgeClient,
		eventFeedPurgeClient,
	)
	pb_data_lifecycle.RegisterDataLifecycleServer(grpcServer, dataLifecycleServer)

	infraProxyClient, err := clients.InfraProxyClient()
	if err != nil {
		return errors.Wrap(err, "create client for infra proxy service")
	}
	pb_infra_proxy.RegisterInfraProxyServer(grpcServer, handler_infra_proxy.NewInfraProxyHandler(infraProxyClient))

	// Reflection to be able to make grpcurl calls
	reflection.Register(grpcServer)

	datafeedClient, err := clients.DatafeedClient()
	if err != nil {
		return errors.Wrap(err, "create client for secret service")
	}
	pb_data_feed.RegisterDatafeedServiceServer(grpcServer, handler.NewDatafeedHandler(datafeedClient))

	grpc_prometheus.Register(grpcServer)

	return nil
}

// returns all the openapi/swagger json definitions for each service
func openAPIServicesHandler() http.Handler {
	return http.HandlerFunc(swagger.Swagger.HandleSwaggerSpec)
}

type registerFunc func(context.Context, *runtime.ServeMux, string, []grpc.DialOption) error

// unversionedRESTMux returns all endpoints for the GRPC rest gateway
func unversionedRESTMux(grpcURI string, dopts []grpc.DialOption) (http.Handler, func(), error) {
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
		"teams-service":        pb_iam_v2.RegisterTeamsHandlerFromEndpoint,
		"node manager":         pb_nodes_manager.RegisterNodeManagerServiceHandlerFromEndpoint,
		"telemetry":            pb_telemetry.RegisterTelemetryHandlerFromEndpoint,
		"data-feed":            pb_data_feed.RegisterDatafeedServiceHandlerFromEndpoint,
		"data-lifecycle":       pb_data_lifecycle.RegisterDataLifecycleHandlerFromEndpoint,
		"applications":         pb_apps.RegisterApplicationsServiceHandlerFromEndpoint,
		"infra-proxy":          pb_infra_proxy.RegisterInfraProxyHandlerFromEndpoint,
	})
}

func versionedRESTMux(grpcURI string, dopts []grpc.DialOption, toggles gwRouteFeatureFlags) (http.Handler, func(), error) {
	endpointMap := map[string]registerFunc{
		"policies v2": pb_iam_v2.RegisterPoliciesHandlerFromEndpoint,
		"users v2":    pb_iam_v2.RegisterUsersHandlerFromEndpoint,
		"tokens v2":   pb_iam_v2.RegisterTokensHandlerFromEndpoint,
		"teams v2":    pb_iam_v2.RegisterTeamsHandlerFromEndpoint,
		"rules v2":    pb_iam_v2.RegisterRulesHandlerFromEndpoint,
	}
	return muxFromRegisterMap(grpcURI, dopts, endpointMap)
}

func muxFromRegisterMap(grpcURI string, dopts []grpc.DialOption, localEndpoints map[string]registerFunc) (*runtime.ServeMux, func(), error) {
	opts := []runtime.ServeMuxOption{
		runtime.WithIncomingHeaderMatcher(headerMatcher),
		runtime.WithMarshalerOption("application/json+pretty", &runtime.JSONPb{OrigName: true, EmitDefaults: true, Indent: "  "}),
		runtime.WithMarshalerOption(runtime.MIMEWildcard, &runtime.JSONPb{OrigName: true, EmitDefaults: true}),
		runtime.WithMetadata(middleware.CertificatePasser),
	}
	gwmux := runtime.NewServeMux(opts...)
	ctx, cancel := context.WithCancel(context.Background())

	// register each endpoint with runtime.ServeMux
	for ep, register := range localEndpoints {
		log.Infof("Register %s to REST Gateway %s", ep, grpcURI)
		if err := register(ctx, gwmux, grpcURI, dopts); err != nil {
			return nil, cancel, errors.Wrapf(err, "cannot serve %s api", ep)
		}
	}
	return gwmux, cancel, nil
}

type ProfileRequest struct {
	Name    string
	Version string
	Owner   string
}

func (s *Server) ProfileCreateHandler(w http.ResponseWriter, r *http.Request) {
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
		defer file.Close() // nolint: errcheck

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
		action = "compliance:profiles:create"
	)
	resource := fmt.Sprintf("compliance:profiles:%s", owner)
	ctx, err := s.authRequest(r, resource, action)
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
	w.Write(data) // nolint: errcheck
}

func (s *Server) ProfileTarHandler(w http.ResponseWriter, r *http.Request) {
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

	var resource, action string
	if len(profileOwner) > 0 {
		action = "compliance:profiles:get"
		resource = fmt.Sprintf("compliance:profiles:%s", profileOwner)
	} else {
		action = "compliance:marketProfiles:get"
		resource = "compliance:profiles:market"
	}

	ctx, err := s.authRequest(r, resource, action)
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
		w.Write(data.GetData()) // nolint: errcheck
	}
}

func (s *Server) ReportExportHandler(w http.ResponseWriter, r *http.Request) {
	// Node: from glancing at the code, I don't believe this is using query.Id, so
	// we can't be more specific. Also, we can do this before looking at the
	// request body.
	const (
		resource = "compliance:reporting:reports"
		action   = "compliance:reports:export"
	)

	ctx, err := s.authRequest(r, resource, action)
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
	writeContent(w, stream, query.Type)
}

func writeContent(w http.ResponseWriter, stream reporting.ReportingService_ExportClient, queryType string) {
	//when the type is json, we will be retrieving a json array of objects and since we will be getting them one at a
	// time, we need to provide the '[' to open and the ']' to close (the close will happen on EOF, below)
	if queryType == "json" {
		_, err := w.Write([]byte("["))
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	}
	for {
		data, err := stream.Recv()
		if err == io.EOF {
			if queryType == "json" {
				_, err = w.Write([]byte("]"))
				if err != nil {
					http.Error(w, err.Error(), http.StatusInternalServerError)
					return
				}
			}
			break
		}
		if err != nil {
			http.Error(w, err.Error(), http.StatusNotFound)
			return
		}
		w.Write(data.GetContent()) // nolint: errcheck
	}
}

func (s *Server) NodeExportHandler(w http.ResponseWriter, r *http.Request) {
	const (
		resource = "compliance:reporting:nodes:{id}"
		action   = "compliance:reportNodes:export"
	)

	ctx, err := s.authRequest(r, resource, action)
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

	stream, err := reportingClient.ExportNode(ctx, &query)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	writeContent(w, stream, query.Type)
}

func (s *Server) configMgmtNodeExportHandler(w http.ResponseWriter, r *http.Request) {
	const (
		action   = "infra:nodes:list"
		resource = "infra:nodes"
	)

	ctx, err := s.authRequest(r, resource, action)
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
		w.Write(data.GetContent()) // nolint: errcheck
	}
}

func (s *Server) configMgmtReportExportHandler(w http.ResponseWriter, r *http.Request) {
	const (
		action   = "infra:nodes:list"
		resource = "infra:nodes"
	)

	ctx, err := s.authRequest(r, resource, action)
	if err != nil {
		http.Error(w, err.Error(), http.StatusForbidden)
		return
	}

	decoder := json.NewDecoder(r.Body)
	var reportExportRequest cfgmgmt_request.ReportExport
	if err := decoder.Decode(&reportExportRequest); err != nil {
		http.Error(w, err.Error(), http.StatusBadRequest)
		return
	}

	cfgMgmtClient, err := s.clientsFactory.CfgMgmtClient()
	if err != nil {
		http.Error(w, "grpc service for config mgmt unavailable", http.StatusServiceUnavailable)
		return
	}

	stream, err := cfgMgmtClient.ReportExport(ctx, &reportExportRequest)
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
		w.Write(data.GetContent()) // nolint: errcheck
	}
}

func (s *Server) DeploymentStatusHandler(w http.ResponseWriter, r *http.Request) {
	const (
		action   = "system:status:get"
		resource = "system:service:status"
	)

	ctx, err := s.authRequest(r, resource, action)
	if err != nil {
		http.Error(w, err.Error(), http.StatusForbidden)
		return
	}

	deploymentClient, err := s.clientsFactory.DeploymentServiceClient()
	if err != nil {
		http.Error(w, "grpc service for deployment service unavailable", http.StatusServiceUnavailable)
		return
	}

	status, err := deploymentClient.Status(ctx, &deploy_api.StatusRequest{})
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	serviceStatus := make([]ServiceStatus, len(status.ServiceStatus.Services))

	var overallOks bool = true
	for index, svc := range status.ServiceStatus.Services {
		overallOks = overallOks && (svc.State == deploy_api.ServiceState_OK)

		serviceStatus[index] = ServiceStatus{
			Service: svc.Name,
			Status:  svc.State.String(),
		}
	}

	result := DeploymentStatus{
		Ok:              overallOks,
		ServiceStatuses: serviceStatus,
	}

	// Because this is a monitoring endpoint, we want to return 500 if services are down.
	// This is consistent with Chef Infra Server's behavior and a nicer experience for users
	// running monitoring software with built-in support for http checks. (e.g. nagios)
	if overallOks {
		w.WriteHeader(http.StatusOK)
	} else {
		w.WriteHeader(http.StatusInternalServerError)
	}
	w.Header().Set("Content-Type", "application/json")

	// Provide prettyprinted output.
	var jsonBody []byte
	if _, ok := r.URL.Query()["pretty"]; ok {
		jsonBody, err = json.MarshalIndent(&result, "", "  ")
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	} else {
		jsonBody, err = json.Marshal(&result)
		if err != nil {
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}
	}

	_, err = w.Write(jsonBody)
	if err != nil {
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}
	return
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

func (s *Server) authRequest(r *http.Request, resource, action string) (context.Context, error) {
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

	authzResp, err := s.authorizer.IsAuthorized(ctx, subjects, resource, action, projects)
	if err != nil {
		return nil, errors.Wrap(err, "authz-service error")
	}

	// err is nil if authorization succeeded
	// Note: if we need all the auth info, use auth_context.NewOutgoingContext
	return auth_context.NewOutgoingProjectsContext(authzResp.Ctx()), authzResp.Err()
}
