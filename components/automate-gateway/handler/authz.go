package handler

import (
	"context"
	"strings"

	"github.com/grpc-ecosystem/go-grpc-middleware/logging/logrus/ctxlogrus"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	// Shared Response/Request definitions
	version "github.com/chef/automate/api/external/common/version"

	// Authz Service Requests/Response/Service definitions
	authz "github.com/chef/automate/api/interservice/authz"
	authzV2 "github.com/chef/automate/api/interservice/authz/v2"

	// Gateway Requests/Response/Service definitions
	"github.com/chef/automate/components/automate-gateway/api/authz/pairs"
	"github.com/chef/automate/components/automate-gateway/api/authz/policy"
	gwAuthzReq "github.com/chef/automate/components/automate-gateway/api/authz/request"
	gwAuthzRes "github.com/chef/automate/components/automate-gateway/api/authz/response"

	// other dependencies
	"github.com/chef/automate/components/automate-gateway/authz/policy_v2"
	"github.com/chef/automate/components/automate-gateway/gateway/middleware"
	"github.com/chef/automate/lib/grpc/auth_context"
)

// AuthzServer is the server interface
type AuthzServer struct {
	client        authz.AuthorizationClient
	clientV2      authzV2.AuthorizationClient
	filterHandler middleware.SwitchingFilterHandler
}

// NewAuthzServer creates a server with its client
func NewAuthzServer(
	client authz.AuthorizationClient,
	clientV2 authzV2.AuthorizationClient,
	filterHandler middleware.SwitchingFilterHandler,
) *AuthzServer {
	return &AuthzServer{
		client:        client,
		clientV2:      clientV2,
		filterHandler: filterHandler,
	}
}

// GetVersion returns service version
func (a *AuthzServer) GetVersion(ctx context.Context, _ *version.VersionInfoRequest) (*version.VersionInfo, error) {
	req := &version.VersionInfoRequest{}
	res, err := a.client.GetVersion(ctx, req)
	if err != nil {
		return nil, err
	}

	return res, nil
}

// CreatePolicy creates a new policy in authz-service.
func (a *AuthzServer) CreatePolicy(ctx context.Context,
	gwReq *gwAuthzReq.CreatePolicyReq) (*gwAuthzRes.CreatePolicyResp, error) {
	// we want a user's permissions to be a union of their allowed policies
	// so only "allow" policies can be created
	domainReq := &authz.CreatePolicyReq{
		Subjects: gwReq.GetSubjects(),
		Action:   gwReq.GetAction(),
		Resource: gwReq.GetResource(),
	}
	domainRes, err := a.client.CreatePolicy(ctx, domainReq)
	if err != nil {
		return nil, err
	}

	// field names are the same, so we can cast this
	return (*gwAuthzRes.CreatePolicyResp)(domainPolicyToGatewayPolicy(domainRes.Policy)), nil
}

// ListPolicies returns an array of all policy objects
// that currently exist in authz-service.
func (a *AuthzServer) ListPolicies(ctx context.Context,
	gwReq *gwAuthzReq.ListPoliciesReq) (*gwAuthzRes.ListPoliciesResp, error) {
	domainReq := &authz.ListPoliciesReq{}
	domainRes, err := a.client.ListPolicies(ctx, domainReq)
	if err != nil {
		return nil, err
	}

	gwRes := gwAuthzRes.ListPoliciesResp{}
	for _, pol := range domainRes.GetPolicies() {
		gwRes.Policies = append(gwRes.Policies, domainPolicyToGatewayPolicy(pol))
	}

	return &gwRes, nil
}

// DeletePolicy removes a policy from authz-service by id.
func (a *AuthzServer) DeletePolicy(ctx context.Context,
	gwReq *gwAuthzReq.DeletePolicyReq) (*gwAuthzRes.DeletePolicyResp, error) {

	domainRes, err := a.client.DeletePolicy(ctx, (*authz.DeletePolicyReq)(gwReq))
	if err != nil {
		return nil, err
	}

	return (*gwAuthzRes.DeletePolicyResp)(domainPolicyToGatewayPolicy(domainRes.Policy)), nil
}

// IntrospectAll returns a list of all HTTP endpoints the requestor has access to and,
// for each endpoint, a map of the supported HTTP methods with a Boolean status
// indicating allowed or denied.
func (a *AuthzServer) IntrospectAll(
	ctx context.Context, gwReq *gwAuthzReq.IntrospectAllReq) (*gwAuthzRes.IntrospectResp, error) {

	methodsInfoV1 := policy.GetInfoMap()
	methodsInfoV2 := policy_v2.GetInfoMap()

	// Filter out parameterized API methods; can only evaluate concrete methods.
	mapByResourceAndActionV1 := pairs.InvertMapNonParameterized(methodsInfoV1)
	mapByResourceAndActionV2 := pairs.InvertMapNonParameterized(methodsInfoV2)

	// Extra work of hydrating not needed for IntrospectAll; already does that.
	endpointMap, err := a.getAllowedMap(ctx,
		mapByResourceAndActionV1, mapByResourceAndActionV2,
		methodsInfoV1, methodsInfoV2,
		false)
	if err != nil {
		return nil, err
	}

	return &gwAuthzRes.IntrospectResp{
		Endpoints: endpointMap,
	}, nil
}

// IntrospectSome returns a list of the supported HTTP methods with a Boolean status
// indicating allowed or denied, for each endpoint in the request list.
// All supplied endpoints must be non-parameterized.
func (a *AuthzServer) IntrospectSome(
	ctx context.Context, gwReq *gwAuthzReq.IntrospectSomeReq) (*gwAuthzRes.IntrospectResp, error) {

	log := ctxlogrus.Extract(ctx)
	log.Debugf("Requested paths: " + strings.Join(gwReq.Paths, ", "))

	methodsInfoV1 := policy.GetInfoMap()
	methodsInfoV2 := policy_v2.GetInfoMap()

	querySetV1 := getSelectedSubset(log, gwReq.Paths, methodsInfoV1)
	querySetV2 := getSelectedSubset(log, gwReq.Paths, methodsInfoV2)
	logEndpoints(log, querySetV1)
	logEndpoints(log, querySetV2)

	// Filter out parameterized API methods; can only evaluate concrete methods.
	mapByResourceAndActionV1 := pairs.InvertMapNonParameterized(querySetV1)
	mapByResourceAndActionV2 := pairs.InvertMapNonParameterized(querySetV2)

	endpointMap, err := a.getAllowedMap(ctx,
		mapByResourceAndActionV1, mapByResourceAndActionV2,
		methodsInfoV1, methodsInfoV2, true)
	if err != nil {
		return nil, err
	}

	logResult(log, endpointMap)
	return &gwAuthzRes.IntrospectResp{
		Endpoints: endpointMap,
	}, nil
}

// Introspect returns a list of the supported HTTP methods with a Boolean status
// indicating allowed or denied, for the given, single endpoint.
// This method must be used for parameterized endpoints
// but may also be used for non-parameterized endpoints.
func (a *AuthzServer) Introspect(
	ctx context.Context, gwReq *gwAuthzReq.IntrospectReq) (*gwAuthzRes.IntrospectResp, error) {

	log := ctxlogrus.Extract(ctx)
	realizedPath := gwReq.Path
	paramList := gwReq.Parameters // inputs validated during processing below

	methodsInfoV1 := policy.GetInfoMap()
	methodsInfoV2 := policy_v2.GetInfoMap()

	// Filter out methods that do not match the realizedPath
	mapByResourceAndActionV1, err := pairs.InvertMapParameterized(methodsInfoV1, realizedPath, paramList)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}
	logIntrospectionDetails(log, mapByResourceAndActionV1)

	mapByResourceAndActionV2, err := pairs.InvertMapParameterized(methodsInfoV2, realizedPath, paramList)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}
	logIntrospectionDetails(log, mapByResourceAndActionV2)

	endpointMap, err := a.getAllowedMap(ctx,
		mapByResourceAndActionV1, mapByResourceAndActionV2,
		methodsInfoV1, methodsInfoV2, true)
	if err != nil {
		return nil, err
	}

	// Replace endpoint with realized path
	// Note: For POST /introspect (this handler), it should be safe to assume
	// that there's only ever one endpoint under consideration.
	if len(endpointMap) == 1 {
		var m *gwAuthzRes.MethodsAllowed
		// Note (sr): haven't found a better way to take whatever is the
		// first value and replace the key.
		for _, v := range endpointMap {
			m = v
		}
		endpointMap = map[string]*gwAuthzRes.MethodsAllowed{
			realizedPath: m,
		}
	}
	return &gwAuthzRes.IntrospectResp{
		Endpoints: endpointMap,
	}, nil
}

// IntrospectAllProjects returns a list of all projects the requestor has access to.
func (a *AuthzServer) IntrospectAllProjects(
	ctx context.Context, gwReq *gwAuthzReq.IntrospectAllProjectsReq) (*gwAuthzRes.IntrospectProjectsResp, error) {

	methodsInfoV1 := policy.GetInfoMap()
	methodsInfoV2 := policy_v2.GetInfoMap()

	// Filter out parameterized API methods; can only evaluate concrete methods.
	// TODO?? Is that what we want?
	mapByResourceAndActionV1 := pairs.InvertMapNonParameterized(methodsInfoV1)
	mapByResourceAndActionV2 := pairs.InvertMapNonParameterized(methodsInfoV2)

	// Note that projects do not exist in v1 of course,
	// but as project introspection is so similar to endpoint introspection,
	// it was easy to mirror that code here.
	projects, err := a.getAllowedProjects(ctx,
		mapByResourceAndActionV1, mapByResourceAndActionV2,
		methodsInfoV1, methodsInfoV2,
		false)
	if err != nil {
		return nil, err
	}
	return &gwAuthzRes.IntrospectProjectsResp{
		Projects: projects,
	}, nil
}

func logResult(log *logrus.Entry, endpointMap map[string]*gwAuthzRes.MethodsAllowed) {
	if len(endpointMap) == 0 {
		log.Debug("Allowed paths: NONE")
		return
	}
	paths := make([]string, len(endpointMap))
	i := 0
	for k := range endpointMap {
		paths[i] = k
		i++
	}
	log.Debugf("Allowed paths: " + strings.Join(paths, ", "))
}

func logEndpoints(log *logrus.Entry, querySet map[string]pairs.Info) {
	if len(querySet) == 0 {
		log.Debug("Empty map!")
		return
	}
	endpoints := make([]string, len(querySet))
	i := 0
	for k := range querySet {
		endpoints[i] = k
		i++
	}
	log.Debugf("Endpoints to process: " + strings.Join(endpoints, ", "))
}

func getSelectedSubset(log *logrus.Entry, paths []string, methodsInfo map[string]pairs.Info) map[string]pairs.Info {
	// to make the main loop more efficient, build a hash first
	lookupHash := make(map[string]bool, len(paths))
	for _, path := range paths {
		lookupHash[path] = false
	}
	// Filter methodsInfo to just those elements in paths
	subset := make(map[string]pairs.Info, len(paths))
	for key, meth := range methodsInfo {
		if _, ok := lookupHash[meth.HTTPEndpoint]; ok {
			lookupHash[meth.HTTPEndpoint] = true // record that we've touched it
			subset[key] = meth
		}
	}
	reportBadPaths(log, lookupHash)
	return subset
}

func reportBadPaths(log *logrus.Entry, lookupHash map[string]bool) {
	var badPaths []string
	for path, value := range lookupHash {
		if !value { // have not touched it
			badPaths = append(badPaths, path)
		}
	}
	if len(badPaths) > 0 {
		log.Warn("Unrecognized endpoint paths: " + strings.Join(badPaths, ", "))
	}
}

func (a *AuthzServer) getAllowedMap(
	ctx context.Context,
	mapByResourceAndActionV1, mapByResourceAndActionV2 map[pairs.Pair][]string,
	methodsInfoV1, methodsInfoV2 map[string]pairs.Info,
	fullyHydrate bool) (map[string]*gwAuthzRes.MethodsAllowed, error) {

	log := ctxlogrus.Extract(ctx)

	// Fetches the id of the current user PLUS the team ids for that user
	subjects := auth_context.FromContext(ctx).Subjects

	resp, err := a.filterHandler.FilterAuthorizedPairs(ctx, subjects,
		mapByResourceAndActionV1, mapByResourceAndActionV2,
		methodsInfoV1, methodsInfoV2)
	if err != nil {
		log.WithError(err).Debug("Error on client.FilterAuthorizedPairs")
		return nil, err
	}
	endpointMap, err := pairs.GetEndpointMapFromResponse(
		resp.Pairs, resp.MethodsInfo, resp.MapByResourceAndAction, fullyHydrate)
	if err != nil {
		log.WithError(err).Debug("Error on pairs.GetEndpointMapFromResponse")
		return nil, err
	}
	return endpointMap, nil
}

func (a *AuthzServer) getAllowedProjects(
	ctx context.Context,
	mapByResourceAndActionV1, mapByResourceAndActionV2 map[pairs.Pair][]string,
	methodsInfoV1, methodsInfoV2 map[string]pairs.Info,
	fullyHydrate bool) ([]string, error) {

	log := ctxlogrus.Extract(ctx)

	// Fetches the id of the current user PLUS the team ids for that user
	subjects := auth_context.FromContext(ctx).Subjects

	inputPairs := pairs.GetKeys(mapByResourceAndActionV2)
	pairsV2 := make([]*authzV2.Pair, len(inputPairs))
	for i, p := range inputPairs {
		pairsV2[i] = &authzV2.Pair{Resource: p.Resource, Action: p.Action}
	}

	resp, err := a.clientV2.FilterAuthorizedProjects(ctx, &authzV2.FilterAuthorizedProjectsReq{
		Subjects: subjects,
	})
	if err != nil {
		log.WithError(err).Debug("Error on client.FilterAuthorizedProjects")
		return nil, err
	}
	return resp.Projects, nil
}

func domainPolicyToGatewayPolicy(pol *authz.Policy) *gwAuthzRes.Policy {
	return (*gwAuthzRes.Policy)(pol)
}

func logIntrospectionDetails(log *logrus.Entry, pairMap map[pairs.Pair][]string) {
	resources := make(map[string]interface{}, len(pairMap))
	for pair := range pairMap {
		resources[pair.Action] = pair.Resource
	}
	log.WithFields(resources).Info("expanded resource")
}
