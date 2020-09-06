package introspect

import (
	"context"
	"fmt"
	"strings"

	"github.com/grpc-ecosystem/go-grpc-middleware/logging/logrus/ctxlogrus"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	// Shared Response/Request definitions
	version "github.com/chef/automate/api/external/common/version"

	// Authz Service Requests/Response/Service definitions
	"github.com/chef/automate/api/interservice/authz"

	// Gateway Requests/Response/Service definitions
	"github.com/chef/automate/api/external/iam/v2/pairs"
	gwAuthzReq "github.com/chef/automate/api/external/iam/v2/request"
	gwAuthzRes "github.com/chef/automate/api/external/iam/v2/response"

	// other dependencies
	"github.com/chef/automate/api/external/iam/v2/policy"
	"github.com/chef/automate/components/automate-gateway/gateway/middleware"
	"github.com/chef/automate/lib/grpc/auth_context"
)

// AuthzServer is the server interface
type AuthzServer struct {
	client               authz.AuthorizationServiceClient // now only needed for GetVersion
	introspectionHandler middleware.IntrospectionHandler
}

// NewServer creates a server with its client
func NewServer(
	client authz.AuthorizationServiceClient,
	introspectionHandler middleware.IntrospectionHandler,
) *AuthzServer {
	return &AuthzServer{
		client:               client,
		introspectionHandler: introspectionHandler,
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

// IntrospectAll returns a list of all HTTP endpoints the requestor has access to and,
// for each endpoint, a map of the supported HTTP methods with a Boolean status
// indicating allowed or denied.
func (a *AuthzServer) IntrospectAll(
	ctx context.Context, gwReq *gwAuthzReq.IntrospectAllReq) (*gwAuthzRes.IntrospectResp, error) {

	methodsInfo := policy.GetInfoMap()

	// Filter out parameterized API methods; can only evaluate concrete methods.
	mapByResourceAndAction := pairs.InvertMapNonParameterized(methodsInfo)

	endpointMap, err := a.getAllowedMap(ctx, mapByResourceAndAction, methodsInfo)
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

	methodsInfo := policy.GetInfoMap()

	querySet := getSelectedSubset(log, gwReq.Paths, methodsInfo)
	logEndpoints(log, querySet)

	// Filter out parameterized API methods; can only evaluate concrete methods.
	mapByResourceAndAction := pairs.InvertMapNonParameterized(querySet)

	endpointMap, err := a.getAllowedMap(ctx, mapByResourceAndAction, methodsInfo)
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

	methodsInfo := policy.GetInfoMap()

	// Filter out methods that do not match the realizedPath
	mapByResourceAndAction, err := pairs.InvertMapParameterized(methodsInfo, realizedPath, paramList)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument, err.Error())
	}
	logIntrospectionDetails(log, mapByResourceAndAction)

	endpointMap, err := a.getAllowedMap(ctx, mapByResourceAndAction, methodsInfo)
	if err != nil {
		return nil, err
	}

	// Replace each endpoint key with realized path
	inflatedEndpointMap := make(map[string]*gwAuthzRes.MethodsAllowed)
	for _, methodsAllowed := range endpointMap {
		if inflatedEndpointMap[realizedPath] == nil {
			inflatedEndpointMap[realizedPath] = methodsAllowed
		} else {
			inflatedEndpointMap[realizedPath].Get =
				inflatedEndpointMap[realizedPath].Get || methodsAllowed.Get
			inflatedEndpointMap[realizedPath].Put =
				inflatedEndpointMap[realizedPath].Put || methodsAllowed.Put
			inflatedEndpointMap[realizedPath].Patch =
				inflatedEndpointMap[realizedPath].Patch || methodsAllowed.Patch
			inflatedEndpointMap[realizedPath].Delete =
				inflatedEndpointMap[realizedPath].Delete || methodsAllowed.Delete
			inflatedEndpointMap[realizedPath].Post =
				inflatedEndpointMap[realizedPath].Post || methodsAllowed.Post
		}
	}

	return &gwAuthzRes.IntrospectResp{
		Endpoints: inflatedEndpointMap,
	}, nil
}

func logResult(log *logrus.Entry, endpointMap map[string]*gwAuthzRes.MethodsAllowed) {
	paths := make([]string, len(endpointMap))
	i := 0
	for k, v := range endpointMap {
		methods := []string{}
		if v.Get {
			methods = append(methods, "Get")
		}
		if v.Put {
			methods = append(methods, "Put")
		}
		if v.Post {
			methods = append(methods, "Post")
		}
		if v.Delete {
			methods = append(methods, "Delete")
		}
		if v.Patch {
			methods = append(methods, "Patch")
		}
		if len(methods) > 0 {
			paths[i] = fmt.Sprintf("%s[%s]", k, strings.Join(methods, ","))
		} else {
			paths[i] = fmt.Sprintf("%s[NONE]", k)
		}
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
	mapByResourceAndAction map[pairs.Pair][]string,
	methodsInfo map[string]pairs.Info) (map[string]*gwAuthzRes.MethodsAllowed, error) {

	log := ctxlogrus.Extract(ctx)

	// Fetches the id of the current user PLUS the team ids for that user
	subjects := auth_context.FromContext(ctx).Subjects

	inputPairs := pairs.GetKeys(mapByResourceAndAction)

	filteredPairs, err := a.introspectionHandler.FilterAuthorizedPairs(ctx, subjects, inputPairs)
	if err != nil {
		log.WithError(err).Debug("Error on client.FilterAuthorizedPairs")
		return nil, err
	}
	endpointMap, err := pairs.GetEndpointMapFromResponse(
		filteredPairs, methodsInfo, mapByResourceAndAction, true)
	if err != nil {
		log.WithError(err).Debug("Error on pairs.GetEndpointMapFromResponse")
		return nil, err
	}
	return endpointMap, nil
}

func logIntrospectionDetails(log *logrus.Entry, pairMap map[pairs.Pair][]string) {
	resources := make(map[string]interface{}, len(pairMap))
	for pair := range pairMap {
		resources[pair.Action] = pair.Resource
	}
	log.WithFields(resources).Info("expanded resource")
}
