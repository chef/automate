package introspect_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	authz "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/automate-gateway/api/iam/v2/request"
	"github.com/chef/automate/components/automate-gateway/api/iam/v2/response"
	"github.com/chef/automate/components/automate-gateway/gateway/middleware"
	"github.com/chef/automate/components/automate-gateway/gateway/middleware/authv2"
	"github.com/chef/automate/components/automate-gateway/handler/iam/v2/introspect"
	"github.com/chef/automate/components/automate-gateway/pkg/authorizer"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/test/helpers"

	// These next imports are strictly for the side-effect of registering resources
	// with the policy-mapping-global.
	// That is, the tests herein use the real policy store which is populated
	// piecewise by each package imported into the runtime.
	// Here in this testbed, that automatically includes less than 40 endpoints
	// (primarily nodes, notifications, and secrets at the time of writing).
	_ "github.com/chef/automate/api/external/cfgmgmt"
	_ "github.com/chef/automate/api/external/compliance/profiles"
	_ "github.com/chef/automate/api/external/ingest"
	_ "github.com/chef/automate/components/automate-gateway/api/iam/v2"
	_ "github.com/chef/automate/components/automate-gateway/api/notifications"
)

func TestIntrospectAll(t *testing.T) {
	authzSrv, s, hdlr := testServerAndHandler(t)
	defer s.Close()
	reset := func() {
		authzSrv.FilterAuthorizedPairsFunc = nil
	}

	cases := map[string]struct {
		authzResp *authz.FilterAuthorizedPairsResp
		expected  map[string]*response.MethodsAllowed
	}{
		"one response pair, mapped": {
			&authz.FilterAuthorizedPairsResp{Pairs: []*authz.Pair{
				{Resource: "notifications:rules", Action: "notifications:notifyRules:create"},
			}},
			map[string]*response.MethodsAllowed{"/notifications/rules": &response.MethodsAllowed{Post: true}},
		},
		"two response pairs, both mapped": {
			&authz.FilterAuthorizedPairsResp{Pairs: []*authz.Pair{
				{Resource: "notifications:rules", Action: "notifications:notifyRules:create"},
				{Resource: "iam:introspect", Action: "iam:introspect:getAll"},
			}},
			map[string]*response.MethodsAllowed{
				"/iam/v2/introspect":   &response.MethodsAllowed{Get: true},
				"/notifications/rules": &response.MethodsAllowed{Post: true},
			},
		},
		"two response pairs, both mapped, one with holes": {
			&authz.FilterAuthorizedPairsResp{Pairs: []*authz.Pair{
				{Resource: "notifications:rules", Action: "notifications:notifyRules:create"},
				{Resource: "notifications:rules:id}", Action: "notifications:notifyRules:delete"},
			}},
			map[string]*response.MethodsAllowed{"/notifications/rules": {Post: true}},
		},
	}

	for name, tc := range cases {
		t.Run(name, func(t *testing.T) {
			ctx := auth_context.NewContext(
				context.Background(), []string{"user:local:admin"}, []string{"project"}, "some:resource", "some:action:do", middleware.AuthV2.String())
			req := &request.IntrospectAllReq{}
			authzSrv.FilterAuthorizedPairsFunc = func(
				context.Context, *authz.FilterAuthorizedPairsReq) (*authz.FilterAuthorizedPairsResp, error) {
				return tc.authzResp, nil
			}
			resp, err := hdlr.IntrospectAll(ctx, req)
			require.NoError(t, err)
			require.NotNil(t, resp)
			for key, value := range tc.expected {
				assert.Contains(t, resp.Endpoints, key)
				assert.Equal(t, value, resp.Endpoints[key])
			}
		})
		reset()
	}
}

func TestIntrospectSome(t *testing.T) {
	authzSrv, s, hdlr := testServerAndHandler(t)
	defer s.Close()
	reset := func() {
		authzSrv.FilterAuthorizedPairsFunc = nil
	}

	cases := map[string]struct {
		authzResp *authz.FilterAuthorizedPairsResp
		req       *request.IntrospectSomeReq
		expected  map[string]*response.MethodsAllowed
	}{
		"empty response": {
			&authz.FilterAuthorizedPairsResp{},
			&request.IntrospectSomeReq{},
			map[string]*response.MethodsAllowed{},
		},
		"no response pair": {
			&authz.FilterAuthorizedPairsResp{},
			&request.IntrospectSomeReq{Paths: []string{
				"/foo/bar",
			}},
			map[string]*response.MethodsAllowed{},
		},
		"ONE response pair, from two requested with one an INVALID path": {
			&authz.FilterAuthorizedPairsResp{Pairs: []*authz.Pair{
				{Resource: "notifications:rules", Action: "notifications:notifyRules:create"},
			}},
			&request.IntrospectSomeReq{Paths: []string{
				"/foo/bar",
				"/notifications/rules",
			}},
			map[string]*response.MethodsAllowed{"/notifications/rules": {Post: true}},
		},
		"TWO response pairs, from two requested with one a DISALLOWED path": {
			&authz.FilterAuthorizedPairsResp{Pairs: []*authz.Pair{
				{Resource: "system:service:version", Action: "system:serviceVersion:get"},
			}},
			&request.IntrospectSomeReq{Paths: []string{
				"/notifications/rules",
				"/notifications/version",
			}},
			map[string]*response.MethodsAllowed{
				"/notifications/rules":   {Get: false, Post: false, Put: false, Delete: false, Patch: false},
				"/notifications/version": {Get: true, Post: false, Put: false, Delete: false, Patch: false},
			},
		},
		"two response pairs, from two requested": {
			&authz.FilterAuthorizedPairsResp{Pairs: []*authz.Pair{
				{Resource: "notifications:rules", Action: "notifications:notifyRules:create"},
				{Resource: "iam:introspect", Action: "iam:introspect:getAll"},
			}},
			&request.IntrospectSomeReq{Paths: []string{
				"/iam/v2/introspect",
				"/notifications/rules",
			}},
			map[string]*response.MethodsAllowed{
				"/iam/v2/introspect":   {Get: true},
				"/notifications/rules": {Post: true},
			},
		},
	}

	for name, tc := range cases {
		t.Run(name, func(t *testing.T) {
			ctx := auth_context.NewContext(
				context.Background(), []string{"user:local:admin"}, []string{"project"}, "some:resource", "some:action", middleware.AuthV2.String())
			authzSrv.FilterAuthorizedPairsFunc = func(
				context.Context, *authz.FilterAuthorizedPairsReq) (*authz.FilterAuthorizedPairsResp, error) {
				return tc.authzResp, nil
			}
			resp, err := hdlr.IntrospectSome(ctx, tc.req)
			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, tc.expected, resp.Endpoints)
		})
		reset()
	}
}

func TestIntrospect(t *testing.T) {
	authzSrv, s, hdlr := testServerAndHandler(t)
	defer s.Close()
	reset := func() {
		authzSrv.FilterAuthorizedPairsFunc = nil
	}

	cases := map[string]struct {
		authzResp *authz.FilterAuthorizedPairsResp
		req       *request.IntrospectReq
		expected  map[string]*response.MethodsAllowed
	}{
		"empty response": {
			&authz.FilterAuthorizedPairsResp{},
			&request.IntrospectReq{Parameters: []string{"foo", "bar"}, Path: "/foo/:foo/bar/:bar"},
			map[string]*response.MethodsAllowed{},
		},
		"response pair matching the request with param in path": {
			&authz.FilterAuthorizedPairsResp{Pairs: []*authz.Pair{
				{Resource: "iam:policies:f33a996c-b4e8-4328-9730-90f4b351fa6e", Action: "iam:policies:delete"},
			}},
			&request.IntrospectReq{Path: "/iam/v2/policies/f33a996c-b4e8-4328-9730-90f4b351fa6e"},
			map[string]*response.MethodsAllowed{"/iam/v2/policies/f33a996c-b4e8-4328-9730-90f4b351fa6e": &response.MethodsAllowed{Delete: true}},
		},
		"response pair matching the request with param in POST body": {
			&authz.FilterAuthorizedPairsResp{Pairs: []*authz.Pair{
				{Resource: "ingest:nodes:f33a996c-b4e8-4328-9730-90f4b351fa6e:runs", Action: "ingest:nodes:create"},
			}},
			&request.IntrospectReq{
				Parameters: []string{"entity_uuid=f33a996c-b4e8-4328-9730-90f4b351fa6e"},
				Path:       "/ingest/events/chef/run"},
			map[string]*response.MethodsAllowed{"/ingest/events/chef/run": &response.MethodsAllowed{Post: true}},
		},
		"response pair matching the request with multiple params in path": {
			&authz.FilterAuthorizedPairsResp{Pairs: []*authz.Pair{
				{Resource: "infra:nodes:42", Action: "infra:nodes:get"},
			}},
			&request.IntrospectReq{Path: "/cfgmgmt/nodes/42/runs/509"},
			map[string]*response.MethodsAllowed{"/cfgmgmt/nodes/42/runs/509": &response.MethodsAllowed{Get: true}},
		},
		// TODO: AUTH-1337 Either: enable this test case after providing support
		// or remove test case if decide not to support it
		// "response pair matching the request with excess params in path": {
		// 	&authz.FilterAuthorizedPairsResp{Pairs: []*authz.Pair{
		// 		{Resource: "compliance:profiles:storage:OwnerBob", Action: "read"},
		// 	}},
		// 	&request.IntrospectReq{Path: "/compliance/profiles/read/OwnerBob/bob/version/V1.34"},
		// 	map[string]*response.MethodsAllowed{"/compliance/profiles/read/{owner}/{name}/version/{version}": &response.MethodsAllowed{Get: true}},
		// },
	}

	for name, tc := range cases {
		t.Run(name, func(t *testing.T) {
			ctx := auth_context.NewContext(
				context.Background(), []string{"user:local:admin"}, []string{"project"}, "some:resource", "some:action", "Auth")
			authzSrv.FilterAuthorizedPairsFunc = func(
				context.Context, *authz.FilterAuthorizedPairsReq) (*authz.FilterAuthorizedPairsResp, error) {
				return tc.authzResp, nil
			}
			resp, err := hdlr.Introspect(ctx, tc.req)
			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, tc.expected, resp.Endpoints)
		})
		reset()
	}
}

func testServerAndHandler(t *testing.T) (
	*authz.AuthorizationServerMock,
	*grpctest.Server,
	*introspect.AuthzServer) {
	serviceCerts := helpers.LoadDevCerts(t, "authz-service")
	connFactory := secureconn.NewFactory(*serviceCerts)

	authSrv := authz.NewAuthorizationServerMock()

	g := connFactory.NewServer()
	authz.RegisterAuthorizationServer(g, authSrv)
	s := grpctest.NewServer(g)

	conn, err := connFactory.Dial("authz-service", s.URL)
	require.NoError(t, err)
	client := authz.NewAuthorizationClient(conn)

	return authSrv,
		s,
		introspect.NewServer(
			nil,
			authorizer.NewAuthorizer(authv2.AuthorizationHandler(client)))
}
