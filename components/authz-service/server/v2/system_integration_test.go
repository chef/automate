package v2_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/logger"

	api_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/authz-service/engine/opa"
)

// In these tests, we assert that our default policies are in place, and do
// their job as expected. For this, we setup a v2 server using a proper OPA
// engine instance, and run a few queries.

func TestSystemPolicies(t *testing.T) {
	ctx := context.Background()
	ts := setupWithOPA(t)
	cl := ts.authz

	isAuthorized := func(subject, action, resource string) func(*testing.T) {
		return func(t *testing.T) {
			resp, err := cl.IsAuthorized(ctx, &api_v2.IsAuthorizedReq{Subjects: []string{subject}, Resource: resource, Action: action})
			require.NoError(t, err)
			assert.True(t, resp.Authorized)
		}
	}

	cases := map[string]func(t *testing.T){
		"service version":                       isAuthorized("user:ldap:alice", "system:serviceVersion:get", "system:service:version"),
		"introspect all":                        isAuthorized("user:ldap:alice", "iam:introspect:getAll", "iam:introspect"),
		"introspect some":                       isAuthorized("user:ldap:alice", "iam:introspect:getSome", "iam:introspect"),
		"introspect get":                        isAuthorized("user:ldap:alice", "iam:introspect:get", "iam:introspect"),
		"get user record":                       isAuthorized("user:local:alice", "iam:users:get", "iam:users:alice"),
		"list user record":                      isAuthorized("user:local:alice", "iam:users:list", "iam:users:alice"),
		"d-s can do allthethings":               isAuthorized("tls:service:deployment-service:cert-id", "iam:users:delete", "iam:users:alice"),
		"ingest run as provider oc-erchef":      isAuthorized("tls:service:automate-cs-oc-erchef:cert", "infra:ingest:create", "infra:nodes:nodeUUID:runs"),
		"ingest action as provider oc-erchef":   isAuthorized("tls:service:automate-cs-oc-erchef:cert", "infra:ingest:create", "infra:actions"),
		"ingest delete as provider oc-erchef":   isAuthorized("tls:service:automate-cs-oc-erchef:cert", "infra:ingest:delete", "infra:nodes"),
		"ingest liveness as provider oc-erchef": isAuthorized("tls:service:automate-cs-oc-erchef:cert", "infra:ingest:create", "infra:nodes:nodeUUID:liveness"),
		"ingest run as provider cs-nginx":       isAuthorized("tls:service:automate-cs-nginx:cert", "infra:ingest:create", "infra:nodes:nodeUUID:runs"),
		"ingest action as provider nginx":       isAuthorized("tls:service:automate-cs-nginx:cert", "infra:ingest:create", "infra:actions"),
		"ingest delete as provider nginx":       isAuthorized("tls:service:automate-cs-nginx:cert", "infra:ingest:delete", "infra:nodes"),
		"ingest liveness as provider nginx":     isAuthorized("tls:service:automate-cs-nginx:cert", "infra:ingest:create", "infra:nodes:nodeUUID:liveness"),
	}

	for desc, test := range cases {
		t.Run(desc, test)
	}
}

func setupWithOPA(t *testing.T) testSetup {
	t.Helper()
	ctx := context.Background()

	l, err := logger.NewLogger("text", "debug")
	require.NoError(t, err, "init logger for storage+engine")

	o, err := opa.New(ctx, l)
	require.NoError(t, err, "init OPA")

	vChan := make(chan api_v2.Version, 1)
	emptyV1List := v1Lister{}
	ts := setupV2(t, o, o, &emptyV1List, vChan)
	_, err = ts.policy.MigrateToV2(ctx, &api_v2.MigrateToV2Req{})
	require.NoError(t, err)
	return ts
}
