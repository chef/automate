package v2_test

import (
	"context"
	"fmt"
	"math/rand"
	"reflect"
	"strings"
	"testing"

	grpc_middleware "github.com/grpc-ecosystem/go-grpc-middleware"
	"github.com/jaswdr/faker"
	cache "github.com/patrickmn/go-cache"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/reflection"

	"github.com/chef/automate/lib/logger"
	uuid "github.com/chef/automate/lib/uuid4"

	api_v2 "github.com/chef/automate/api/interservice/authz/v2"
	constants_v2 "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/authz-service/engine"
	"github.com/chef/automate/components/authz-service/prng"
	grpc_server "github.com/chef/automate/components/authz-service/server"
	v2 "github.com/chef/automate/components/authz-service/server/v2"
	storage_v1 "github.com/chef/automate/components/authz-service/storage/v1"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
	memstore_v2 "github.com/chef/automate/components/authz-service/storage/v2/memstore"
	"github.com/chef/automate/components/authz-service/testhelpers"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/test/helpers"
)

var dummyWriter engine.V2p1Writer = nil

func TestCreatePolicy(t *testing.T) {
	ctx := context.Background()
	prng := prng.Seed(t)
	ts := setupV2p1WithWriter(t, dummyWriter)
	cl := ts.policy
	store := ts.policyCache
	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"fails with InvalidArgument when policy statement omits projects", func(t *testing.T) {
			_, items := addSomePoliciesToStore(t, store, prng)
			statement0 := api_v2.Statement{
				Effect:    api_v2.Statement_ALLOW,
				Projects:  []string{},
				Resources: []string{"cfgmgmt:delete", "cfgmgmt:list"},
				Actions:   []string{"cfgmgmt:nodes:*"},
			}
			req := api_v2.CreatePolicyReq{
				Id:         "policy1",
				Name:       "my favorite policy",
				Members:    []string{"team:local:admins", "user:local:alice"},
				Statements: []*api_v2.Statement{&statement0},
			}

			resp, err := cl.CreatePolicy(ctx, &req)

			require.Nil(t, resp)
			require.Equal(t, len(items), store.ItemCount())
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Regexp(t, "statement.*must include projects", err.Error())
		}},
		{"successfully creates policy with statement containing resources and actions inline", func(t *testing.T) {
			projects := []string{"project-1"}
			statement0 := api_v2.Statement{
				Effect:    api_v2.Statement_DENY,
				Resources: []string{"cfgmgmt:delete", "cfgmgmt:list"},
				Actions:   []string{"cfgmgmt:nodes:*"},
				Projects:  projects,
			}
			req := api_v2.CreatePolicyReq{
				Id:         "policy1",
				Name:       "my favorite policy",
				Members:    []string{"team:local:admins", "user:local:alice"},
				Statements: []*api_v2.Statement{&statement0},
				Projects:   projects,
			}
			resp, err := cl.CreatePolicy(ctx, &req)
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.Equal(t, 1, len(resp.Statements))
			assert.Equal(t, api_v2.Statement_DENY, resp.Statements[0].Effect)
			assert.Equal(t, []string{"cfgmgmt:nodes:*"}, resp.Statements[0].Actions)
			assert.Equal(t, []string{"cfgmgmt:delete", "cfgmgmt:list"}, resp.Statements[0].Resources)
			assert.Empty(t, resp.Statements[0].Role)
			assert.ElementsMatch(t, projects, resp.Statements[0].Projects)

			// Note: these could be repeated for any of the following tests. But
			// that wouldn't buy us much, so let's not do it.
			assert.Equal(t, []string{"team:local:admins", "user:local:alice"}, resp.Members)
			assert.Equal(t, "CUSTOM", resp.Type.String())

			// check the storage state => ensures that the grpc handler has passed
			// everything down
			assert.Equal(t, 1, store.ItemCount())
			pol := getPolicyFromStore(t, store, resp.Id)
			assert.Equal(t, "policy1", pol.ID)
			assert.Equal(t, "my favorite policy", pol.Name)
			assert.Equal(t, "team:local:admins", pol.Members[0].Name)
			assert.Equal(t, "user:local:alice", pol.Members[1].Name)
			assert.ElementsMatch(t, projects, pol.Projects)
			require.Equal(t, 1, len(pol.Statements))
			ras := pol.Statements[0]
			assert.Equal(t, storage.Deny, ras.Effect, "effect is deny")
			assert.Equal(t, []string{"cfgmgmt:nodes:*"}, ras.Actions)
			assert.Equal(t, []string{"cfgmgmt:delete", "cfgmgmt:list"}, ras.Resources)
		}},
		{"successfully creates policy with statement containing projects and role", func(t *testing.T) {
			statement0 := api_v2.Statement{
				Effect:   api_v2.Statement_ALLOW,
				Projects: []string{"my-project", "another-project"},
				Role:     "my-role",
			}
			req := api_v2.CreatePolicyReq{
				Id:         "policy1",
				Name:       "my favorite policy",
				Members:    []string{"team:local:admins", "user:local:alice"},
				Statements: []*api_v2.Statement{&statement0},
				Projects:   []string{"project-1"},
			}

			resp, err := cl.CreatePolicy(ctx, &req)

			require.NoError(t, err)
			require.NotNil(t, resp)

			require.Equal(t, 1, len(resp.Statements))
			assert.Equal(t, api_v2.Statement_ALLOW, resp.Statements[0].Effect)
			assert.Equal(t, "my-role", resp.Statements[0].Role)
			assert.Equal(t, []string{"my-project", "another-project"}, resp.Statements[0].Projects)
			assert.Empty(t, resp.Statements[0].Actions)
			assert.Equal(t, []string{"*"}, resp.Statements[0].Resources)

			// the only change to the previous test case is the type of statement, so we
			// only check that
			assert.Equal(t, 1, store.ItemCount())
			pol := getPolicyFromStore(t, store, resp.Id)
			require.Equal(t, 1, len(pol.Statements))
			rss := pol.Statements[0]
			assert.Equal(t, storage.Allow, rss.Effect, "effect is allow")
			assert.Equal(t, "my-role", rss.Role)
			assert.Equal(t, []string{"my-project", "another-project"}, rss.Projects)
		}},
		{"successfully creates policy with statement containing projects, roles, resources, and actions", func(t *testing.T) {
			statement0 := api_v2.Statement{
				Effect:    api_v2.Statement_DENY,
				Projects:  []string{"my-project", "another-project"},
				Role:      "my-role",
				Resources: []string{"cfgmgmt:delete", "cfgmgmt:list"},
				Actions:   []string{"cfgmgmt:nodes:*"},
			}
			req := api_v2.CreatePolicyReq{
				Id:         "policy1",
				Name:       "my favorite policy",
				Members:    []string{"team:local:admins", "user:local:alice"},
				Statements: []*api_v2.Statement{&statement0},
			}

			resp, err := cl.CreatePolicy(ctx, &req)

			require.NoError(t, err)
			require.NotNil(t, resp)
		}},
		{"sets a policy's statement's resources to wildcard if not provided", func(t *testing.T) {
			projects := []string{"project1"}
			statement0 := api_v2.Statement{
				Effect:   api_v2.Statement_DENY,
				Actions:  []string{"cfgmgmt:nodes:*"},
				Projects: projects,
			}
			req := api_v2.CreatePolicyReq{
				Id:         "policy1",
				Name:       "my favorite policy",
				Members:    []string{"team:local:admins", "user:local:alice"},
				Statements: []*api_v2.Statement{&statement0},
				Projects:   projects,
			}

			resp, err := cl.CreatePolicy(ctx, &req)
			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.ElementsMatch(t, projects, resp.Projects)

			assert.Equal(t, 1, store.ItemCount())
			pol := getPolicyFromStore(t, store, resp.Id)
			require.Equal(t, 1, len(pol.Statements))
			rss := pol.Statements[0]
			assert.Equal(t, storage.Deny, rss.Effect, "effect is deny")
			assert.Equal(t, []string{"*"}, rss.Resources)
			assert.ElementsMatch(t, projects, rss.Projects)
		}},
		{"successfully creates policy with multiple statements", func(t *testing.T) {
			statement0 := api_v2.Statement{
				Effect:   api_v2.Statement_DENY,
				Projects: []string{"my-project", "another-project"},
				Role:     "my-role",
			}
			statement1 := api_v2.Statement{
				Effect:    api_v2.Statement_DENY,
				Resources: []string{"cfgmgmt:delete", "cfgmgmt:list"},
				Actions:   []string{"cfgmgmt:nodes:*"},
				Projects:  []string{"project-1"},
			}
			statement2 := api_v2.Statement{
				Effect:   api_v2.Statement_ALLOW,
				Projects: []string{"my-other-project"},
				Role:     "my-other-role",
			}
			req := api_v2.CreatePolicyReq{
				Id:         "policy1",
				Name:       "my favorite policy",
				Members:    []string{"team:local:admins", "user:local:alice"},
				Statements: []*api_v2.Statement{&statement0, &statement1, &statement2},
				Projects:   []string{"project-1"},
			}

			resp, err := cl.CreatePolicy(ctx, &req)

			require.NoError(t, err)
			require.NotNil(t, resp)
		}},
		{"policy with no statement returns InvalidArgument", func(t *testing.T) {
			_, items := addSomePoliciesToStore(t, store, prng)
			req := api_v2.CreatePolicyReq{
				Id:      "policy1",
				Name:    "my favorite policy",
				Members: []string{"team:local:admins", "user:local:alice"},
			}

			resp, err := cl.CreatePolicy(ctx, &req)

			require.Nil(t, resp)
			require.Equal(t, len(items), store.ItemCount())
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		}},
		{"successfully creates policy with one project", func(t *testing.T) {
			expProjects := []string{"project-1"}
			statement0 := api_v2.Statement{
				Effect:   api_v2.Statement_DENY,
				Actions:  []string{"cfgmgmt:nodes:*"},
				Projects: expProjects,
			}
			_, items := addSomePoliciesToStore(t, store, prng)
			req := api_v2.CreatePolicyReq{
				Id:         "policy1",
				Name:       "my favorite policy",
				Members:    []string{"team:local:admins", "user:local:alice"},
				Statements: []*api_v2.Statement{&statement0},
				Projects:   expProjects,
			}

			resp, err := cl.CreatePolicy(ctx, &req)

			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, len(items)+1, store.ItemCount())
			assert.ElementsMatch(t, expProjects, resp.Projects)
			assert.ElementsMatch(t, expProjects, resp.Statements[0].Projects)
		}},
		{"successfully creates policy with multiple projects", func(t *testing.T) {
			expProjects := []string{"project-1", "project-2"}
			statement0 := api_v2.Statement{
				Effect:   api_v2.Statement_DENY,
				Actions:  []string{"cfgmgmt:nodes:*"},
				Projects: expProjects,
			}
			_, items := addSomePoliciesToStore(t, store, prng)
			req := api_v2.CreatePolicyReq{
				Id:         "policy1",
				Name:       "my favorite policy",
				Members:    []string{"team:local:admins", "user:local:alice"},
				Statements: []*api_v2.Statement{&statement0},
				Projects:   expProjects,
			}

			resp, err := cl.CreatePolicy(ctx, &req)

			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, len(items)+1, store.ItemCount())
			assert.ElementsMatch(t, expProjects, resp.Projects)
			assert.ElementsMatch(t, expProjects, resp.Statements[0].Projects)
		}},
		{"creates an unassigned policy when policy has no projects", func(t *testing.T) {
			statement0 := api_v2.Statement{
				Effect:   api_v2.Statement_DENY,
				Actions:  []string{"cfgmgmt:nodes:*"},
				Projects: []string{"project-1"},
			}
			_, items := addSomePoliciesToStore(t, store, prng)
			req := api_v2.CreatePolicyReq{
				Id:         "policy1",
				Name:       "my favorite policy",
				Members:    []string{"team:local:admins", "user:local:alice"},
				Statements: []*api_v2.Statement{&statement0},
			}

			resp, err := cl.CreatePolicy(ctx, &req)

			assert.NoError(t, err)
			assert.NotNil(t, resp)
			assert.ElementsMatch(t, []string{}, resp.Projects)
			assert.Equal(t, len(items)+1, store.ItemCount())
		}},
		{"creates an unassigned policy when policy has empty projects", func(t *testing.T) {
			statement0 := api_v2.Statement{
				Effect:   api_v2.Statement_DENY,
				Actions:  []string{"cfgmgmt:nodes:*"},
				Projects: []string{"project-1"},
			}

			expProjects := []string{}
			_, items := addSomePoliciesToStore(t, store, prng)
			req := api_v2.CreatePolicyReq{
				Id:         "policy1",
				Name:       "my favorite policy",
				Members:    []string{"team:local:admins", "user:local:alice"},
				Statements: []*api_v2.Statement{&statement0},
				Projects:   expProjects,
			}

			resp, err := cl.CreatePolicy(ctx, &req)

			assert.NoError(t, err)
			assert.NotNil(t, resp)
			assert.ElementsMatch(t, []string{}, resp.Projects)
			assert.Equal(t, len(items)+1, store.ItemCount())
		}},
		{"successfully creates policy with duplicate name", func(t *testing.T) {
			target, items := addSomePoliciesToStore(t, store, prng)
			statement0 := api_v2.Statement{
				Effect:    api_v2.Statement_DENY,
				Role:      "my-role",
				Resources: []string{"cfgmgmt:nodes"},
				Actions:   []string{"cfgmgmt:nodes:*"},
				Projects:  []string{"project1"},
			}
			req := api_v2.CreatePolicyReq{
				Id:         "my-favorite-policy",
				Name:       target.Name,
				Members:    []string{"user:local:alice"},
				Statements: []*api_v2.Statement{&statement0},
			}

			resp, err := cl.CreatePolicy(ctx, &req)

			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, len(items)+1, store.ItemCount())
		}},
		{"fails with InvalidArgument when policy contains statement with empty actions AND no role", func(t *testing.T) {
			_, items := addSomePoliciesToStore(t, store, prng)
			statement0 := api_v2.Statement{
				Effect:    api_v2.Statement_DENY,
				Projects:  []string{},
				Role:      "",
				Resources: []string{"cfgmgmt:delete", "cfgmgmt:list"},
				Actions:   []string{},
			}
			req := api_v2.CreatePolicyReq{
				Id:         "policy1",
				Name:       "my favorite policy",
				Members:    []string{"team:local:admins", "user:local:alice"},
				Statements: []*api_v2.Statement{&statement0},
			}

			resp, err := cl.CreatePolicy(ctx, &req)

			require.Nil(t, resp)
			require.Equal(t, len(items), store.ItemCount())
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		}},
		{"fails with InvalidArgument when an ID isn't provided", func(t *testing.T) {
			_, items := addSomePoliciesToStore(t, store, prng)

			req := api_v2.CreatePolicyReq{
				Name:    "my favorite policy",
				Members: []string{"team:local:admins", "user:local:alice"},
			}

			resp, err := cl.CreatePolicy(ctx, &req)

			require.Nil(t, resp)
			require.Equal(t, len(items), store.ItemCount())
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		}},
		{"fails with InvalidArgument when an ID has wrong characters", func(t *testing.T) {
			_, items := addSomePoliciesToStore(t, store, prng)

			req := api_v2.CreatePolicyReq{
				Id:      "underscores_are_not_allowed",
				Name:    "my favorite policy",
				Members: []string{"team:local:admins", "user:local:alice"},
			}

			resp, err := cl.CreatePolicy(ctx, &req)

			require.Nil(t, resp)
			require.Equal(t, len(items), store.ItemCount())
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		}},
		{"fails with AlreadyExists when policy ID is already used", func(t *testing.T) {
			target, items := addSomePoliciesToStore(t, store, prng)
			statement0 := api_v2.Statement{
				Effect:    api_v2.Statement_DENY,
				Role:      "my-role",
				Resources: []string{"cfgmgmt:nodes"},
				Actions:   []string{"cfgmgmt:nodes:*"},
				Projects:  []string{"project1"},
			}
			req := api_v2.CreatePolicyReq{
				Id:         target.ID,
				Name:       "my favorite policy",
				Members:    []string{"user:local:alice"},
				Statements: []*api_v2.Statement{&statement0},
			}

			resp, err := cl.CreatePolicy(ctx, &req)

			require.Nil(t, resp)
			require.Equal(t, len(items), store.ItemCount())
			grpctest.AssertCode(t, codes.AlreadyExists, err)
		}},
		{"successfully creates policy for unassigned projects", func(t *testing.T) {
			_, items := addSomePoliciesToStore(t, store, prng)
			statement0 := api_v2.Statement{
				Effect:    api_v2.Statement_ALLOW,
				Resources: []string{"cfgmgmt:delete", "cfgmgmt:list"},
				Actions:   []string{"cfgmgmt:nodes:*"},
				Projects:  []string{constants_v2.UnassignedProjectID},
			}
			req := api_v2.CreatePolicyReq{
				Id:         "policy1",
				Name:       "my favorite policy",
				Members:    []string{"team:local:admins", "user:local:alice"},
				Statements: []*api_v2.Statement{&statement0},
			}

			resp, err := cl.CreatePolicy(ctx, &req)

			require.NoError(t, err)
			require.NotNil(t, resp)
			assert.Equal(t, len(items)+1, store.ItemCount())
			assert.ElementsMatch(t, []string{constants_v2.UnassignedProjectID}, resp.Statements[0].Projects)
		}},

		{"successfully creates policy with wildcard projects", func(t *testing.T) {
			statement0 := api_v2.Statement{
				Effect:   api_v2.Statement_DENY,
				Projects: []string{"my-project", "another-project"},
				Role:     "my-role",
			}
			statement1 := api_v2.Statement{
				Effect:    api_v2.Statement_DENY,
				Resources: []string{"cfgmgmt:delete", "cfgmgmt:list"},
				Actions:   []string{"cfgmgmt:nodes:*"},
				Projects:  []string{constants_v2.AllProjectsExternalID},
			}
			statement2 := api_v2.Statement{
				Effect:   api_v2.Statement_ALLOW,
				Projects: []string{"my-other-project"},
				Role:     "my-other-role",
			}
			req := api_v2.CreatePolicyReq{
				Id:         "policy1",
				Name:       "my favorite policy",
				Members:    []string{"team:local:admins", "user:local:alice"},
				Statements: []*api_v2.Statement{&statement0, &statement1, &statement2},
			}

			resp, err := cl.CreatePolicy(ctx, &req)

			require.NoError(t, err)
			require.NotNil(t, resp)
			require.Equal(t, 1, store.ItemCount())
			pol := getPolicyFromStore(t, store, resp.Id)
			require.Equal(t, 3, len(pol.Statements))
			require.ElementsMatch(t, []string{"my-project", "another-project"}, pol.Statements[0].Projects)
			require.ElementsMatch(t, []string{"my-project", "another-project"}, resp.Statements[0].Projects)
			require.ElementsMatch(t, []string{"my-other-project"}, pol.Statements[2].Projects)
			require.ElementsMatch(t, []string{"my-other-project"}, resp.Statements[2].Projects)

			assertPoliciesMatch(t, &pol, resp)
			// check internal vs external representation: the alignment of these two fields
			// is covered by assertPoliciesMatch, but important to check the specific expectations here, too
			assert.ElementsMatch(t, []string{constants_v2.AllProjectsID}, pol.Statements[1].Projects)
			assert.ElementsMatch(t, []string{constants_v2.AllProjectsExternalID}, resp.Statements[1].Projects)
		}},
		{"successfully creates policy with assorted projects", func(t *testing.T) {
			statement0 := api_v2.Statement{
				Effect:    api_v2.Statement_ALLOW,
				Resources: []string{"cfgmgmt:delete", "cfgmgmt:list"},
				Actions:   []string{"cfgmgmt:nodes:*"},
				Projects:  []string{constants_v2.UnassignedProjectID},
			}
			statement1 := api_v2.Statement{
				Effect:    api_v2.Statement_DENY,
				Resources: []string{"cfgmgmt:delete", "cfgmgmt:list"},
				Actions:   []string{"cfgmgmt:nodes:*"},
				Projects:  []string{constants_v2.AllProjectsExternalID},
			}
			statement2 := api_v2.Statement{
				Effect:   api_v2.Statement_ALLOW,
				Role:     "my-other-role",
				Projects: []string{"my-project", "another-project"},
			}
			req := api_v2.CreatePolicyReq{
				Id:         "policy1",
				Name:       "my favorite policy",
				Members:    []string{"team:local:admins", "user:local:alice"},
				Statements: []*api_v2.Statement{&statement0, &statement1, &statement2},
			}

			resp, err := cl.CreatePolicy(ctx, &req)

			require.NoError(t, err)
			require.NotNil(t, resp)
			require.Equal(t, 1, store.ItemCount())
			pol := getPolicyFromStore(t, store, resp.Id)
			assertPoliciesMatch(t, &pol, resp)
		}},
	}
	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		store.Flush()
	}
}

func TestDeletePolicy(t *testing.T) {
	ctx := context.Background()
	prng := prng.Seed(t)
	ts := setupV2p1WithWriter(t, dummyWriter)
	cl := ts.policy
	store := ts.policyCache

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"fails with InvalidArgument when ID is empty", func(t *testing.T) {
			require.Zero(t, store.ItemCount())
			req := api_v2.DeletePolicyReq{Id: ""}

			_, err := cl.DeletePolicy(ctx, &req)

			grpctest.AssertCode(t, codes.InvalidArgument, err)
		}},
		{"fails with InvalidArgument when ID is not valid", func(t *testing.T) {
			require.Zero(t, store.ItemCount())
			req := api_v2.DeletePolicyReq{Id: "no spacess"}

			_, err := cl.DeletePolicy(ctx, &req)

			grpctest.AssertCode(t, codes.InvalidArgument, err)
		}},
		{"fails with NotFound when deleting from empty store", func(t *testing.T) {
			require.Zero(t, store.ItemCount())
			req := api_v2.DeletePolicyReq{
				Id: "75b3f659-b734-4b37-84bf-15a53a67349c",
			}

			_, err := cl.DeletePolicy(ctx, &req)

			grpctest.AssertCode(t, codes.NotFound, err)
		}},
		{"deletes unattached policy from store containing a single policy", func(t *testing.T) {
			pol := genPolicy(t, "", prng)
			pol.Members = []storage.Member{} // make the policy detached
			require.Zero(t, store.ItemCount())
			store.Add(pol.ID, &pol, cache.NoExpiration)
			req := api_v2.DeletePolicyReq{Id: pol.ID}

			_, err := cl.DeletePolicy(ctx, &req)

			assert.NoError(t, err)
			assert.Zero(t, store.ItemCount())

			store.Flush()
		}},
		{"deletes UNATTACHED policy from store containing multiple policies", func(t *testing.T) {
			addArbitraryPoliciesToStore(t, store, prng, 3)
			pol := genPolicy(t, "", prng)
			pol.Members = []storage.Member{} // make the policy detached
			store.Add(pol.ID, &pol, cache.NoExpiration)
			itemsAdded := 4
			require.Equal(t, itemsAdded, store.ItemCount())
			req := api_v2.DeletePolicyReq{Id: pol.ID}

			_, err := cl.DeletePolicy(ctx, &req)

			require.NoError(t, err)
			require.Equal(t, itemsAdded-1, store.ItemCount())

			store.Flush()
		}},
		{"deletes ATTACHED policy from store containing multiple policies", func(t *testing.T) {
			pol, items := addSomePoliciesToStore(t, store, prng)
			itemsAdded := len(items)
			require.Equal(t, itemsAdded, store.ItemCount())

			req := api_v2.DeletePolicyReq{
				Id: pol.ID,
			}

			_, err := cl.DeletePolicy(ctx, &req)

			require.NoError(t, err)
			require.Equal(t, itemsAdded-1, store.ItemCount())

			store.Flush()
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		store.Flush()
	}
}

func TestListPolicies(t *testing.T) {
	existingPolicyId := "75b3f659-b734-4b37-84bf-15a53a67349c"
	ctx := context.Background()
	prng := prng.Seed(t)
	req := api_v2.ListPoliciesReq{} // it's not changing, can be reused
	ts := setupV2p1WithWriter(t, dummyWriter)
	cl := ts.policy
	store := ts.policyCache

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"empty store", func(t *testing.T) {
			require.Empty(t, store.Items())

			resp, err := cl.ListPolicies(ctx, &req)

			assert.NoError(t, err)
			assert.Empty(t, resp.Policies)
		}},
		{"one policy in store", func(t *testing.T) {
			// arrange
			id := existingPolicyId
			statement := storage.Statement{
				Effect:    storage.Deny,
				Resources: []string{"compliance:profiles"},
				Actions:   []string{"delete"},
			}
			member, err := storage.NewMember("team:local:heroines")
			require.NoError(t, err)
			storedPol := storage.Policy{
				ID:         id,
				Name:       "testPolicy1",
				Members:    []storage.Member{member},
				Type:       storage.Custom,
				Statements: []storage.Statement{statement},
			}
			store.Add(id, &storedPol, cache.NoExpiration)

			// act
			resp, err := cl.ListPolicies(ctx, &req)
			require.NoError(t, err)

			// assert
			require.Equal(t, 1, len(resp.Policies))
			pol := resp.Policies[0]
			assert.Equal(t, []string{"team:local:heroines"}, pol.Members)
			assert.Equal(t, "CUSTOM", pol.Type.String())

			require.Equal(t, 1, len(pol.Statements))
			assert.Equal(t, api_v2.Statement_DENY, pol.Statements[0].Effect)
			assert.Equal(t, []string{"delete"}, pol.Statements[0].Actions)
			assert.Equal(t, []string{"compliance:profiles"}, pol.Statements[0].Resources)
			assert.Empty(t, pol.Statements[0].Role)
			assert.Empty(t, pol.Statements[0].Projects)
		}},
		{"multiple policies in store", func(t *testing.T) {
			_, items := addSomePoliciesToStore(t, store, prng)

			resp, err := cl.ListPolicies(ctx, &req)
			require.NoError(t, err)

			require.Equal(t, len(items), len(resp.Policies))
			for _, pol := range resp.Policies {
				storedPol := items[pol.Id]
				assertPoliciesMatch(t, &storedPol, pol)
			}
		}},
		{"returns mapped meta-project when present", func(t *testing.T) {
			_, items := addSomePoliciesToStore(t, store, prng)
			storedPol := genPolicy(t, "", prng)
			storedPol.Statements[0].Projects = []string{constants_v2.AllProjectsID}
			store.Add(storedPol.ID, &storedPol, cache.NoExpiration)

			resp, err := cl.ListPolicies(ctx, &req)
			require.NoError(t, err)

			require.Equal(t, len(items)+1, len(resp.Policies))
			for _, pol := range resp.Policies {
				if pol.Id == storedPol.ID {
					assert.Equal(t, constants_v2.AllProjectsExternalID, pol.Statements[0].Projects[0])
				} else {
					p := items[pol.Id]
					assertPoliciesMatch(t, &p, pol)
				}
			}
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		store.Flush()
	}
}

func TestListPolicyMembers(t *testing.T) {
	ctx := context.Background()
	prng := prng.Seed(t)
	ts := setupV2p1WithWriter(t, dummyWriter)
	cl := ts.policy
	store := ts.policyCache
	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"successfully finds policy with a single item in store and lists its members", func(t *testing.T) {
			require.Zero(t, store.ItemCount())
			storedPol := addArbitraryPolicyToStore(t, store, prng)
			req := api_v2.ListPolicyMembersReq{
				Id: storedPol.ID,
			}

			resp, err := cl.ListPolicyMembers(ctx, &req)
			require.NoError(t, err)

			assert.ElementsMatch(t, storage.MemberSliceToStringSlice(storedPol.Members), resp.Members)
			polFromStore := getPolicyFromStore(t, store, storedPol.ID)
			assert.ElementsMatch(t, storage.MemberSliceToStringSlice(polFromStore.Members), resp.Members)
		}},
		{"successfully finds policy with multiple items in store and lists its members", func(t *testing.T) {
			storedPol, items := addSomePoliciesToStore(t, store, prng)
			req := api_v2.ListPolicyMembersReq{
				Id: storedPol.ID,
			}

			resp, err := cl.ListPolicyMembers(ctx, &req)
			require.NoError(t, err)

			require.Equal(t, len(items), store.ItemCount())
			assert.ElementsMatch(t, storage.MemberSliceToStringSlice(storedPol.Members), resp.Members)
			polFromStore := getPolicyFromStore(t, store, storedPol.ID)
			assert.ElementsMatch(t, storage.MemberSliceToStringSlice(polFromStore.Members), resp.Members)
		}},
		{"fails with NotFound when ID doesn't match any policies", func(t *testing.T) {
			addSomePoliciesToStore(t, store, prng)
			nonExistentId := "75b3f659-b734-4b37-84bf-12a34b56789c"
			req := api_v2.ListPolicyMembersReq{
				Id: nonExistentId,
			}

			resp, err := cl.ListPolicyMembers(ctx, &req)

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.NotFound, err)
		}},
		{"fails with InvalidArgument when ID is not provided", func(t *testing.T) {
			addSomePoliciesToStore(t, store, prng)
			req := api_v2.ListPolicyMembersReq{Id: ""}

			resp, err := cl.ListPolicyMembers(ctx, &req)

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		}},
		{"fails with InvalidArgument when ID is not valid", func(t *testing.T) {
			addSomePoliciesToStore(t, store, prng)
			req := api_v2.ListPolicyMembersReq{Id: "no spaces"}

			resp, err := cl.ListPolicyMembers(ctx, &req)

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		store.Flush()
	}
}

func TestGetPolicy(t *testing.T) {
	ctx := context.Background()
	prng := prng.Seed(t)
	ts := setupV2p1WithWriter(t, dummyWriter)
	cl := ts.policy
	store := ts.policyCache
	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"successfully finds policy with a single item in store", func(t *testing.T) {
			require.Zero(t, store.ItemCount())
			storedPol := addArbitraryPolicyToStore(t, store, prng)
			req := api_v2.GetPolicyReq{
				Id: storedPol.ID,
			}

			pol, err := cl.GetPolicy(ctx, &req)
			require.NoError(t, err)

			assertPoliciesMatch(t, &storedPol, pol)
		}},
		{"successfully finds policy with multiple items in store", func(t *testing.T) {
			storedPol, items := addSomePoliciesToStore(t, store, prng)
			req := api_v2.GetPolicyReq{
				Id: storedPol.ID,
			}

			pol, err := cl.GetPolicy(ctx, &req)
			require.NoError(t, err)

			require.Equal(t, len(items), store.ItemCount())
			assertPoliciesMatch(t, &storedPol, pol)
		}},
		{"returns mapped meta-project when present", func(t *testing.T) {
			storedPol := genPolicy(t, "", prng)
			storedPol.Statements[0].Projects = []string{constants_v2.AllProjectsID}
			require.Zero(t, store.ItemCount())
			store.Add(storedPol.ID, &storedPol, cache.NoExpiration)
			req := api_v2.GetPolicyReq{
				Id: storedPol.ID,
			}

			pol, err := cl.GetPolicy(ctx, &req)
			require.NoError(t, err)

			assert.Equal(t, constants_v2.AllProjectsExternalID, pol.Statements[0].Projects[0])
		}},
		{"fails with NotFound when ID doesn't match any policies", func(t *testing.T) {
			addSomePoliciesToStore(t, store, prng)
			nonExistentId := "75b3f659-b734-4b37-84bf-12a34b56789c"
			req := api_v2.GetPolicyReq{
				Id: nonExistentId,
			}

			pol, err := cl.GetPolicy(ctx, &req)

			require.Nil(t, pol)
			grpctest.AssertCode(t, codes.NotFound, err)
		}},
		{"fails with InvalidArgument when ID isn't provided", func(t *testing.T) {
			req := api_v2.GetPolicyReq{Id: ""}

			pol, err := cl.GetPolicy(ctx, &req)

			require.Nil(t, pol)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		}},
		{"fails with InvalidArgument when ID isn't valid", func(t *testing.T) {
			req := api_v2.GetPolicyReq{Id: "no spaces"}

			pol, err := cl.GetPolicy(ctx, &req)

			require.Nil(t, pol)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		store.Flush()
	}
}

func TestUpdatePolicy(t *testing.T) {
	existingPolicyId := "75b3f659-b734-4b37-84bf-15a53a67349c"
	testStatement := []*api_v2.Statement{
		&api_v2.Statement{
			Effect:    api_v2.Statement_ALLOW,
			Resources: []string{"compliance:profiles"},
			Actions:   []string{"compliance:profiles:upload"},
			Projects:  []string{constants_v2.AllProjectsExternalID},
		},
	}
	ctx := context.Background()
	prng := prng.Seed(t)
	ts := setupV2p1WithWriter(t, dummyWriter)
	cl := ts.policy
	store := ts.policyCache
	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"fails with InvalidArgument when missing policy ID", func(t *testing.T) {
			req := api_v2.UpdatePolicyReq{
				Name: "testPolicy1",
				Statements: []*api_v2.Statement{
					&api_v2.Statement{
						Effect:    api_v2.Statement_ALLOW,
						Resources: []string{"compliance:profiles"},
						Actions:   []string{"compliance:profiles:upload"},
						Projects:  []string{constants_v2.AllProjectsExternalID},
					},
				},
			}

			pol, err := cl.UpdatePolicy(ctx, &req)

			require.Nil(t, pol)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		}},
		{"fails with InvalidArgument when ID is invalid", func(t *testing.T) {
			req := api_v2.UpdatePolicyReq{
				Id:         "no spaces",
				Name:       "testPolicy1",
				Statements: testStatement,
			}

			pol, err := cl.UpdatePolicy(ctx, &req)

			require.Nil(t, pol)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		}},
		{"fails with InvalidArgument when missing policy name", func(t *testing.T) {
			req := api_v2.UpdatePolicyReq{
				Id:         existingPolicyId,
				Statements: testStatement,
			}

			pol, err := cl.UpdatePolicy(ctx, &req)

			require.Nil(t, pol)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		}},
		{"fails with NotFound when policy ID doesn't match any policies", func(t *testing.T) {
			addSomePoliciesToStore(t, store, prng)
			req := api_v2.UpdatePolicyReq{
				Id:         "this-is-wrong",
				Name:       "testPolicy1",
				Statements: testStatement,
			}

			pol, err := cl.UpdatePolicy(ctx, &req)

			require.Nil(t, pol)
			grpctest.AssertCode(t, codes.NotFound, err)
		}},
		{"successfully updates policy name", func(t *testing.T) {
			storedPol, _ := addSomePoliciesToStore(t, store, prng)
			req := api_v2.UpdatePolicyReq{
				Id:         storedPol.ID,
				Name:       "newTestPolicy1",
				Statements: testStatement,
			}

			pol, err := cl.UpdatePolicy(ctx, &req)

			require.NoError(t, err)
			assert.Equal(t, "newTestPolicy1", pol.Name)
			nameInStore := getPolicyFromStore(t, store, storedPol.ID).Name
			assert.Equal(t, pol.Name, nameInStore)
			storedPol = getPolicyFromStore(t, store, storedPol.ID)
			assertPoliciesMatch(t, &storedPol, pol)
		}},
		{"successfully updates policy members", func(t *testing.T) {
			storedPol, _ := addSomePoliciesToStore(t, store, prng)
			req := api_v2.UpdatePolicyReq{
				Id:         storedPol.ID,
				Name:       "TestPolicy1",
				Members:    []string{"team:local:heroines", "team:local:ladies"},
				Statements: testStatement,
			}

			pol, err := cl.UpdatePolicy(ctx, &req)

			require.NoError(t, err)
			assert.Equal(t, []string{"team:local:heroines", "team:local:ladies"}, pol.Members)
			membersInStore := getPolicyFromStore(t, store, storedPol.ID).Members
			assertMembersMatch(t, membersInStore, pol.Members)
			storedPol = getPolicyFromStore(t, store, storedPol.ID)
			assertPoliciesMatch(t, &storedPol, pol)
		}},
		{"successfully updates policy statements", func(t *testing.T) {
			storedPol, _ := addSomePoliciesToStore(t, store, prng)
			statement0 := api_v2.Statement{
				Effect:    api_v2.Statement_ALLOW,
				Resources: []string{"compliance:profiles"},
				Actions:   []string{"compliance:profiles:upload"},
				Projects:  []string{constants_v2.AllProjectsExternalID},
			}
			statement1 := api_v2.Statement{
				Effect:    api_v2.Statement_ALLOW,
				Resources: []string{"cfgmgmt:delete", "cfgmgmt:list"},
				Actions:   []string{"cfgmgmt:nodes:*"},
				Projects:  []string{constants_v2.UnassignedProjectID},
			}
			req := api_v2.UpdatePolicyReq{
				Id:         storedPol.ID,
				Name:       "TestPolicy1",
				Statements: []*api_v2.Statement{&statement0, &statement1},
			}

			pol, err := cl.UpdatePolicy(ctx, &req)

			require.NoError(t, err)

			// These individual statement checks are included in the final assertPoliciesMatch,
			// but useful to check these key elements explicitly.
			s0 := pol.Statements[0]
			assert.Equal(t, api_v2.Statement_ALLOW, s0.Effect)
			assert.Equal(t, []string{"compliance:profiles"}, s0.Resources)
			assert.Equal(t, []string{"compliance:profiles:upload"}, s0.Actions)
			assert.Equal(t, []string{constants_v2.AllProjectsExternalID}, s0.Projects)

			s1 := pol.Statements[1]
			assert.Equal(t, api_v2.Statement_ALLOW, s1.Effect)
			assert.Equal(t, []string{"cfgmgmt:delete", "cfgmgmt:list"}, s1.Resources)
			assert.Equal(t, []string{"cfgmgmt:nodes:*"}, s1.Actions)
			assert.Equal(t, []string{constants_v2.UnassignedProjectID}, s1.Projects)

			storedPol = getPolicyFromStore(t, store, storedPol.ID)
			assertPoliciesMatch(t, &storedPol, pol)
		}},
		{"successfully updates multiple properties at once", func(t *testing.T) {
			storedPol, _ := addSomePoliciesToStore(t, store, prng)
			req := api_v2.UpdatePolicyReq{
				Id:         storedPol.ID,
				Name:       "newTestPolicy1",
				Members:    []string{"team:local:ladies"},
				Statements: testStatement,
			}

			pol, err := cl.UpdatePolicy(ctx, &req)

			require.NoError(t, err)
			assert.Equal(t, "newTestPolicy1", pol.Name)
			assert.Equal(t, []string{"team:local:ladies"}, pol.Members)

			storedPol = getPolicyFromStore(t, store, storedPol.ID)
			assertPoliciesMatch(t, &storedPol, pol)
		}},
		{"fails with InvalidArgument when policy statement omits projects", func(t *testing.T) {
			_, items := addSomePoliciesToStore(t, store, prng)
			statement0 := api_v2.Statement{
				Effect:    api_v2.Statement_ALLOW,
				Resources: []string{"cfgmgmt:delete", "cfgmgmt:list"},
				Actions:   []string{"cfgmgmt:nodes:*"},
			}
			req := api_v2.UpdatePolicyReq{
				Id:         "policy1",
				Name:       "my favorite policy",
				Members:    []string{"team:local:admins", "user:local:alice"},
				Statements: []*api_v2.Statement{&statement0},
			}

			resp, err := cl.UpdatePolicy(ctx, &req)

			require.Nil(t, resp)
			require.Equal(t, len(items), store.ItemCount())
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Regexp(t, "statement.*must include projects", err.Error())
		}},

		{"successfully updates policy with assorted projects", func(t *testing.T) {
			storedPol, _ := addSomePoliciesToStore(t, store, prng)
			statement0 := api_v2.Statement{
				Effect:    api_v2.Statement_ALLOW,
				Resources: []string{"cfgmgmt:delete", "cfgmgmt:list"},
				Actions:   []string{"cfgmgmt:nodes:*"},
				Projects:  []string{constants_v2.UnassignedProjectID},
			}
			statement1 := api_v2.Statement{
				Effect:    api_v2.Statement_DENY,
				Resources: []string{"cfgmgmt:delete", "cfgmgmt:list"},
				Actions:   []string{"cfgmgmt:nodes:*"},
				Projects:  []string{constants_v2.AllProjectsExternalID},
			}
			statement2 := api_v2.Statement{
				Effect:   api_v2.Statement_ALLOW,
				Role:     "my-other-role",
				Projects: []string{"my-project", "another-project"},
			}

			req := api_v2.UpdatePolicyReq{
				Id:         storedPol.ID,
				Name:       "TestPolicy1",
				Statements: []*api_v2.Statement{&statement0, &statement1, &statement2},
			}

			pol, err := cl.UpdatePolicy(ctx, &req)

			require.NoError(t, err)

			storedPol = getPolicyFromStore(t, store, storedPol.ID)
			assertPoliciesMatch(t, &storedPol, pol)
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		store.Flush()
	}
}

func TestReplacePolicyMembers(t *testing.T) {
	ctx := context.Background()
	prng := prng.Seed(t)
	ts := setupV2p1WithWriter(t, dummyWriter)
	cl := ts.policy
	store := ts.policyCache
	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"successfully replaces member list with new member list", func(t *testing.T) {
			storedPol, _ := addSomePoliciesToStore(t, store, prng)
			members := []string{
				"user:local:name1",
				"user:local:name2",
				"user:local:name3",
			}
			req := api_v2.ReplacePolicyMembersReq{
				Id:      storedPol.ID,
				Members: members,
			}

			pol, err := cl.ReplacePolicyMembers(ctx, &req)

			require.NoError(t, err)
			assert.Equal(t, len(members), len(pol.Members))
			assert.Equal(t, members, pol.Members)
		}},
		{"fails with InvalidArgument when empty ID provided", func(t *testing.T) {
			addSomePoliciesToStore(t, store, prng)
			req := api_v2.ReplacePolicyMembersReq{
				Id:      "",
				Members: []string{"user:local:n1", "user:local:n2"},
			}

			pol, err := cl.ReplacePolicyMembers(ctx, &req)

			require.Nil(t, pol)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Regexp(t, "invalid ReplacePolicyMembersReq.Id: value does not match regex pattern", err.Error())
		}},
		{"fails with NotFound when policy not found", func(t *testing.T) {
			addSomePoliciesToStore(t, store, prng)
			req := api_v2.ReplacePolicyMembersReq{
				Id:      "this-is-not-found",
				Members: []string{"user:local:n1", "user:local:n2"},
			}

			pol, err := cl.ReplacePolicyMembers(ctx, &req)

			require.Nil(t, pol)
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Regexp(t, "no policy.*found", err.Error())
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		store.Flush()
	}
}

func TestRemovePolicyMembers(t *testing.T) {
	ctx := context.Background()
	prng := prng.Seed(t)
	ts := setupV2p1WithWriter(t, dummyWriter)
	cl := ts.policy
	store := ts.policyCache
	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"successfully removes some members from policy with members ignoring non-members", func(t *testing.T) {
			storedPol, _ := addSomePoliciesToStore(t, store, prng)
			originalMembers := []string{
				"user:local:name1",
				"user:local:name2",
				"user:local:name3",
				"user:local:name4",
			}
			setupReq := api_v2.ReplacePolicyMembersReq{
				Id:      storedPol.ID,
				Members: originalMembers,
			}
			_, err := cl.ReplacePolicyMembers(ctx, &setupReq)
			require.NoError(t, err)

			wrongMemberCount := 2
			removeMembers := []string{
				"user:local:name1",
				"user:local:wrong1",
				"user:local:name3",
				"user:local:wrong2",
			}
			req := api_v2.RemovePolicyMembersReq{
				Id:      storedPol.ID,
				Members: removeMembers,
			}
			resp, err := cl.RemovePolicyMembers(ctx, &req)

			require.NoError(t, err)
			assert.Equal(t, len(originalMembers)-len(removeMembers)+wrongMemberCount, len(resp.Members))
			assert.ElementsMatch(t, []string{"user:local:name2", "user:local:name4"}, resp.Members)
		}},
		{"successfully removes all members from policy", func(t *testing.T) {
			storedPol, _ := addSomePoliciesToStore(t, store, prng)
			originalMembers := []string{
				"user:local:name1",
				"user:local:name2",
				"user:local:name3",
				"user:local:name4",
			}
			setupReq := api_v2.ReplacePolicyMembersReq{
				Id:      storedPol.ID,
				Members: originalMembers,
			}
			_, err := cl.ReplacePolicyMembers(ctx, &setupReq)
			require.NoError(t, err)

			wrongMemberCount := 2
			removeMembers := []string{
				"user:local:name1",
				"user:local:name2",
				"user:local:name3",
				"user:local:name4",
				"user:local:wrong",
				"user:local:wrong2",
			}
			req := api_v2.RemovePolicyMembersReq{
				Id:      storedPol.ID,
				Members: removeMembers,
			}
			resp, err := cl.RemovePolicyMembers(ctx, &req)

			require.NoError(t, err)
			assert.Equal(t, len(originalMembers)-len(removeMembers)+wrongMemberCount, len(resp.Members))
			assert.Empty(t, resp.Members)
		}},
		{"fails with InvalidArgument when empty ID provided", func(t *testing.T) {
			addSomePoliciesToStore(t, store, prng)
			req := api_v2.RemovePolicyMembersReq{
				Id:      "",
				Members: []string{"user:local:n1", "user:local:n2"},
			}
			pol, err := cl.RemovePolicyMembers(ctx, &req)
			require.Nil(t, pol)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Regexp(t, "invalid RemovePolicyMembersReq.Id: value does not match regex pattern", err.Error())
		}},
		{"fails with NotFound when policy not found", func(t *testing.T) {
			addSomePoliciesToStore(t, store, prng)
			req := api_v2.RemovePolicyMembersReq{
				Id:      "this-is-not-found",
				Members: []string{"user:local:n1", "user:local:n2"},
			}

			pol, err := cl.RemovePolicyMembers(ctx, &req)

			require.Nil(t, pol)
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Regexp(t, "no policy.*found", err.Error())
		}},
		{"fails when one of the members is the admin", func(t *testing.T) {
			// this policy acts as a stand-in for the default admin policy
			adminPolicyID := constants_v2.AdminPolicyID
			adminPolicy := genPolicy(t, adminPolicyID, prng)
			store.Add(adminPolicyID, &adminPolicy, cache.NoExpiration)

			originalMembers := []string{
				"team:local:admins",
				"user:local:somebody",
				"user:local:somebodyelse",
			}
			setupReq := api_v2.ReplacePolicyMembersReq{
				Id:      adminPolicyID,
				Members: originalMembers,
			}
			_, err := cl.ReplacePolicyMembers(ctx, &setupReq)
			require.NoError(t, err)

			req := api_v2.RemovePolicyMembersReq{
				Id:      adminPolicyID,
				Members: []string{"user:local:somebody", "team:local:admins"},
			}

			pol, err := cl.RemovePolicyMembers(ctx, &req)

			require.Nil(t, pol)
			grpctest.AssertCode(t, codes.PermissionDenied, err)
			assert.Regexp(t, "cannot remove", err.Error())

			polFromStore := getPolicyFromStore(t, store, adminPolicyID)
			for _, member := range polFromStore.Members {
				assert.Contains(t, originalMembers, member.Name)
			}
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		store.Flush()
	}
}

func TestAddPolicyMembers(t *testing.T) {
	ctx := context.Background()
	prng := prng.Seed(t)
	ts := setupV2p1WithWriter(t, dummyWriter)
	cl := ts.policy
	store := ts.policyCache
	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"successfully adds one member to a policy", func(t *testing.T) {
			pol, _ := addSomePoliciesToStore(t, store, prng)
			newMember := []string{
				"user:local:name1",
			}
			setupReq := api_v2.AddPolicyMembersReq{
				Id:      pol.ID,
				Members: newMember,
			}
			members := append(storage.MemberSliceToStringSlice(pol.Members), newMember...)

			resp, err := cl.AddPolicyMembers(ctx, &setupReq)

			require.NoError(t, err)
			assert.NotEmpty(t, resp.Members)
			assert.ElementsMatch(t, members, resp.Members)
			polFromStore := getPolicyFromStore(t, store, pol.ID)
			assert.ElementsMatch(t, storage.MemberSliceToStringSlice(polFromStore.Members), resp.Members)
		}},
		{
			"successfully adds several members to a policy", func(t *testing.T) {
				pol, _ := addSomePoliciesToStore(t, store, prng)
				newMembers := []string{
					"user:local:name1",
					"user:local:name2",
					"user:local:name3",
					"user:local:name4",
				}
				members := append(storage.MemberSliceToStringSlice(pol.Members), newMembers...)
				setupReq := api_v2.AddPolicyMembersReq{
					Id:      pol.ID,
					Members: members,
				}

				resp, err := cl.AddPolicyMembers(ctx, &setupReq)

				require.NoError(t, err)
				assert.NotEmpty(t, resp.Members)
				assert.ElementsMatch(t, members, resp.Members)
				polFromStore := getPolicyFromStore(t, store, pol.ID)
				assert.ElementsMatch(t, storage.MemberSliceToStringSlice(polFromStore.Members), resp.Members)
			}},
		{"fails to add duplicate member when member has already been added", func(t *testing.T) {
			pol, _ := addSomePoliciesToStore(t, store, prng)
			dupMember := pol.Members[0]
			members := storage.MemberSliceToStringSlice(pol.Members)
			setupReq := api_v2.AddPolicyMembersReq{
				Id:      pol.ID,
				Members: []string{dupMember.Name},
			}

			resp, err := cl.AddPolicyMembers(ctx, &setupReq)

			require.NoError(t, err)
			assert.NotEmpty(t, resp.Members)
			assert.ElementsMatch(t, members, resp.Members)
			polFromStore := getPolicyFromStore(t, store, pol.ID)
			assert.ElementsMatch(t, storage.MemberSliceToStringSlice(polFromStore.Members), resp.Members)
		}},
		{"fails with InvalidArgument when empty ID provided", func(t *testing.T) {
			addSomePoliciesToStore(t, store, prng)
			req := api_v2.AddPolicyMembersReq{
				Id:      "",
				Members: []string{"user:local:name1", "user:local:name2"},
			}

			resp, err := cl.AddPolicyMembers(ctx, &req)

			require.Nil(t, resp)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Regexp(t, "invalid AddPolicyMembersReq.Id: value does not match regex pattern", err.Error())
		}},
		{"fails with NotFound when policy not found", func(t *testing.T) {
			addSomePoliciesToStore(t, store, prng)
			req := api_v2.AddPolicyMembersReq{
				Id:      "this-is-not-found",
				Members: []string{"team:saml:name1", "user:ldap:name2"},
			}

			pol, err := cl.AddPolicyMembers(ctx, &req)

			require.Nil(t, pol)
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Regexp(t, "no policy.*found", err.Error())
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		store.Flush()
	}
}

func TestCreateRole(t *testing.T) {
	ctx := context.Background()
	ts := setupV2p1WithWriter(t, dummyWriter)
	cl := ts.policy
	store := ts.roleCache

	project1 := api_v2.CreateProjectReq{
		Id:   "project-1",
		Name: "name1",
	}
	_, err := ts.projects.CreateProject(ctx, &project1)
	require.NoError(t, err)

	project2 := api_v2.CreateProjectReq{
		Id:   "project-2",
		Name: "name2",
	}
	_, err = ts.projects.CreateProject(ctx, &project2)
	require.NoError(t, err)

	cases := map[string]func(*testing.T){
		"successfully adds role--handler": func(t *testing.T) {
			req := api_v2.CreateRoleReq{
				Id:       "role-id",
				Name:     "roleName",
				Actions:  []string{"infra:some:action", "infra:some:other"},
				Projects: []string{},
			}

			role, err := cl.CreateRole(ctx, &req)
			require.NoError(t, err)

			assert.Equal(t, req.Id, role.Id)
			assert.Equal(t, req.Name, role.Name)
			assert.Equal(t, "CUSTOM", role.Type.String())
			assert.ElementsMatch(t, req.Actions, role.Actions)
			assert.ElementsMatch(t, req.Projects, role.Projects)
		},
		"successfully adds role--store": func(t *testing.T) {
			req := api_v2.CreateRoleReq{
				Id:       "role-id",
				Name:     "roleName",
				Actions:  []string{"infra:some:action", "infra:some:other"},
				Projects: []string{},
			}
			require.Equal(t, 0, store.ItemCount())

			role, err := cl.CreateRole(ctx, &req)
			require.NoError(t, err)

			assert.Equal(t, 1, store.ItemCount())
			storedRole := getRoleFromStore(t, store, role.Id)
			assertRolesMatch(t, storedRole, *role)
		},
		"successfully adds role with all suppported characters": func(t *testing.T) {
			req := api_v2.CreateRoleReq{
				Id:       "-role-1234",
				Name:     "roleName",
				Actions:  []string{"infra:some:action", "infra:some:other"},
				Projects: []string{},
			}

			resp, err := cl.CreateRole(ctx, &req)
			require.NoError(t, err)

			assert.Equal(t, req.Name, resp.Name)
			assert.Equal(t, "CUSTOM", resp.Type.String())
			assert.ElementsMatch(t, req.Actions, resp.Actions)
			assert.ElementsMatch(t, req.Projects, resp.Projects)
		},
		"successfully adds role with a single project": func(t *testing.T) {
			req := api_v2.CreateRoleReq{
				Id:       "-role-1234",
				Name:     "roleName",
				Actions:  []string{"infra:some:action", "infra:some:other"},
				Projects: []string{project1.Id},
			}

			resp, err := cl.CreateRole(ctx, &req)
			require.NoError(t, err)

			assert.Equal(t, req.Name, resp.Name)
			assert.Equal(t, "CUSTOM", resp.Type.String())
			assert.ElementsMatch(t, req.Actions, resp.Actions)
			assert.ElementsMatch(t, req.Projects, resp.Projects)
		},
		"successfully adds role with multiple projects": func(t *testing.T) {
			req := api_v2.CreateRoleReq{
				Id:       "-role-1234",
				Name:     "roleName",
				Actions:  []string{"infra:some:action", "infra:some:other"},
				Projects: []string{project1.Id, project2.Id},
			}

			resp, err := cl.CreateRole(ctx, &req)
			require.NoError(t, err)

			assert.Equal(t, req.Name, resp.Name)
			assert.Equal(t, "CUSTOM", resp.Type.String())
			assert.ElementsMatch(t, req.Actions, resp.Actions)
			assert.ElementsMatch(t, req.Projects, resp.Projects)
		},
		"fails with InvalidArgument with empty action": func(t *testing.T) {
			req := api_v2.CreateRoleReq{
				Id:       "role-id",
				Name:     "roleName",
				Actions:  []string{"infra:some:action", ""},
				Projects: []string{},
			}

			resp, err := cl.CreateRole(ctx, &req)
			require.Nil(t, resp)

			assert.Equal(t, 0, store.ItemCount())
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Regexp(t, "invalid.*Actions.*does not match", err.Error())
		},
		"fails with InvalidArgument with invalid actions": func(t *testing.T) {
			req := api_v2.CreateRoleReq{
				Id:       "role-id",
				Name:     "roleName",
				Actions:  []string{"action1", "action2"},
				Projects: []string{},
			}

			resp, err := cl.CreateRole(ctx, &req)

			require.Nil(t, resp)

			assert.Equal(t, 0, store.ItemCount())
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Regexp(t, "invalid.*Actions.*does not match", err.Error())
		},
		"fails with InvalidArgument with no actions": func(t *testing.T) {
			req := api_v2.CreateRoleReq{
				Id:       "role-id",
				Name:     "roleName",
				Actions:  []string{},
				Projects: []string{},
			}

			resp, err := cl.CreateRole(ctx, &req)
			require.Nil(t, resp)

			assert.Equal(t, 0, store.ItemCount())
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Regexp(t, "role-id.*role must contain at least one action", err.Error())
		},
		"fails with InvalidArgument with no id": func(t *testing.T) {
			req := api_v2.CreateRoleReq{
				Name:     "roleName",
				Actions:  []string{"infra:some:action"},
				Projects: []string{},
			}

			resp, err := cl.CreateRole(ctx, &req)
			require.Nil(t, resp)

			assert.Equal(t, 0, store.ItemCount())
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Regexp(t, "invalid.*Id.*does not match", err.Error())
		},
		"fails with InvalidArgument with blank id": func(t *testing.T) {
			req := api_v2.CreateRoleReq{
				Id:       "   ",
				Name:     "roleName",
				Actions:  []string{"infra:some:action"},
				Projects: []string{},
			}

			resp, err := cl.CreateRole(ctx, &req)
			require.Nil(t, resp)

			assert.Equal(t, 0, store.ItemCount())
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Regexp(t, "invalid.*Id.*does not match", err.Error())
		},
		"fails with InvalidArgument with invalid characters in id": func(t *testing.T) {
			req := api_v2.CreateRoleReq{
				Id:       "roleID", // only lowercase allowed
				Name:     "roleName",
				Actions:  []string{"infra:some:action"},
				Projects: []string{},
			}

			resp, err := cl.CreateRole(ctx, &req)
			require.Nil(t, resp)

			assert.Equal(t, 0, store.ItemCount())
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Regexp(t, "invalid.*Id.*does not match", err.Error())
		},
		"successfully adds role with 64-char id": func(t *testing.T) {
			req := api_v2.CreateRoleReq{
				Id:       "abcd-123456789-123456789-123456789-123456789-123456789-123456789",
				Name:     "roleName",
				Actions:  []string{"infra:some:action"},
				Projects: []string{},
			}

			role, err := cl.CreateRole(ctx, &req)
			require.NoError(t, err)

			assert.Equal(t, 1, store.ItemCount())
			storedRole := getRoleFromStore(t, store, role.Id)
			assertRolesMatch(t, storedRole, *role)
		},
		"fails to add role with 1 char more than 64-char limit": func(t *testing.T) {
			req := api_v2.CreateRoleReq{
				Id:       "0abcd-123456789-123456789-123456789-123456789-123456789-123456789",
				Name:     "roleName",
				Actions:  []string{"infra:some:action"},
				Projects: []string{},
			}

			resp, err := cl.CreateRole(ctx, &req)
			require.Nil(t, resp)

			assert.Equal(t, 0, store.ItemCount())
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Regexp(t, "invalid.*Id.*does not match", err.Error())
		},
		"fails when attempting to violate ID uniqueness": func(t *testing.T) {
			storedRole := storage.Role{
				ID:       "role-id",
				Name:     "roleName",
				Actions:  []string{"foo:bar:baz"},
				Type:     storage.Custom,
				Projects: []string{},
			}
			store.Add(storedRole.ID, &storedRole, cache.NoExpiration)
			req := api_v2.CreateRoleReq{
				Id:       "role-id",
				Name:     "otherName",
				Actions:  []string{"infra:some:action"},
				Projects: []string{},
			}

			resp, err := cl.CreateRole(ctx, &req)
			require.Nil(t, resp)

			assert.Equal(t, 1, store.ItemCount())
			grpctest.AssertCode(t, codes.AlreadyExists, err)
			assert.Regexp(t, "role-id.*already exists", err.Error())
		},
	}

	for desc, test := range cases {
		t.Run(desc, test)
		store.Flush()
	}
}

func TestUpdateRole(t *testing.T) {
	ctx := context.Background()
	prng := prng.Seed(t)
	ts := setupV2p1WithWriter(t, dummyWriter)
	cl := ts.policy
	store := ts.roleCache
	existingRoleId := "test-role"

	cases := map[string]func(*testing.T){
		"fails with InvalidArgument when missing role id": func(t *testing.T) {
			req := api_v2.UpdateRoleReq{
				Name:     "name",
				Actions:  []string{"foo:bar:baz"},
				Projects: []string{},
			}

			role, err := cl.UpdateRole(ctx, &req)

			require.Nil(t, role)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		},
		"fails with InvalidArgument when ID is invalid": func(t *testing.T) {
			req := api_v2.UpdateRoleReq{
				Id:       "no spaces",
				Name:     "name",
				Actions:  []string{"foo:bar:baz"},
				Projects: []string{},
			}

			role, err := cl.UpdateRole(ctx, &req)

			require.Nil(t, role)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		},
		"fails with InvalidArgument when missing role name": func(t *testing.T) {
			req := api_v2.UpdateRoleReq{
				Id:       existingRoleId,
				Actions:  []string{"foo:bar:baz"},
				Projects: []string{},
			}

			role, err := cl.UpdateRole(ctx, &req)

			require.Nil(t, role)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		},
		"fails with InvalidArgument when missing role actions": func(t *testing.T) {
			req := api_v2.UpdateRoleReq{
				Id:       existingRoleId,
				Name:     "name",
				Projects: []string{},
			}

			role, err := cl.UpdateRole(ctx, &req)

			require.Nil(t, role)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		},
		"fails with InvalidArgument when new role name is whitespace": func(t *testing.T) {
			req := api_v2.UpdateRoleReq{
				Id:       existingRoleId,
				Name:     " ",
				Actions:  []string{"foo:bar:baz"},
				Projects: []string{},
			}

			role, err := cl.UpdateRole(ctx, &req)

			require.Nil(t, role)
			grpctest.AssertCode(t, codes.InvalidArgument, err)
		},
		"fails with NotFound when ID doesn't match any roles and there's no roles in store": func(t *testing.T) {
			req := api_v2.UpdateRoleReq{
				Id:       "fake-role-id",
				Name:     "name",
				Actions:  []string{"foo:bar:baz"},
				Projects: []string{},
			}

			role, err := cl.UpdateRole(ctx, &req)

			require.Nil(t, role)
			grpctest.AssertCode(t, codes.NotFound, err)
		},
		"fails with NotFound when ID doesn't match any roles and there's many roles in store": func(t *testing.T) {
			addSomeRolesToStore(t, store, prng)
			req := api_v2.UpdateRoleReq{
				Id:       "fake-role-id",
				Name:     "name",
				Actions:  []string{"foo:bar:baz"},
				Projects: []string{},
			}

			role, err := cl.UpdateRole(ctx, &req)

			require.Nil(t, role)
			grpctest.AssertCode(t, codes.NotFound, err)
		},
		"successfully updates role name": func(t *testing.T) {
			existingRole := genRole(t, existingRoleId, prng)
			store.Add(existingRole.ID, &existingRole, cache.NoExpiration)

			req := api_v2.UpdateRoleReq{
				Id:       existingRoleId,
				Name:     "new name",
				Actions:  existingRole.Actions,
				Projects: existingRole.Projects,
			}

			role, err := cl.UpdateRole(ctx, &req)

			require.NoError(t, err)
			assert.Equal(t, "new name", role.Name)
			nameInStore := getRoleFromStore(t, store, existingRole.ID).Name
			assert.Equal(t, role.Name, nameInStore)
			existingRole = getRoleFromStore(t, store, existingRole.ID)
			assertRolesMatch(t, existingRole, *role)
		},
		"successfully updates role actions": func(t *testing.T) {
			existingRole := genRole(t, existingRoleId, prng)
			store.Add(existingRole.ID, &existingRole, cache.NoExpiration)

			req := api_v2.UpdateRoleReq{
				Id:       existingRoleId,
				Name:     existingRole.Name,
				Actions:  []string{"foo:bar:qux"},
				Projects: existingRole.Projects,
			}

			role, err := cl.UpdateRole(ctx, &req)

			require.NoError(t, err)
			assert.Equal(t, []string{"foo:bar:qux"}, role.Actions)
			actionsInStore := getRoleFromStore(t, store, existingRole.ID).Actions
			assert.Equal(t, role.Actions, actionsInStore)
			existingRole = getRoleFromStore(t, store, existingRole.ID)
			assertRolesMatch(t, existingRole, *role)
		},
		"successfully runs even if nothing is actually changed": func(t *testing.T) {
			existingRole := genRole(t, existingRoleId, prng)
			store.Add(existingRole.ID, &existingRole, cache.NoExpiration)

			req := api_v2.UpdateRoleReq{
				Id:       existingRoleId,
				Name:     existingRole.Name,
				Actions:  existingRole.Actions,
				Projects: existingRole.Projects,
			}

			role, err := cl.UpdateRole(ctx, &req)

			require.NoError(t, err)
			assert.Equal(t, req.Id, role.Id)
			assert.Equal(t, req.Name, role.Name)
			assert.Equal(t, req.Actions, role.Actions)

			roleInStore := getRoleFromStore(t, store, existingRole.ID)
			assert.Equal(t, role.Id, roleInStore.ID)
			assert.Equal(t, role.Name, roleInStore.Name)
			assert.Equal(t, role.Actions, roleInStore.Actions)

			existingRole = getRoleFromStore(t, store, existingRole.ID)
			assertRolesMatch(t, existingRole, *role)
		},
		"successfully updates multiple properties at once": func(t *testing.T) {
			existingRole := genRole(t, existingRoleId, prng)
			store.Add(existingRole.ID, &existingRole, cache.NoExpiration)

			req := api_v2.UpdateRoleReq{
				Id:       existingRoleId,
				Name:     "new name",
				Actions:  []string{"foo:bar:qux"},
				Projects: []string{"project-1"},
			}

			role, err := cl.UpdateRole(ctx, &req)

			require.NoError(t, err)
			assert.Equal(t, "new name", role.Name)
			nameInStore := getRoleFromStore(t, store, existingRole.ID).Name
			assert.Equal(t, role.Name, nameInStore)

			require.NoError(t, err)
			assert.Equal(t, []string{"foo:bar:qux"}, role.Actions)
			actionsInStore := getRoleFromStore(t, store, existingRole.ID).Actions
			assert.Equal(t, role.Actions, actionsInStore)

			require.NoError(t, err)
			assert.Equal(t, []string{"project-1"}, role.Projects)
			projectsInStore := getRoleFromStore(t, store, existingRole.ID).Projects
			assert.Equal(t, role.Projects, projectsInStore)

			existingRole = getRoleFromStore(t, store, existingRole.ID)
			assertRolesMatch(t, existingRole, *role)
		},
	}

	for desc, test := range cases {
		t.Run(desc, test)
		store.Flush()
	}
}

func TestDeleteRole(t *testing.T) {
	ctx := context.Background()
	prng := prng.Seed(t)
	ts := setupV2p1WithWriter(t, dummyWriter)
	cl := ts.policy
	store := ts.roleCache
	cases := map[string]func(*testing.T){
		"fails with NotFound when deleting from empty store": func(t *testing.T) {
			require.Zero(t, store.ItemCount())
			req := api_v2.DeleteRoleReq{
				Id: "test-role-1",
			}

			_, err := cl.DeleteRole(ctx, &req)

			grpctest.AssertCode(t, codes.NotFound, err)
		},
		"fails with InvalidArgument when ID is not permissable": func(t *testing.T) {
			// "*" is not a permitted role name per the regex validation
			req := api_v2.DeleteRoleReq{
				Id: "*",
			}

			_, err := cl.DeleteRole(ctx, &req)

			grpctest.AssertCode(t, codes.InvalidArgument, err)
		},
		"deletes role from store containing a single role": func(t *testing.T) {
			require.Zero(t, store.ItemCount())
			role := addArbitraryRoleToStore(t, store, prng)

			req := api_v2.DeleteRoleReq{
				Id: role.ID,
			}

			_, err := cl.DeleteRole(ctx, &req)

			assert.NoError(t, err)
			assert.Zero(t, store.ItemCount())
		},
		"deletes role from store containing several roles": func(t *testing.T) {
			role, roles := addSomeRolesToStore(t, store, prng)

			req := api_v2.DeleteRoleReq{
				Id: role.ID,
			}

			_, err := cl.DeleteRole(ctx, &req)

			assert.NoError(t, err)
			require.Equal(t, len(roles)-1, store.ItemCount())

			for r := range roles {
				assert.NotEqual(t, role, r)
			}
		},
	}

	for desc, test := range cases {
		t.Run(desc, test)
		store.Flush()
	}
}
func TestListRoles(t *testing.T) {
	ctx := context.Background()
	prng := prng.Seed(t)
	req := api_v2.ListRolesReq{} // it's not changing, can be reused
	ts := setupV2p1WithWriter(t, dummyWriter)
	cl := ts.policy
	store := ts.roleCache
	cases := map[string]func(*testing.T){

		"returns empty list when there aren't any roles in the store": func(t *testing.T) {
			require.Empty(t, store.Items())

			resp, err := cl.ListRoles(ctx, &req)

			require.NoError(t, err)
			assert.Empty(t, resp.Roles)
		},

		"returns list with appropriate role when there's one role in the store": func(t *testing.T) {
			storedRole := addArbitraryRoleToStore(t, store, prng)

			resp, err := cl.ListRoles(ctx, &req)
			require.NoError(t, err)

			require.Equal(t, 1, len(resp.Roles))
			role := resp.Roles[0]
			assert.Equal(t, storedRole.ID, role.Id)
			assert.Equal(t, storedRole.Name, role.Name)
			assert.Equal(t, "CUSTOM", role.Type.String())
			assert.ElementsMatch(t, storedRole.Actions, role.Actions)
		},
		"returns list with appropriate roles when there's several roles in the store": func(t *testing.T) {
			storedRoles := addArbitraryRolesToStore(t, store, prng, 3)

			expectedRoles := make(map[string]storage.Role)
			expectedRoles[storedRoles[0].ID] = storedRoles[0]
			expectedRoles[storedRoles[1].ID] = storedRoles[1]
			expectedRoles[storedRoles[2].ID] = storedRoles[2]

			resp, err := cl.ListRoles(ctx, &req)
			require.NoError(t, err)

			require.Equal(t, len(expectedRoles), len(resp.Roles))
			for _, role := range resp.Roles {
				storedRole, found := expectedRoles[role.Id]
				require.True(t, found)

				assert.Equal(t, storedRole.ID, role.Id)
				assert.Equal(t, storedRole.Name, role.Name)
				assert.Equal(t, "CUSTOM", role.Type.String())
				assert.ElementsMatch(t, storedRole.Actions, role.Actions)
			}
		},
	}

	for desc, test := range cases {
		t.Run(desc, test)
		store.Flush()
	}
}

func TestGetRole(t *testing.T) {
	ctx := context.Background()
	prng := prng.Seed(t)
	ts := setupV2p1WithWriter(t, dummyWriter)
	cl := ts.policy
	store := ts.roleCache
	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"successfully finds role with a single item in store", func(t *testing.T) {
			require.Zero(t, store.ItemCount())
			storedRole := addArbitraryRoleToStore(t, store, prng)
			req := api_v2.GetRoleReq{
				Id: storedRole.ID,
			}

			role, err := cl.GetRole(ctx, &req)
			require.NoError(t, err)

			assert.Equal(t, storedRole.ID, role.Id)
			assert.Equal(t, storedRole.Name, role.Name)
			assert.Equal(t, "CUSTOM", role.Type.String())
			assert.ElementsMatch(t, storedRole.Actions, role.Actions)
		}},
		{"successfully finds role with multiple items in store", func(t *testing.T) {
			storedRole, _ := addSomeRolesToStore(t, store, prng)
			req := api_v2.GetRoleReq{
				Id: storedRole.ID,
			}

			role, err := cl.GetRole(ctx, &req)
			require.NoError(t, err)

			assert.Equal(t, storedRole.ID, role.Id)
			assert.Equal(t, storedRole.Name, role.Name)
			assert.Equal(t, "CUSTOM", role.Type.String())
			assert.ElementsMatch(t, storedRole.Actions, role.Actions)
		}},
		{"fails with NotFound when ID doesn't match any roles", func(t *testing.T) {
			addSomeRolesToStore(t, store, prng)
			nonExistentId := "fake-id"
			req := api_v2.GetRoleReq{
				Id: nonExistentId,
			}

			role, err := cl.GetRole(ctx, &req)

			require.Nil(t, role)
			grpctest.AssertCode(t, codes.NotFound, err)
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		store.Flush()
	}
}

type v1Lister struct {
	pols []*storage_v1.Policy
	err  error
}

func (pl *v1Lister) ListPolicies(context.Context) ([]*storage_v1.Policy, error) {
	return pl.pols, pl.err
}

func (pl *v1Lister) ListPoliciesWithSubjects(context.Context) ([]*storage_v1.Policy, error) {
	pols := []*storage_v1.Policy{}
	for _, p := range pl.pols {
		if len(p.Subjects) > 0 {
			pols = append(pols, p)
		}
	}
	return pols, pl.err
}

func TestTypeConversion(t *testing.T) {
	cases := map[string]storage.Type{
		"custom":       storage.Custom,
		"chef-managed": storage.ChefManaged,
	}
	for desc, expected := range cases {
		t.Run("successfully converts "+desc, func(t *testing.T) {
			newType, err := storage.NewType(desc)
			require.NoError(t, err)
			assert.Equal(t, expected, newType)
		})
	}
	t.Run("reports failure when attempting to convert illegal value", func(t *testing.T) {
		_, err := storage.NewType("something else")
		assert.Regexp(t, "policy type.*something else", err.Error())
	})
}

func TestAuthzGRPCInteractionWithTestEngineStore(t *testing.T) {
	singleStatementAllow := &api_v2.CreatePolicyReq{
		Id:      "single-statement-allow",
		Name:    "singleStatementAllow",
		Members: []string{},
		Statements: []*api_v2.Statement{
			{
				Effect:    api_v2.Statement_ALLOW,
				Resources: []string{"some-resource", "some-other-resource"},
				Actions:   []string{"infra:some:action", "infra:some:other"},
				Projects:  []string{"project1"},
			},
		},
		Projects: []string{"project1"},
	}

	multiStatementAllow := &api_v2.CreatePolicyReq{
		Id:      "multi-statement-allow",
		Name:    "multiStatementAllow",
		Members: []string{},
		Statements: []*api_v2.Statement{
			{
				Effect:    api_v2.Statement_ALLOW,
				Resources: []string{"someResource", "someOtherResource"},
				Actions:   []string{"infra:some:action", "infra:some:other"},
				Projects:  []string{"project1"},
			},
			{
				Effect:    api_v2.Statement_DENY,
				Resources: []string{"someResource2", "someOtherResource"},
				Actions:   []string{"infra:some:action"},
				Role:      "",
				Projects:  []string{"project1"},
			},
			{
				Effect:    api_v2.Statement_ALLOW,
				Resources: []string{"someResource3"},
				Actions:   []string{"infra:some:action"},
				Projects:  []string{"project1"},
			},
		},
		Projects: []string{"project1"},
	}

	ctx := context.Background()
	te := &testEngine{}
	ts := setupV2p1WithWriter(t, te)
	cl := ts.policy
	store := ts.policyCache

	t.Run("CreatePolicy updates the engine", func(t *testing.T) {
		tests := map[string][]*api_v2.CreatePolicyReq{
			"add multiple policies": {singleStatementAllow, multiStatementAllow},
			"add a single policy":   {multiStatementAllow},
		}
		for desc, testPolicies := range tests {
			t.Run(desc, func(t *testing.T) {
				require.Zero(t, store.ItemCount())

				// arrange + act: the action we're testing happens implicitly
				generateTestPolicies(ctx, t, cl, testPolicies)
				require.Equal(t, len(testPolicies), store.ItemCount())

				// assert
				// Note: The response and its qualities have been tested above -- here,
				// we're interested in the interaction with the engine's store.
				assert.Equal(t, len(testPolicies)+len(v2.SystemPolicies()), len(te.policyMap),
					"the numbers of both stores should match")
				for _, req := range testPolicies {
					assertInterfaceMapContainsPolicy(t, te.policyMap, req)
				}
				store.Flush()
			})
		}

	})

	t.Run("UpdatePolicy updates the engine", func(t *testing.T) {
		require.Zero(t, store.ItemCount())

		// arrange: create test policy via API then assure that it's in the testEngine
		testCreate := []*api_v2.CreatePolicyReq{
			singleStatementAllow,
		}
		policyResp := generateTestPolicies(ctx, t, cl, testCreate)

		assert.Equal(t, 1+len(v2.SystemPolicies()), len(te.policyMap),
			"the numbers of both stores should match")
		assertInterfaceMapContainsPolicy(t, te.policyMap, testCreate[0])

		// act: update test policy via API then assure that the update is in the testEngine
		singleStatementAllowEdited := &api_v2.UpdatePolicyReq{
			Id:      policyResp[0],
			Name:    "updatedSingleStatementAllow",
			Members: []string{},
			Statements: []*api_v2.Statement{
				{
					Effect:    api_v2.Statement_ALLOW,
					Resources: []string{"update-some-resource", "updated-some-other-resource"},
					Actions:   []string{"infra:some:updatedAction", "infra:some:updatedOther"},
					Projects:  []string{"project1"},
				},
			},
			Projects: []string{"project1"},
		}

		pol, err := cl.UpdatePolicy(ctx, singleStatementAllowEdited)
		assert.NoError(t, err)
		singleStatementAllowEditedFromStore := getPolicyFromStore(t, store, singleStatementAllowEdited.Id)

		// assert: there's still one policy in the engine and it matches the updated one
		assert.Equal(t, 1+len(v2.SystemPolicies()), len(te.policyMap),
			"the numbers of both stores should match")
		assertPoliciesMatch(t, &singleStatementAllowEditedFromStore, pol)

		store.Flush()
	})

	t.Run("DeletePolicy updates the engine", func(t *testing.T) {
		tests := map[string][]*api_v2.CreatePolicyReq{
			"delete multiple policies": {singleStatementAllow, multiStatementAllow},
			"delete a single policy":   {multiStatementAllow},
		}
		for desc, testPolicies := range tests {
			t.Run(desc, func(t *testing.T) {
				require.Zero(t, store.ItemCount())

				// arrange + act: the action we're testing happens implicitly
				policies := generateTestPolicies(ctx, t, cl, testPolicies)
				require.Equal(t, len(testPolicies), store.ItemCount())

				assert.Equal(t, len(testPolicies)+len(v2.SystemPolicies()), len(te.policyMap),
					"the numbers of both stores should match")
				for _, req := range testPolicies {
					assertInterfaceMapContainsPolicy(t, te.policyMap, req)
				}

				for _, policy := range policies {
					req := api_v2.DeletePolicyReq{
						Id: policy,
					}

					_, err := cl.DeletePolicy(ctx, &req)
					require.NoError(t, err)
				}

				// assert store has changed
				assert.Equal(t, len(v2.SystemPolicies()), len(te.policyMap),
					"the numbers of both stores should match")

				for _, policy := range policies {
					_, ok := store.Get(policy)
					assert.False(t, ok)
				}

				store.Flush()
			})
		}
	})
}

func TestPurgeSubjectFromPolicies(t *testing.T) {
	ctx := context.Background()
	prng := prng.Seed(t)
	ts := setupV2p1WithWriter(t, dummyWriter)
	cl := ts.policy
	store := ts.policyCache
	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"successfully removes one member from policies", func(t *testing.T) {
			pol, _ := addSomePoliciesToStore(t, store, prng)
			oldMembers := pol.Members
			req := api_v2.PurgeSubjectFromPoliciesReq{Subject: pol.Members[0].Name}

			resp, err := cl.PurgeSubjectFromPolicies(ctx, &req)
			require.NoError(t, err)
			assert.NotEmpty(t, resp.Ids)

			// Note: since addSomePoliciesToStore COULD set up a situation where more
			// than ONE policy has the subject we've purged as member, we only check for
			// "contains", not "elements match", here
			assert.Contains(t, resp.Ids, pol.ID)

			// check store contents
			polFromStore := getPolicyFromStore(t, store, pol.ID)
			assert.ElementsMatch(t, oldMembers[1:], polFromStore.Members)
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		store.Flush()
	}
}

func getPolicyFromStore(t *testing.T, store *cache.Cache, id string) storage.Policy {
	t.Helper()
	storedPol, ok := store.Get(id)
	require.True(t, ok, "stored in cache")
	pol, ok := storedPol.(*storage.Policy)
	require.True(t, ok, "cannot cast to Policy")
	return *pol
}

func assertProjectsMatch(t *testing.T, storageProject string, apiProject string) {
	if storageProject == constants_v2.AllProjectsID {
		storageProject = constants_v2.AllProjectsExternalID
	}
	assert.Equal(t, storageProject, apiProject, "statement projects differ")
}

func assertStatementsMatch(t *testing.T, storageStatement storage.Statement, apiStatement api_v2.Statement) {
	if storageStatement.Actions != nil && apiStatement.Actions != nil {
		assert.Equal(t, storageStatement.Actions, apiStatement.Actions, "statement actions differ")
	}
	assert.Equal(t, int(storageStatement.Effect), int(apiStatement.Effect), "statement effects differ")
	if storageStatement.Resources != nil && apiStatement.Resources != nil {
		assert.Equal(t, storageStatement.Resources, apiStatement.Resources, "statement resources differ")
	}
	assert.Equal(t, storageStatement.Role, apiStatement.Role, "statement roles differ")
	// This allows for grpc return of []string(nil) being compared to []string{}
	if len(storageStatement.Projects) != 0 || len(apiStatement.Projects) != 0 {
		for i, project := range storageStatement.Projects {
			assertProjectsMatch(t, project, apiStatement.Projects[i])
		}
	}
}

func assertPoliciesMatch(t *testing.T, storagePolicy *storage.Policy, apiPolicy *api_v2.Policy) {
	assert.Equal(t, storagePolicy.Name, apiPolicy.Name, "policy names differ")
	assert.Equal(t, storagePolicy.Type.String(), strings.ToLower(apiPolicy.Type.String()), "policy types differ")
	assert.Equal(t, len(storagePolicy.Members), len(apiPolicy.Members), "number of policy members differ")
	assertMembersMatch(t, storagePolicy.Members, apiPolicy.Members)
	assert.Equal(t, len(storagePolicy.Statements), len(apiPolicy.Statements), "number of policy statements differ")
	for i, statement := range storagePolicy.Statements {
		assertStatementsMatch(t, statement, *apiPolicy.Statements[i])
	}
}

func assertMembersMatch(t *testing.T, storageMembers []storage.Member, apiMembers []string) {
	if storageMembers == nil {
		assert.Nil(t, apiMembers, "policy members differ (nil)")
	} else {
		for i, member := range storageMembers {
			assert.Equal(t, member.Name, apiMembers[i], "policy members differ (slice value)")
		}
	}
}

func getRoleFromStore(t *testing.T, store *cache.Cache, id string) storage.Role {
	t.Helper()
	storedRole, ok := store.Get(id)
	require.True(t, ok, "stored in cache")
	role, ok := storedRole.(*storage.Role)
	require.True(t, ok, "cannot cast to Role")
	return *role
}

func assertRolesMatch(t *testing.T, storageRole storage.Role, apiRole api_v2.Role) {
	assert.Equal(t, storageRole.ID, apiRole.Id, "role names differ")
	assert.Equal(t, storageRole.Name, apiRole.Name, "role names differ")
	assert.Equal(t, storageRole.Type.String(), strings.ToLower(apiRole.Type.String()), "role types differ")
	assert.Equal(t, storageRole.Actions, apiRole.Actions, "role actions differ")
	assert.Equal(t, storageRole.Projects, apiRole.Projects, "role projects differ")
}

type testSetup struct {
	policy       api_v2.PoliciesClient
	authz        api_v2.AuthorizationClient
	projects     api_v2.ProjectsClient
	policyCache  *cache.Cache
	roleCache    *cache.Cache
	projectCache *cache.Cache
}

func setupV2p1WithWriter(t *testing.T,
	writer engine.V2p1Writer) testSetup {
	return setupV2(t, nil, writer, nil)
}

func setupV2(t *testing.T,
	authorizer engine.V2Authorizer,
	writer engine.V2p1Writer,
	pl storage_v1.PoliciesLister) testSetup {

	t.Helper()
	ctx := context.Background()

	l, err := logger.NewLogger("text", "error")
	require.NoError(t, err, "init logger for storage")

	if writer == nil {
		writer = &testEngine{}
	}

	mem_v2 := memstore_v2.New()

	polRefresher, err := v2.NewPolicyRefresher(ctx, l, writer, mem_v2)
	require.NoError(t, err)

	polV2, err := v2.NewPoliciesServer(ctx, l, polRefresher, mem_v2, writer, pl)
	require.NoError(t, err)

	require.NoError(t, err)
	projectsSrv, err := v2.NewProjectsServer(ctx, l, mem_v2,
		testhelpers.NewMockProjectUpdateManager(), testhelpers.NewMockProjectPurger(true), testhelpers.NewMockPolicyRefresher())
	require.NoError(t, err)

	authzV2, err := v2.NewAuthzServer(l, authorizer, projectsSrv, mem_v2)
	require.NoError(t, err)

	serviceCerts := helpers.LoadDevCerts(t, "authz-service")
	connFactory := secureconn.NewFactory(*serviceCerts)

	// TODO(sr): refactor our constructors. Having to maintain the middleware in
	// two places is tedious and error-prone.
	serv := connFactory.NewServer(grpc.UnaryInterceptor(
		grpc_middleware.ChainUnaryServer(
			grpc_server.InputValidationInterceptor(),
			polV2.EngineUpdateInterceptor(),
		),
	))

	api_v2.RegisterPoliciesServer(serv, polV2)
	api_v2.RegisterAuthorizationServer(serv, authzV2)
	api_v2.RegisterProjectsServer(serv, projectsSrv)
	reflection.Register(serv)

	grpcServ := grpctest.NewServer(serv)

	conn, err := connFactory.Dial("authz-service", grpcServ.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}

	return testSetup{
		policy:       api_v2.NewPoliciesClient(conn),
		authz:        api_v2.NewAuthorizationClient(conn),
		projects:     api_v2.NewProjectsClient(conn),
		policyCache:  mem_v2.PoliciesCache(),
		roleCache:    mem_v2.RolesCache(),
		projectCache: mem_v2.ProjectsCache(),
	}
}

// Mini-factory for policy generation.
// The id is optional; if a zero value is given, a real one will be generated
func genPolicy(t *testing.T, id string, p *prng.Prng) storage.Policy {

	const ( // no special significance to these constant choices
		maxMembers          = 10
		maxStatements       = 25
		maxActions          = 5
		maxResources        = 3
		maxProjects         = 10
		maxPolicyNameLength = 50
	)
	effects := []storage.Effect{storage.Allow, storage.Deny}
	types := []storage.Type{storage.Custom, storage.ChefManaged}
	faker := faker.NewWithSeed(p)

	if id == "" {
		id = faker.Lorem().Word()
	}

	memberCount := rand.Intn(maxMembers)
	var members []storage.Member
	for i := 0; i <= memberCount; i++ {
		memberStr := fmt.Sprintf("%s:%s:%s-%d",
			[]string{"user", "team"}[rand.Intn(1)],
			[]string{"local", "ldap", "saml"}[rand.Intn(2)],
			faker.Lorem().Word(),
			i) // add i, Word() could yield duplicates
		members = append(members, genMember(t, memberStr))
	}

	statementCount := 1 + rand.Intn(maxStatements)
	statements := make([]storage.Statement, statementCount)
	for i := range statements {

		actionCount := 1 + rand.Intn(maxActions-1) // store will have [1, 5] actions
		actions := make([]string, actionCount)
		for i := range actions {
			actions[i] = fmt.Sprintf("%s:%s:%s-%d",
				faker.Lorem().Word(), faker.Lorem().Word(), faker.Lorem().Word(), i)
		}

		resourceCount := rand.Intn(maxResources)
		resources := make([]string, resourceCount)
		for i := range resources {
			resources[i] = fmt.Sprintf("%s:%s:%s-%d",
				faker.Lorem().Word(), faker.Lorem().Word(), faker.Lorem().Word(), i)
		}

		projectCount := 1 + rand.Intn(maxProjects)
		projects := make([]string, projectCount)
		for i := range projects {
			projects[i] = fmt.Sprintf("%s-%d",
				faker.Lorem().Word(), i)
		}

		statements[i] = storage.Statement{
			Actions:   actions,
			Resources: resources,
			Role:      faker.Lorem().Word(),
			Projects:  projects,
			Effect:    effects[rand.Intn(len(effects))],
		}
	}

	return storage.Policy{
		ID:         id,
		Name:       faker.Lorem().Text(maxPolicyNameLength),
		Members:    members,
		Type:       types[rand.Intn(len(types))],
		Statements: statements,
	}
}

func genRole(t *testing.T, id string, p *prng.Prng) storage.Role {
	t.Helper()

	const maxActions = 5

	faker := faker.NewWithSeed(p)

	if id == "" {
		id = faker.Lorem().Word()
	}

	name := faker.Lorem().Word()
	actionCount := 1 + rand.Intn(maxActions-1) // store will have [1, 5] actions
	actions := make([]string, actionCount)
	for i := range actions {
		actions[i] = fmt.Sprintf("%s:%s:%s",
			faker.Lorem().Word(), faker.Lorem().Word(), faker.Lorem().Word())
	}

	role, err := storage.NewRole(id, name, storage.Custom, actions, []string{})
	require.NoError(t, err)
	return *role
}

func genMember(t *testing.T, name string) storage.Member {
	t.Helper()
	member, err := storage.NewMember(name)
	require.NoError(t, err)
	return member
}

func addArbitraryRoleToStore(t *testing.T, store *cache.Cache, p *prng.Prng) storage.Role {
	return addArbitraryRolesToStore(t, store, p, 1)[0]
}

func addArbitraryRolesToStore(t *testing.T, store *cache.Cache, p *prng.Prng, n int) []storage.Role {
	roles := make([]storage.Role, n)
	for i := 0; i < n; i++ {
		role := genRole(t, "", p)
		role.ID = fmt.Sprintf("%s-%d", role.ID, i)
		store.Add(role.ID, &role, cache.NoExpiration)
		roles[i] = role
	}
	return roles
}

func addArbitraryPolicyToStore(t *testing.T, store *cache.Cache, p *prng.Prng) storage.Policy {
	return addArbitraryPoliciesToStore(t, store, p, 1)[0]
}

func addArbitraryPoliciesToStore(t *testing.T, store *cache.Cache, p *prng.Prng, n int) []storage.Policy {
	policies := make([]storage.Policy, n)
	for i := 0; i < n; i++ {
		policyToStore := genPolicy(t, "", p)
		policyToStore.ID = fmt.Sprintf("%s-%d", policyToStore.ID, i)
		// API can only add custom type
		policyToStore.Type = storage.Custom
		store.Add(policyToStore.ID, &policyToStore, cache.NoExpiration)
		policies[i] = policyToStore
	}
	return policies
}

func addSomePoliciesToStore(t *testing.T, store *cache.Cache, p *prng.Prng) (storage.Policy, map[string]storage.Policy) {
	itemCount := 3 + rand.Intn(5) // store will have [3, 8) elements
	policies := make(map[string]storage.Policy, itemCount)
	targetIndex := rand.Intn(itemCount)
	var targetPol storage.Policy
	for i, pol := range addArbitraryPoliciesToStore(t, store, p, itemCount) {
		policies[pol.ID] = pol
		if i == targetIndex {
			targetPol = pol
		}
	}
	return targetPol, policies
}

func addSomeRolesToStore(t *testing.T, store *cache.Cache, p *prng.Prng) (storage.Role, map[string]storage.Role) {
	itemCount := 3 + rand.Intn(5) // store will have [3, 8) elements
	roles := make(map[string]storage.Role, itemCount)
	targetIndex := rand.Intn(itemCount)
	var targetRole storage.Role
	for i, role := range addArbitraryRolesToStore(t, store, p, itemCount) {
		roles[role.ID] = role
		if i == targetIndex {
			targetRole = role
		}
	}
	return targetRole, roles
}

func generateTestPolicies(ctx context.Context, t *testing.T,
	cl api_v2.PoliciesClient, policies []*api_v2.CreatePolicyReq) []string {

	t.Helper()
	policyResponses := make([]string, len(policies))
	for i, req := range policies {
		resp, err := cl.CreatePolicy(ctx, req)
		if assert.NoError(t, err) {
			policyResponses[i] = resp.Id
		}
	}
	return policyResponses
}

func assertInterfaceMapContainsPolicy(t *testing.T,
	data map[string]interface{}, target *api_v2.CreatePolicyReq) bool {

	t.Helper()

	targetDatum := map[string]interface{}{
		"name":       target.Name,
		"members":    target.Members,
		"statements": target.Statements,
	}
	for _, policy := range data {
		if reflect.DeepEqual(policy, targetDatum) {
			return true
		}
	}
	return false
}

type testEngine struct {
	policyMap map[string]interface{}
	roleMap   map[string]interface{}
	ruleMap   map[string][]storage.Rule
}

func (te *testEngine) V2p1SetPolicies(
	ctx context.Context, policies map[string]interface{},
	roles map[string]interface{}) error {
	te.policyMap = policies
	// TODO: use these
	te.roleMap = roles
	return nil
}

func (te *testEngine) SetRules(
	ctx context.Context, rules map[string][]storage.Rule) error {
	te.ruleMap = rules
	return nil
}

func id(t *testing.T, p *prng.Prng) string {
	t.Helper()
	faker := faker.NewWithSeed(p)
	return faker.Lorem().Word() + "-" + faker.Lorem().Word()
}

func genUUID(t *testing.T) uuid.UUID {
	t.Helper()
	i, err := uuid.NewV4()
	require.NoError(t, err)
	return i
}

func wellknown(t *testing.T, wellknownID string) *storage_v1.Policy {
	t.Helper()
	v1DefaultPols, err := storage_v1.DefaultPolicies()
	require.NoError(t, err)
	inputPol, found := v1DefaultPols[wellknownID]
	require.True(t, found)
	return inputPol
}
