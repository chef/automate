package server_test

import (
	"context"
	"fmt"
	"math/rand"
	"strconv"
	"testing"
	"time"

	cache "github.com/patrickmn/go-cache"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"

	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/cereal/postgres"
	"github.com/chef/automate/lib/logger"

	api "github.com/chef/automate/api/interservice/authz"
	constants "github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/authz-service/prng"
	"github.com/chef/automate/components/authz-service/server"
	"github.com/chef/automate/components/authz-service/server/project_purger_workflow"
	"github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/components/authz-service/storage/memstore"
	"github.com/chef/automate/components/authz-service/testhelpers"
	"github.com/chef/automate/lib/authz/project_purge"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/test/helpers"
)

const projectLimitForTesting = 10 // DefaultProjectLimit is higher

func TestUpdateProject(t *testing.T) {
	ctx := context.Background()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"if the project name is empty, returns 'invalid argument'", func(t *testing.T) {
			cl, _, cleanup := setupProjects(t)
			defer cleanup()

			resp, err := cl.UpdateProject(ctx, &api.UpdateProjectReq{Id: "empty-name", Name: ""})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the project id is empty, returns 'invalid argument'", func(t *testing.T) {
			cl, _, cleanup := setupProjects(t)
			defer cleanup()
			resp, err := cl.UpdateProject(ctx, &api.UpdateProjectReq{Id: "", Name: "empty-id"})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the project id is whitespace, returns 'invalid argument'", func(t *testing.T) {
			cl, _, cleanup := setupProjects(t)
			defer cleanup()

			resp, err := cl.UpdateProject(ctx,
				&api.UpdateProjectReq{Id: "    ", Name: "any name"})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the project with that id does not exist, returns 'not found'", func(t *testing.T) {
			cl, _, cleanup := setupProjects(t)
			defer cleanup()

			resp, err := cl.UpdateProject(ctx,
				&api.UpdateProjectReq{Id: "false-project", Name: "my other foo"})
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, resp)
		}},
		{"if the project already exists and update request is valid, project is updated",
			func(t *testing.T) {
				cl, store, cleanup := setupProjects(t)
				defer cleanup()

				id, updatedName := "my-foo", "updated name"
				addProjectToStore(t, store, id, "original name", storage.ChefManaged)

				resp, err := cl.UpdateProject(ctx, &api.UpdateProjectReq{Id: id, Name: updatedName})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.NotNil(t, resp.Project)
				assert.Equal(t, id, resp.Project.GetId())
				assert.Equal(t, updatedName, resp.Project.GetName())
				assert.Equal(t, api.Type_CUSTOM, resp.Project.GetType())
			}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
	}
}

func TestCreateProject(t *testing.T) {
	ctx := context.Background()
	projCl, polCl, testDB, store, _ := testhelpers.SetupProjectsAndRulesWithDB(t)
	defer testDB.CloseDB(t)
	defer store.Close()

	id, name := "my-foo", "my foo"

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"if the project data is invalid, returns 'invalid argument'", func(t *testing.T) {
			resp, err := projCl.CreateProject(ctx, &api.CreateProjectReq{Id: "empty-name", Name: "", SkipPolicies: true})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the project id is empty, returns 'invalid argument'", func(t *testing.T) {
			resp, err := projCl.CreateProject(ctx, &api.CreateProjectReq{Id: "", Name: "empty-id", SkipPolicies: true})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the project id is invalid, returns 'invalid argument'", func(t *testing.T) {
			resp, err := projCl.CreateProject(ctx, &api.CreateProjectReq{Id: "no spaces", Name: "any name", SkipPolicies: true})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the project with that id already exists, returns 'already exists'", func(t *testing.T) {
			_, err := projCl.CreateProject(ctx, &api.CreateProjectReq{Id: id, Name: "my foo project", SkipPolicies: true})
			require.NoError(t, err)

			resp, err := projCl.CreateProject(ctx, &api.CreateProjectReq{Id: id, Name: "my other foo", SkipPolicies: true})
			grpctest.AssertCode(t, codes.AlreadyExists, err)
			assert.Nil(t, resp)
		}},
		{"does not create project if project limit surpassed", func(t *testing.T) {
			for i := 1; i <= constants.DefaultProjectLimit; i++ {
				projectID := "my-id-" + strconv.Itoa(i)
				project := &api.CreateProjectReq{
					Id:           projectID,
					Name:         "name-" + strconv.Itoa(i),
					SkipPolicies: true,
				}
				_, err := projCl.CreateProject(ctx, project)
				require.NoError(t, err)
			}

			oneProjectTooMany := &api.CreateProjectReq{
				Id:   "my-id-" + strconv.Itoa(projectLimitForTesting+1),
				Name: "name-" + strconv.Itoa(projectLimitForTesting+1),
			}
			resp, err := projCl.CreateProject(ctx, oneProjectTooMany)
			assert.Nil(t, resp)
			grpctest.AssertCode(t, codes.FailedPrecondition, err)
		}},
		{"if the project was successfully created, returns hydrated project", func(t *testing.T) {
			resp, err := projCl.CreateProject(ctx, &api.CreateProjectReq{Id: id, Name: name, SkipPolicies: true})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.NotNil(t, resp.Project)
			assert.Equal(t, id, resp.Project.GetId())
			assert.Equal(t, name, resp.Project.GetName())
			assert.Equal(t, api.Type_CUSTOM, resp.Project.GetType())
		}},
		{"if the skipPolicies flag is unset, creates the associated project policies by default", func(t *testing.T) {
			ctx = auth_context.NewOutgoingContext(auth_context.NewContext(context.Background(),
				[]string{SuperuserSubject}, []string{}, "*", "*"))
			err := createSystemRoles(ctx, polCl)
			require.NoError(t, err)

			resp, err := projCl.CreateProject(ctx, &api.CreateProjectReq{Id: id, Name: name})
			require.NoError(t, err)
			require.NotNil(t, resp.Project)

			editorResp, err := polCl.GetPolicy(ctx, &api.GetPolicyReq{Id: fmt.Sprintf("%s-project-editors", id)})
			require.NoError(t, err)
			require.NotNil(t, editorResp)

			viewerResp, err := polCl.GetPolicy(ctx, &api.GetPolicyReq{Id: fmt.Sprintf("%s-project-viewers", id)})
			require.NoError(t, err)
			require.NotNil(t, viewerResp)

			ownerResp, err := polCl.GetPolicy(ctx, &api.GetPolicyReq{Id: fmt.Sprintf("%s-project-owners", id)})
			require.NoError(t, err)
			require.NotNil(t, ownerResp)
		}},
		{"if the skipPolicies flag is set to false, creates the associated project policies", func(t *testing.T) {
			ctx = auth_context.NewOutgoingContext(auth_context.NewContext(context.Background(),
				[]string{SuperuserSubject}, []string{}, "*", "*"))
			err := createSystemRoles(ctx, polCl)
			require.NoError(t, err)

			resp, err := projCl.CreateProject(ctx, &api.CreateProjectReq{Id: id, Name: name, SkipPolicies: false})
			require.NoError(t, err)
			require.NotNil(t, resp.Project)

			editorResp, err := polCl.GetPolicy(ctx, &api.GetPolicyReq{Id: fmt.Sprintf("%s-project-editors", id)})
			require.NoError(t, err)
			require.NotNil(t, editorResp)

			viewerResp, err := polCl.GetPolicy(ctx, &api.GetPolicyReq{Id: fmt.Sprintf("%s-project-viewers", id)})
			require.NoError(t, err)
			require.NotNil(t, viewerResp)

			ownerResp, err := polCl.GetPolicy(ctx, &api.GetPolicyReq{Id: fmt.Sprintf("%s-project-owners", id)})
			require.NoError(t, err)
			require.NotNil(t, ownerResp)
		}},
		{"if the skipPolicies flag is set to true, does not create the associated project policies", func(t *testing.T) {
			_, err := projCl.CreateProject(ctx, &api.CreateProjectReq{Id: id, Name: name, SkipPolicies: true})
			require.NoError(t, err)

			_, err = polCl.GetPolicy(ctx, &api.GetPolicyReq{Id: fmt.Sprintf("%s-project-editors", id)})
			grpctest.AssertCode(t, codes.NotFound, err)

			_, err = polCl.GetPolicy(ctx, &api.GetPolicyReq{Id: fmt.Sprintf("%s-project-viewers", id)})
			grpctest.AssertCode(t, codes.NotFound, err)

			_, err = polCl.GetPolicy(ctx, &api.GetPolicyReq{Id: fmt.Sprintf("%s-project-owners", id)})
			grpctest.AssertCode(t, codes.NotFound, err)
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		err := store.Reset(ctx)
		require.NoError(t, err)
	}
}

func TestGetProject(t *testing.T) {
	ctx := context.Background()
	cl, store, cleanup := setupProjects(t)
	defer cleanup()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"if the project id is empty, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.GetProject(ctx, &api.GetProjectReq{Id: ""})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the project id is whitespace, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.GetProject(ctx, &api.GetProjectReq{Id: "     "})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the project does not exist, returns code 'not found'", func(t *testing.T) {
			resp, err := cl.GetProject(ctx, &api.GetProjectReq{Id: "foo"})
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, resp)
		}},
		{"if a chef-managed project exists, returns the project", func(t *testing.T) {
			id := "foo-project"
			apiProject := addProjectToStore(t, store, id, "my foo", storage.ChefManaged)

			resp, err := cl.GetProject(ctx, &api.GetProjectReq{Id: id})

			require.NoError(t, err)
			assert.Equal(t, &apiProject, resp.Project)
		}},
		{"if a custom project exists, returns the project", func(t *testing.T) {
			id := "foo-project"
			apiProject := addProjectToStore(t, store, id, "my foo", storage.Custom)

			resp, err := cl.GetProject(ctx, &api.GetProjectReq{Id: id})

			require.NoError(t, err)
			assert.Equal(t, &apiProject, resp.Project)
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

func TestDeleteProject(t *testing.T) {
	ctx := context.Background()
	cl, projects, rules, _, cleanup := setupProjectsAndRules(t)
	defer cleanup()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"if the project id is empty, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.DeleteProject(ctx, &api.DeleteProjectReq{Id: ""})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the project id is invalid, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.DeleteProject(ctx, &api.DeleteProjectReq{Id: "     "})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"fails with NotFound when deleting from empty store", func(t *testing.T) {
			require.Zero(t, projects.ItemCount())
			resp, err := cl.DeleteProject(ctx, &api.DeleteProjectReq{Id: "test"})
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, resp)
		}},
		{"deletes custom project with no rules when one project is in database", func(t *testing.T) {
			require.Zero(t, projects.ItemCount())
			id := fmt.Sprintf("test-project-%d", time.Now().UnixNano())
			addProjectToStore(t, projects, id, "my foo", storage.Custom)

			_, err := cl.DeleteProject(ctx, &api.DeleteProjectReq{Id: id})
			require.NoError(t, err)
			assert.Zero(t, projects.ItemCount())
		}},
		{"deletes custom project with no rules when several projects are in database", func(t *testing.T) {
			require.Zero(t, projects.ItemCount())
			id := fmt.Sprintf("test-project-%d", time.Now().UnixNano())
			addProjectToStore(t, projects, id, "my foo", storage.Custom)

			id2 := fmt.Sprintf("test-project-2-%d", time.Now().UnixNano())
			addProjectToStore(t, projects, id2, "my bar", storage.Custom)
			require.Equal(t, 2, projects.ItemCount())

			_, err := cl.DeleteProject(ctx, &api.DeleteProjectReq{Id: id})
			require.NoError(t, err)
			assert.Equal(t, 1, projects.ItemCount())

			_, found := projects.Get(id)
			assert.False(t, found)
		}},
		{"if the project has applied rules, returns 'failed precondition'", func(t *testing.T) {
			require.Zero(t, projects.ItemCount())
			storageConditions := []storage.Condition{
				{
					Attribute: storage.Organization,
					Operator:  storage.MemberOf,
					Value:     []string{"opscode"},
				},
			}
			projectID := fmt.Sprintf("test-project-%d", time.Now().UnixNano())
			ruleID := "foo-applied-rule"
			addProjectToStoreWithStatus(t, projects, projectID, "my foo", storage.Custom, storage.Applied.String())
			addRuleToStore(t, rules, ruleID, "my applied foo rule", applied, storage.Node, projectID, storageConditions)

			_, err := cl.DeleteProject(ctx, &api.DeleteProjectReq{Id: projectID})
			grpctest.AssertCode(t, codes.FailedPrecondition, err)
			assert.Equal(t, 1, projects.ItemCount())

			_, found := projects.Get(projectID)
			assert.True(t, found)
		}},
		{"if the project has staged rules, returns 'failed precondition'", func(t *testing.T) {
			require.Zero(t, projects.ItemCount())
			storageConditions := []storage.Condition{
				{
					Attribute: storage.Organization,
					Operator:  storage.MemberOf,
					Value:     []string{"opscode"},
				},
			}
			projectID := fmt.Sprintf("test-project-%d", time.Now().UnixNano())
			ruleID := "foo-staged-rule"
			addProjectToStoreWithStatus(t, projects, projectID, "my foo", storage.Custom, storage.EditsPending.String())
			addRuleToStore(t, rules, ruleID, "my staged foo rule", staged, storage.Node, projectID, storageConditions)

			_, err := cl.DeleteProject(ctx, &api.DeleteProjectReq{Id: projectID})
			grpctest.AssertCode(t, codes.FailedPrecondition, err)
			assert.Equal(t, 1, projects.ItemCount())

			_, found := projects.Get(projectID)
			assert.True(t, found)
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		projects.Flush()
		rules.Flush()
	}
}

func TestListProjects(t *testing.T) {
	ctx := context.Background()
	cl, store, cleanup := setupProjects(t)
	defer cleanup()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"if no projects exist, returns empty list", func(t *testing.T) {
			resp, err := cl.ListProjects(ctx, &api.ListProjectsReq{})
			require.NoError(t, err)
			assert.Empty(t, resp.Projects)
		}},
		{"if two projects exist, returns these projects", func(t *testing.T) {
			require.Zero(t, store.ItemCount())
			id0 := "foo-project"
			apiProject0 := addProjectToStore(t, store, id0, "my foo", storage.ChefManaged)

			id1 := "test-2-project"
			apiProject1 := addProjectToStore(t, store, id1, "my bar", storage.Custom)

			resp, err := cl.ListProjects(ctx, &api.ListProjectsReq{})
			require.NoError(t, err)
			assert.ElementsMatch(t, []*api.Project{&apiProject0, &apiProject1}, resp.Projects)
		}},
		{"suppresses all hidden system projects", func(t *testing.T) {
			require.Zero(t, store.ItemCount())
			addProjectToStore(t, store, constants.AllProjectsID, "All Projects", storage.ChefManaged)
			addProjectToStore(t, store, constants.UnassignedProjectID, "Unassigned", storage.ChefManaged)
			id0 := "foo-project"
			apiProject0 := addProjectToStore(t, store, id0, "my foo", storage.ChefManaged)
			id1 := "bar-project"
			apiProject1 := addProjectToStore(t, store, id1, "my bar", storage.Custom)

			resp, err := cl.ListProjects(ctx, &api.ListProjectsReq{})
			require.NoError(t, err)
			assert.ElementsMatch(t, []*api.Project{&apiProject0, &apiProject1}, resp.Projects)
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

func TestListProjectsForIntrospection(t *testing.T) {
	ctx := context.Background()
	cl, store, cleanup := setupProjects(t)
	defer cleanup()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"if no projects exist, returns empty list", func(t *testing.T) {
			resp, err := cl.ListProjectsForIntrospection(ctx, &api.ListProjectsReq{})
			require.NoError(t, err)
			assert.Empty(t, resp.Projects)
		}},
		{"if two projects exist, returns these projects", func(t *testing.T) {
			require.Zero(t, store.ItemCount())
			id0 := "foo-project"
			apiProject0 := addProjectToStore(t, store, id0, "my foo", storage.ChefManaged)

			id1 := "test-2-project"
			apiProject1 := addProjectToStore(t, store, id1, "my bar", storage.Custom)

			resp, err := cl.ListProjectsForIntrospection(ctx, &api.ListProjectsReq{})
			require.NoError(t, err)
			assert.ElementsMatch(t, []*api.Project{&apiProject0, &apiProject1}, resp.Projects)
		}},
		{"suppresses hidden system projects except for unassigned", func(t *testing.T) {
			require.Zero(t, store.ItemCount())
			addProjectToStore(t, store, constants.AllProjectsID, "All Projects", storage.ChefManaged)
			unassignedProject := addProjectToStore(t, store, constants.UnassignedProjectID, "Unassigned", storage.ChefManaged)
			id0 := "foo-project"
			apiProject0 := addProjectToStore(t, store, id0, "my foo", storage.ChefManaged)
			id1 := "bar-project"
			apiProject1 := addProjectToStore(t, store, id1, "my bar", storage.Custom)

			resp, err := cl.ListProjectsForIntrospection(ctx, &api.ListProjectsReq{})
			require.NoError(t, err)
			assert.ElementsMatch(t, []*api.Project{&apiProject0, &apiProject1, &unassignedProject}, resp.Projects)
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

func addProjectToStoreWithStatus(t *testing.T, store *cache.Cache, id,
	name string, projType storage.Type, status string) api.Project {
	t.Helper()

	proj := &storage.Project{
		ID:     id,
		Name:   name,
		Type:   projType,
		Status: status,
	}
	store.Add(id, proj, 0)

	returnType := api.Type_CHEF_MANAGED
	if projType == storage.Custom {
		returnType = api.Type_CUSTOM
	}
	return api.Project{
		Id:     id,
		Name:   name,
		Type:   returnType,
		Status: status,
	}
}

func addProjectToStore(t *testing.T, store *cache.Cache, id,
	name string, projType storage.Type) api.Project {
	return addProjectToStoreWithStatus(t, store, id, name, projType, storage.NoRules.String())
}

func setupProjects(t *testing.T) (api.ProjectsServiceClient, *cache.Cache, cleanupFunc) {
	cl, ca, _, _, cleanup := setupProjectsAndRules(t)
	return cl, ca, cleanup
}

type MockPurgeClient struct {
	purgeProject func(context.Context, string) error
}

func (m *MockPurgeClient) PurgeProject(ctx context.Context, something string) error {
	if m.purgeProject != nil {
		return m.purgeProject(ctx, something)
	}
	return nil
}

type cleanupFunc func()

func setupProjectsAndRules(t *testing.T) (api.ProjectsServiceClient, *cache.Cache, *cache.Cache,
	int64, cleanupFunc) {
	t.Helper()
	ctx := context.Background()
	seed := prng.GenSeed(t)

	l, err := logger.NewLogger("text", "error")
	require.NoError(t, err, "init logger for storage")

	memInstance := memstore.NewWithProjectLimit(projectLimitForTesting)
	// TODO: we should be able to optionally use PG. We're only using memstore
	pg, testDb, _, _, _ := testhelpers.SetupTestDBWithLimit(t, projectLimitForTesting)
	manager, err := cereal.NewManager(postgres.NewPostgresBackend(testDb.ConnURI))
	require.NoError(t, err)
	domainServices := []string{"testdomain"}
	projectPurger, err := project_purger_workflow.RegisterCerealProjectPurgerWithDomainServices(manager, l, memInstance, domainServices)
	require.NoError(t, err)

	err = project_purge.RegisterTaskExecutors(manager, "testdomain", &MockPurgeClient{})
	require.NoError(t, err)

	projectUpdateManager := testhelpers.NewMockProjectUpdateManager()
	projectsSrv, err := server.NewProjectsServer(
		ctx, l, memInstance,
		projectUpdateManager, projectPurger, testhelpers.NewMockPolicyRefresher())
	require.NoError(t, err)
	serviceCerts := helpers.LoadDevCerts(t, "authz-service")
	connFactory := secureconn.NewFactory(*serviceCerts)

	// TODO(sr): refactor our constructors. Having to maintain the middleware in
	// three places is tedious and error-prone.
	serv := connFactory.NewServer(grpc.UnaryInterceptor(
		server.InputValidationInterceptor(),
	))
	api.RegisterProjectsServiceServer(serv, projectsSrv)

	grpcServ := grpctest.NewServer(serv)

	conn, err := connFactory.Dial("authz-service", grpcServ.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}

	manager.Start(ctx)
	return api.NewProjectsServiceClient(conn), memInstance.ProjectsCache(), memInstance.RulesCache(), seed,
		func() {
			manager.Stop()
			pg.Close()
			testDb.CloseDB(t)
		}
}
