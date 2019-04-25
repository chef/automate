package v2_test

import (
	"context"
	"math/rand"
	"strconv"
	"testing"

	cache "github.com/patrickmn/go-cache"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"

	"github.com/chef/automate/lib/logger"

	api "github.com/chef/automate/api/interservice/authz/v2"
	automate_event "github.com/chef/automate/api/interservice/event"
	constants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/authz-service/engine"
	"github.com/chef/automate/components/authz-service/prng"
	grpc_server "github.com/chef/automate/components/authz-service/server"
	v2 "github.com/chef/automate/components/authz-service/server/v2"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
	memstore_v2 "github.com/chef/automate/components/authz-service/storage/v2/memstore"
	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/test/helpers"
)

func TestUpdateProject(t *testing.T) {
	ctx := context.Background()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"if the project name is empty, returns 'invalid argument'", func(t *testing.T) {
			cl, _, _ := setupProjects(t)
			resp, err := cl.UpdateProject(ctx, &api.UpdateProjectReq{Id: "empty-name", Name: ""})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the project id is empty, returns 'invalid argument'", func(t *testing.T) {
			cl, _, _ := setupProjects(t)
			resp, err := cl.UpdateProject(ctx, &api.UpdateProjectReq{Id: "", Name: "empty-id"})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the project id is invalid, returns 'invalid argument'", func(t *testing.T) {
			cl, _, _ := setupProjects(t)
			resp, err := cl.UpdateProject(ctx,
				&api.UpdateProjectReq{Id: "no_underscores", Name: "any name"})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the project with that id does not exist, returns 'not found'", func(t *testing.T) {
			cl, _, _ := setupProjects(t)
			resp, err := cl.UpdateProject(ctx,
				&api.UpdateProjectReq{Id: "false-project", Name: "my other foo"})
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, resp)
		}},
		{"if the project already exists and update request is valid, project is updated",
			func(t *testing.T) {
				cl, store, _ := setupProjects(t)
				id, updatedName := "my-foo", "updated name"
				addProjectToStore(t, store, id, "original name", storage.ChefManaged)

				resp, err := cl.UpdateProject(ctx, &api.UpdateProjectReq{Id: id, Name: updatedName})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.NotNil(t, resp.Project)
				assert.Equal(t, id, resp.Project.GetId())
				assert.Equal(t, updatedName, resp.Project.GetName())
				assert.Equal(t, api.Type_CUSTOM, resp.Project.GetType())
				require.Equal(t, 1, len(resp.Project.GetProjects()))
				assert.Equal(t, id, resp.Project.Projects[0])
			}},
		{"when a project is updated an event is published",
			func(t *testing.T) {
				cl, store, eventServiceClient := setupProjects(t)
				numberOfPublishes := eventServiceClient.PublishedEvents
				id, updatedName := "my-foo", "updated name"
				addProjectToStore(t, store, id, "original name", storage.ChefManaged)

				_, _ = cl.UpdateProject(ctx, &api.UpdateProjectReq{Id: id, Name: updatedName})

				assert.Equal(t, numberOfPublishes+1, eventServiceClient.PublishedEvents)
				assert.NotNil(t, eventServiceClient.LastestPublishedEvent.EventID)
				assert.NotNil(t, eventServiceClient.LastestPublishedEvent.Published)
				assert.NotNil(t,
					eventServiceClient.LastestPublishedEvent.Data.Fields["ProjectUpdateID"].GetStringValue())
			}},
		{"when a project is not updated an event is not published", func(t *testing.T) {
			cl, _, eventServiceClient := setupProjects(t)
			numberOfPublishes := eventServiceClient.PublishedEvents
			_, _ = cl.UpdateProject(ctx,
				&api.UpdateProjectReq{Id: "false-project", Name: "my other foo"})
			assert.Equal(t, numberOfPublishes, eventServiceClient.PublishedEvents)
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
	cl, store, _ := setupProjects(t)
	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"if the project data is invalid, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.CreateProject(ctx, &api.CreateProjectReq{Id: "empty-name", Name: ""})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the project id is empty, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.CreateProject(ctx, &api.CreateProjectReq{Id: "", Name: "empty-id"})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the project id is invalid, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.CreateProject(ctx, &api.CreateProjectReq{Id: "no_underscores", Name: "any name"})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the project with that id already exists, returns 'already exists'", func(t *testing.T) {
			id := "foo-project"
			addProjectToStore(t, store, id, "my foo project", storage.ChefManaged)

			resp, err := cl.CreateProject(ctx, &api.CreateProjectReq{Id: id, Name: "my other foo"})
			grpctest.AssertCode(t, codes.AlreadyExists, err)
			assert.Nil(t, resp)
		}},
		{"does not create project if max projects allow surpassed", func(t *testing.T) {
			for i := 1; i <= constants.MaxProjects+len(storage.DefaultProjectIDs()); i++ {
				projectID := "my-id-" + strconv.Itoa(i)
				project := &api.CreateProjectReq{
					Id:   projectID,
					Name: "name-" + strconv.Itoa(i),
				}
				_, err := cl.CreateProject(ctx, project)
				require.NoError(t, err)
			}

			oneProjectTooMany := &api.CreateProjectReq{
				Id:   "my-id-" + strconv.Itoa(constants.MaxProjects+1),
				Name: "name-" + strconv.Itoa(constants.MaxProjects+1),
			}
			resp, err := cl.CreateProject(ctx, oneProjectTooMany)
			assert.Nil(t, resp)
			grpctest.AssertCode(t, codes.FailedPrecondition, err)
		}},
		{"if the project was successfully created, returns hydrated project", func(t *testing.T) {
			id, name := "my-foo", "my foo"
			resp, err := cl.CreateProject(ctx, &api.CreateProjectReq{Id: id, Name: name})
			require.NoError(t, err)
			require.NotNil(t, resp)
			require.NotNil(t, resp.Project)
			assert.Equal(t, id, resp.Project.GetId())
			assert.Equal(t, name, resp.Project.GetName())
			assert.Equal(t, api.Type_CUSTOM, resp.Project.GetType())
			require.Equal(t, 1, len(resp.Project.GetProjects()))
			assert.Equal(t, id, resp.Project.Projects[0])
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

func TestGetProject(t *testing.T) {
	ctx := context.Background()
	cl, store, _ := setupProjects(t)
	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"if the project id is empty, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.GetProject(ctx, &api.GetProjectReq{Id: ""})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"if the project id is invalid, returns 'invalid argument'", func(t *testing.T) {
			resp, err := cl.GetProject(ctx, &api.GetProjectReq{Id: "no_underscore_allowed"})
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
	cl, store, _ := setupProjects(t)
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
			resp, err := cl.DeleteProject(ctx, &api.DeleteProjectReq{Id: "no_underscore_allowed"})
			grpctest.AssertCode(t, codes.InvalidArgument, err)
			assert.Nil(t, resp)
		}},
		{"fails with NotFound when deleting from empty store", func(t *testing.T) {
			require.Zero(t, store.ItemCount())
			resp, err := cl.DeleteProject(ctx, &api.DeleteProjectReq{Id: "test"})
			grpctest.AssertCode(t, codes.NotFound, err)
			assert.Nil(t, resp)
		}},
		{"deletes custom project when one project is in database", func(t *testing.T) {
			require.Zero(t, store.ItemCount())
			id := "test-project"
			addProjectToStore(t, store, id, "my foo", storage.Custom)

			_, err := cl.DeleteProject(ctx, &api.DeleteProjectReq{Id: id})
			require.NoError(t, err)
			assert.Zero(t, store.ItemCount())
		}},
		{"deletes custom project when several are in database", func(t *testing.T) {
			require.Zero(t, store.ItemCount())
			id := "test-project"
			addProjectToStore(t, store, id, "my foo", storage.Custom)

			id2 := "test-2-project"
			addProjectToStore(t, store, id2, "my bar", storage.Custom)
			require.Equal(t, 2, store.ItemCount())

			_, err := cl.DeleteProject(ctx, &api.DeleteProjectReq{Id: id})
			require.NoError(t, err)
			assert.Equal(t, 1, store.ItemCount())

			_, found := store.Get(id)
			assert.False(t, found)
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

func TestListProjects(t *testing.T) {
	ctx := context.Background()
	cl, store, _ := setupProjects(t)

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
		{"suppresses hidden system projects", func(t *testing.T) {
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

func addProjectToStore(t *testing.T, store *cache.Cache, id, name string, projType storage.Type) api.Project {
	t.Helper()

	proj := &storage.Project{
		ID:       id,
		Name:     name,
		Type:     projType,
		Projects: []string{id},
	}
	store.Add(id, proj, 0)

	returnType := api.Type_CHEF_MANAGED
	if projType == storage.Custom {
		returnType = api.Type_CUSTOM
	}
	return api.Project{
		Id:       id,
		Name:     name,
		Type:     returnType,
		Projects: []string{id},
	}
}

func setupProjects(t *testing.T) (api.ProjectsClient, *cache.Cache, *mockEventServiceClient) {
	t.Helper()
	ctx := context.Background()
	prng.Seed(t)

	l, err := logger.NewLogger("text", "error")
	require.NoError(t, err, "init logger for storage")

	mem_v2 := memstore_v2.New()
	eventServiceClient := &mockEventServiceClient{}
	projectsSrv, err := v2.NewProjectsServer(ctx, l, mem_v2, &testProjectRulesRetriever{},
		eventServiceClient)
	require.NoError(t, err)

	serviceCerts := helpers.LoadDevCerts(t, "authz-service")
	connFactory := secureconn.NewFactory(*serviceCerts)

	// TODO(sr): refactor our constructors. Having to maintain the middleware in
	// three places is tedious and error-prone.
	serv := connFactory.NewServer(grpc.UnaryInterceptor(
		grpc_server.InputValidationInterceptor(),
	))
	api.RegisterProjectsServer(serv, projectsSrv)

	grpcServ := grpctest.NewServer(serv)

	conn, err := connFactory.Dial("authz-service", grpcServ.URL)
	if err != nil {
		t.Fatalf("connecting to grpc endpoint: %s", err)
	}

	return api.NewProjectsClient(conn), mem_v2.ProjectsCache(), eventServiceClient
}

// TODO More testing
type testProjectRulesRetriever struct{}

func (t *testProjectRulesRetriever) RulesForProject(
	context.Context, string) ([]engine.Rule, error) {
	return []engine.Rule{}, nil
}

func (t *testProjectRulesRetriever) ListProjectMappings(
	context.Context) (map[string][]engine.Rule, error) {
	return make(map[string][]engine.Rule, 0), nil
}

type mockEventServiceClient struct {
	PublishedEvents       int
	LastestPublishedEvent *automate_event.EventMsg
}

func (t *mockEventServiceClient) Publish(ctx context.Context,
	in *automate_event.PublishRequest,
	opts ...grpc.CallOption) (*automate_event.PublishResponse, error) {
	t.PublishedEvents++
	t.LastestPublishedEvent = in.Msg
	return &automate_event.PublishResponse{}, nil
}

func (t *mockEventServiceClient) Subscribe(ctx context.Context,
	in *automate_event.SubscribeRequest,
	opts ...grpc.CallOption) (*automate_event.SubscribeResponse, error) {
	return &automate_event.SubscribeResponse{}, nil
}

func (t *mockEventServiceClient) Start(ctx context.Context,
	in *automate_event.StartRequest,
	opts ...grpc.CallOption) (*automate_event.StartResponse, error) {
	return &automate_event.StartResponse{}, nil
}

func (t *mockEventServiceClient) Stop(ctx context.Context,
	in *automate_event.StopRequest,
	opts ...grpc.CallOption) (*automate_event.StopResponse, error) {
	return &automate_event.StopResponse{}, nil
}
