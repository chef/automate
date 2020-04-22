package postgres_test

import (
	"context"
	"testing"

	"github.com/lib/pq"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/components/authz-service/testhelpers"
)

func TestCreateRole(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()
	ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})

	cases := map[string]func(*testing.T){
		"successfully creates role with NO actions": func(t *testing.T) {
			// While disallowed at a higher level, no actions are OK at the DB level
			role := storage.Role{
				ID:       "my-id-1",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{},
				Projects: []string{},
			}
			assertPolicyChange(t, store, func() {
				resp, err := store.CreateRole(ctx, &role, false)
				require.NoError(t, err)
				require.Equal(t, &role, resp)
			})

			assertRolesMatch(t, db, role)
		},
		"successfully creates role with SOME actions": func(t *testing.T) {
			role := storage.Role{
				ID:       "my-id-2",
				Name:     "name2",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2", "action3"},
				Projects: []string{},
			}
			assertPolicyChange(t, store, func() {
				resp, err := store.CreateRole(ctx, &role, false)
				require.NoError(t, err)
				require.Equal(t, &role, resp)
			})

			assertRolesMatch(t, db, role)
		},
		"successfully creates a role with a project": func(t *testing.T) {
			project := storage.Project{
				ID:   "my-id-1",
				Name: "name1",
				Type: storage.Custom,
			}

			_, err := store.CreateProject(ctx, &project, true)
			require.NoError(t, err)

			role := storage.Role{
				ID:       "my-id-2",
				Name:     "name2",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2", "action3"},
				Projects: []string{"my-id-1"},
			}
			assertPolicyChange(t, store, func() {
				resp, err := store.CreateRole(ctx, &role, true)
				require.NoError(t, err)
				require.Equal(t, &role, resp)
			})

			assertRolesMatch(t, db, role)
		},
		"successfully creates role with multiple projects": func(t *testing.T) {
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			role := storage.Role{
				ID:       "my-id-2",
				Name:     "name2",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2", "action3"},
				Projects: []string{project1.ID, project2.ID},
			}
			assertPolicyChange(t, store, func() {
				resp, err := store.CreateRole(ctx, &role, false)
				require.NoError(t, err)
				require.Equal(t, &role, resp)
			})

			assertRolesMatch(t, db, role)
		},
		"does not creates role with duplicate ID": func(t *testing.T) {
			roleID := "my-id-5"
			role := storage.Role{
				ID:      roleID,
				Name:    "name4a",
				Type:    storage.Custom,
				Actions: []string{"action1", "action2"},
			}
			assertPolicyChange(t, store, func() {
				resp, err := store.CreateRole(ctx, &role, false)
				require.NoError(t, err)
				require.Equal(t, &role, resp)
			})

			role2 := storage.Role{
				ID:      roleID,
				Name:    "name4b",
				Type:    storage.Custom,
				Actions: []string{"action3", "action4"},
			}
			assertNoPolicyChange(t, store, func() {
				resp, err := store.CreateRole(ctx, &role2, false)
				assert.Error(t, err)
				assert.Equal(t, storage.ErrConflict, err)
				assert.Nil(t, resp)
			})
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestListRoles(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"successfully returns empty list when the database is empty": func(t *testing.T) {
			ctx := context.Background()
			resp, err := store.ListRoles(ctx)

			require.NoError(t, err)
			assert.Nil(t, resp)
		},
		"successfully returns list with role when the database has one role": func(t *testing.T) {
			ctx := context.Background()
			roles := []*storage.Role{{
				ID:       "my-id-1",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action1"},
				Projects: []string{},
			}}
			insertTestRole(t, db, roles[0].ID, roles[0].Name, roles[0].Actions, roles[0].Projects)

			resp, err := store.ListRoles(ctx)

			require.NoError(t, err)
			assert.ElementsMatch(t, roles, resp)
		},
		"successfully returns list with roles when the database has several roles": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			roles := []*storage.Role{{
				ID:       "my-id-2",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2"},
				Projects: []string{},
			}, {
				ID:       "my-id-3",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action3", "action4"},
				Projects: []string{project1.ID},
			}, {
				ID:       "my-id-4",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action5", "action6"},
				Projects: []string{project1.ID, project2.ID},
			}}
			for _, role := range roles {
				insertTestRole(t, db, role.ID, role.Name, role.Actions, role.Projects)
			}

			resp, err := store.ListRoles(ctx)

			require.NoError(t, err)
			assert.ElementsMatch(t, roles, resp)
		},
		"successfully returns filtered list when rows intersect with projects filter": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:   "project-3",
				Name: "name3",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project3, true)
			require.NoError(t, err)

			roles := []*storage.Role{{
				ID:       "my-id-0",
				Name:     "name0",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2"},
				Projects: []string{},
			}, {
				ID:       "my-id-1",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action3", "action4"},
				Projects: []string{project1.ID},
			}, {
				ID:       "my-id-2",
				Name:     "name2",
				Type:     storage.Custom,
				Actions:  []string{"action5", "action6"},
				Projects: []string{project1.ID, project2.ID},
			}, {
				ID:       "my-id-3",
				Name:     "name3",
				Type:     storage.Custom,
				Actions:  []string{"action7", "action8"},
				Projects: []string{project3.ID},
			}, {
				ID:       "my-id-4",
				Name:     "name4",
				Type:     storage.Custom,
				Actions:  []string{"action7", "action8"},
				Projects: []string{project2.ID},
			}}
			for _, role := range roles {
				insertTestRole(t, db, role.ID, role.Name, role.Actions, role.Projects)
			}

			ctx = insertProjectsIntoContext(ctx, []string{project1.ID, project2.ID})
			resp, err := store.ListRoles(ctx)

			require.NoError(t, err)
			expected := []*storage.Role{
				roles[1],
				roles[2],
				roles[4],
			}
			assert.ElementsMatch(t, expected, resp)
		},
		"successfully returns all projects when filter is *": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:   "project-3",
				Name: "name3",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project3, true)
			require.NoError(t, err)

			roles := []*storage.Role{{
				ID:       "my-id-1",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2"},
				Projects: []string{},
			}, {
				ID:       "my-id-2",
				Name:     "name2",
				Type:     storage.Custom,
				Actions:  []string{"action3", "action4"},
				Projects: []string{project1.ID},
			}, {
				ID:       "my-id-3",
				Name:     "name3",
				Type:     storage.Custom,
				Actions:  []string{"action5", "action6"},
				Projects: []string{project1.ID, project2.ID},
			}, {
				ID:       "my-id-4",
				Name:     "name4",
				Type:     storage.Custom,
				Actions:  []string{"action7", "action8"},
				Projects: []string{project3.ID},
			}, {
				ID:       "my-id-5",
				Name:     "name5",
				Type:     storage.Custom,
				Actions:  []string{"action7", "action8"},
				Projects: []string{project2.ID},
			}}
			for _, role := range roles {
				insertTestRole(t, db, role.ID, role.Name, role.Actions, role.Projects)
			}

			ctx = insertProjectsIntoContext(ctx, []string{constants.AllProjectsExternalID})
			resp, err := store.ListRoles(ctx)

			require.NoError(t, err)
			assert.ElementsMatch(t, roles, resp)
		},
		"successfully returns roles with unassigned projects": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			roles := []*storage.Role{{
				ID:       "my-id-2",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2"},
				Projects: []string{},
			}, {
				ID:       "my-id-3",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action3", "action4"},
				Projects: []string{project1.ID},
			}, {
				ID:       "my-id-4",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action5", "action6"},
				Projects: []string{project1.ID, project2.ID},
			}}
			for _, role := range roles {
				insertTestRole(t, db, role.ID, role.Name, role.Actions, role.Projects)
			}

			ctx = insertProjectsIntoContext(ctx, []string{constants.UnassignedProjectID})
			resp, err := store.ListRoles(ctx)

			expected := []*storage.Role{
				{
					ID:       "my-id-2",
					Name:     "name1",
					Type:     storage.Custom,
					Actions:  []string{"action1", "action2"},
					Projects: []string{},
				},
			}
			require.NoError(t, err)
			assert.ElementsMatch(t, expected, resp)
		},
		"returns empty list if projects filter excludes all objects": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			roles := []*storage.Role{{
				ID:       "my-id-2",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action1", "action2"},
				Projects: []string{},
			}, {
				ID:       "my-id-3",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action3", "action4"},
				Projects: []string{project1.ID},
			}, {
				ID:       "my-id-4",
				Name:     "name1",
				Type:     storage.Custom,
				Actions:  []string{"action5", "action6"},
				Projects: []string{project1.ID, project2.ID},
			}}
			for _, role := range roles {
				insertTestRole(t, db, role.ID, role.Name, role.Actions, role.Projects)
			}

			ctx = insertProjectsIntoContext(ctx, []string{"some-other-project"})
			resp, err := store.ListRoles(ctx)

			require.NoError(t, err)
			assert.ElementsMatch(t, []*storage.Role{}, resp)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestGetRole(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"returns policy not found error with empty database": func(t *testing.T) {
			ctx := context.Background()
			resp, err := store.GetRole(ctx, "fake-id")
			assert.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrNotFound, err)
		},
		"returns policy not found error with database that has several roles": func(t *testing.T) {
			ctx := context.Background()
			resp, err := store.GetRole(ctx, "fake-id")
			assert.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrNotFound, err)
		},
		"successfully returns appropriate role when the database has one role": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID})

			resp, err := store.GetRole(ctx, "my-id-1")

			require.NoError(t, err)
			assert.Equal(t, role.ID, resp.ID)
			assert.Equal(t, role.Name, resp.Name)
			assert.Equal(t, storage.Custom, resp.Type)
			assert.ElementsMatch(t, role.Actions, resp.Actions)
		},
		"successfully returns appropriate role when the database has several roles": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			resp, err := store.GetRole(ctx, "my-id-1")

			require.NoError(t, err)
			assert.Equal(t, role.ID, resp.ID)
			assert.Equal(t, role.Name, resp.Name)
			assert.Equal(t, storage.Custom, resp.Type)
			assert.ElementsMatch(t, role.Actions, resp.Actions)
		},
		"successfully returns appropriate role when the project filter intersects": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project2.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{project1.ID, project2.ID})
			resp, err := store.GetRole(ctx, "my-id-1")

			require.NoError(t, err)
			assert.Equal(t, role.ID, resp.ID)
			assert.Equal(t, role.Name, resp.Name)
			assert.Equal(t, storage.Custom, resp.Type)
			assert.ElementsMatch(t, role.Actions, resp.Actions)
		},
		"successfully returns appropriate role when the project filter is *": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project2.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{constants.AllProjectsExternalID})
			resp, err := store.GetRole(ctx, "my-id-1")

			require.NoError(t, err)
			assert.Equal(t, role.ID, resp.ID)
			assert.Equal(t, role.Name, resp.Name)
			assert.Equal(t, storage.Custom, resp.Type)
			assert.ElementsMatch(t, role.Actions, resp.Actions)
		},
		"successfully returns appropriate role when the project filter is * and role has no projects": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{constants.AllProjectsExternalID})
			resp, err := store.GetRole(ctx, "my-id-1")

			require.NoError(t, err)
			assert.Equal(t, role.ID, resp.ID)
			assert.Equal(t, role.Name, resp.Name)
			assert.Equal(t, storage.Custom, resp.Type)
			assert.ElementsMatch(t, role.Actions, resp.Actions)
		},
		"successfully returns appropriate role when the role has no projects": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{constants.UnassignedProjectID})
			resp, err := store.GetRole(ctx, "my-id-1")

			require.NoError(t, err)
			assert.Equal(t, role.ID, resp.ID)
			assert.Equal(t, role.Name, resp.Name)
			assert.Equal(t, storage.Custom, resp.Type)
			assert.ElementsMatch(t, role.Actions, resp.Actions)
		},
		"returns NotFound when no project filter intersection": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project2.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{project1.ID})
			resp, err := store.GetRole(ctx, "my-id-1")

			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrNotFound, err)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestDeleteRole(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"returns role not found error with empty database": func(t *testing.T) {
			ctx := context.Background()
			err := store.DeleteRole(ctx, "test-role")
			assert.Error(t, err)
			assert.Equal(t, storage.ErrNotFound, err)
		},
		"returns role not found with several roles in database": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			err = store.DeleteRole(ctx, "test-role")
			assert.Error(t, err)
			assert.Equal(t, storage.ErrNotFound, err)
		},
		"deletes role with one role in database": func(t *testing.T) {
			ctx := context.Background()
			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})

			err := store.DeleteRole(ctx, role.ID)

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles WHERE id=$1`, role.ID))
		},
		"deletes role with several roles in database": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			err = store.DeleteRole(ctx, role.ID)

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles WHERE id=$1`, role.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_roles`))
		},
		"when statements contains a role and no actions and are the last statements in a policy, on role deletion the policy is deleted": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			roleDeleted := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID})
			roleRemaining := insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{project1.ID})

			polID := insertTestPolicy(t, db, "testpolicy")
			sID0 := insertTestStatement(t, db, polID, "allow", roleDeleted.ID, []string{}, []string{"iam:users"})
			sID1 := insertTestStatement(t, db, polID, "deny", roleDeleted.ID, []string{}, []string{"compliance:profiles"})

			polIDWrongRole := insertTestPolicy(t, db, "testpolicy2")
			sID0WrongRole := insertTestStatement(t, db, polIDWrongRole, "allow", roleRemaining.ID, []string{}, []string{"iam:users"})
			sID1WrongRole := insertTestStatement(t, db, polIDWrongRole, "deny", roleRemaining.ID, []string{}, []string{"compliance:profiles"})

			assertPolicyChange(t, store, func() {
				err = store.DeleteRole(ctx, roleDeleted.ID)
				require.NoError(t, err)
			})

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles WHERE id=$1`, roleDeleted.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_roles`))

			assertEmpty(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(statementWithID, sID0))
			assertEmpty(t, db.QueryRow(statementWithID, sID1))

			assertOne(t, db.QueryRow(policyWithID, polIDWrongRole))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE db_id=$1 AND role_id=role_db_id($2)`, sID0WrongRole, roleRemaining.ID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE db_id=$1 AND role_id=role_db_id($2)`, sID1WrongRole, roleRemaining.ID))
		},
		"when one statement contains a role and as well as actions but one statement contains no actions, on role deletion there is only one modified statement remaining": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			roleDeleted := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID})
			roleRemaining := insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{project1.ID})

			polID := insertTestPolicy(t, db, "testpolicy")
			sID0 := insertTestStatement(t, db, polID, "allow", roleDeleted.ID, []string{}, []string{"iam:users"})
			actions := []string{"compliance:profiles:download"}
			sID1 := insertTestStatement(t, db, polID, "deny", roleDeleted.ID, actions, []string{"compliance:profiles"})

			polIDWrongRole := insertTestPolicy(t, db, "testpolicy2")
			sID0WrongRole := insertTestStatement(t, db, polIDWrongRole, "allow", roleRemaining.ID, []string{}, []string{"iam:users"})
			sID1WrongRole := insertTestStatement(t, db, polIDWrongRole, "deny", roleRemaining.ID, []string{}, []string{"compliance:profiles"})

			assertPolicyChange(t, store, func() {
				err = store.DeleteRole(ctx, roleDeleted.ID)
				require.NoError(t, err)
			})

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles WHERE id=$1`, roleDeleted.ID))
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_roles`))

			assertOne(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(statementWithID, sID0))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE db_id=$1 AND actions=$2 AND role_id IS NULL`, sID1, pq.Array(actions)))

			assertOne(t, db.QueryRow(policyWithID, polIDWrongRole))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE db_id=$1 AND role_id=role_db_id($2)`, sID0WrongRole, roleRemaining.ID))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statements WHERE db_id=$1 AND role_id=role_db_id($2)`, sID1WrongRole, roleRemaining.ID))
		},
		"deletes role with several roles in database when projects filter has intersection": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:   "project-3",
				Name: "name3",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project3, true)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID, project3.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{project1.ID, project2.ID})
			err = store.DeleteRole(ctx, role.ID)

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles WHERE id=$1`, role.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_roles`))
		},
		"deletes role with no projects assigned when projects filter has intersection": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:   "project-3",
				Name: "name3",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project3, true)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{constants.UnassignedProjectID})
			err = store.DeleteRole(ctx, role.ID)

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles WHERE id=$1`, role.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_roles`))
		},
		"deletes role with several roles in database when projects filter is *": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:   "project-3",
				Name: "name3",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project3, true)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID, project3.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{constants.AllProjectsExternalID})
			err = store.DeleteRole(ctx, role.ID)

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_roles WHERE id=$1`, role.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_roles`))
		},
		"returns NotFound when project filter has no intersection": func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:   "project-3",
				Name: "name3",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project3, true)
			require.NoError(t, err)

			role := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID, project3.ID})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{project1.ID})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			ctx = insertProjectsIntoContext(ctx, []string{project2.ID})
			err = store.DeleteRole(ctx, role.ID)
			assert.Equal(t, storage.ErrNotFound, err)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestUpdateRole(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()
	nonexistingRole := storage.Role{
		ID:      "nonexistant",
		Name:    "name",
		Actions: []string{"actionx"},
	}

	cases := map[string]func(*testing.T){
		"returns role not found error with empty database": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			role, err := store.UpdateRole(ctx, &nonexistingRole)

			assert.Nil(t, role)
			assert.Error(t, err)
			assert.Equal(t, storage.ErrNotFound, err)
		},
		"returns role not found with several roles in database": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})
			insertTestRole(t, db, "my-id-2", "name", []string{"action2"}, []string{})
			insertTestRole(t, db, "my-id-3", "name", []string{"action3"}, []string{})
			insertTestRole(t, db, "my-id-4", "name", []string{"action4"}, []string{})

			role, err := store.UpdateRole(ctx, &nonexistingRole)

			assert.Nil(t, role)
			assert.Error(t, err)
			assert.Equal(t, storage.ErrNotFound, err)
		},
		"updates name of a role": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})
			r := storage.Role{
				ID:       dbRole.ID,
				Name:     "new name",
				Actions:  dbRole.Actions,
				Projects: []string{},
			}

			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, "new name", updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, dbRole.Actions, updatedRole.Actions)
			assert.ElementsMatch(t, []string{}, updatedRole.Projects)
		},
		"updates action of a role": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID})
			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{project1.ID},
			}

			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
			assert.ElementsMatch(t, []string{project1.ID}, updatedRole.Projects)
		},
		"updates the projects of a role": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:   "project-3",
				Name: "name3",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project3, true)
			require.NoError(t, err)

			project4 := storage.Project{
				ID:   "project-4",
				Name: "name4",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project4, true)
			require.NoError(t, err)

			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID, project4.ID})
			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{project2.ID, project3.ID, project4.ID},
			}

			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
			assert.ElementsMatch(t, []string{project2.ID, project3.ID, project4.ID}, updatedRole.Projects)
		},
		"updates the projects of a role to be empty": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID, project2.ID})
			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{},
			}

			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
			assert.ElementsMatch(t, []string{}, updatedRole.Projects)
		},
		"fails to update when a project filter is specified with no intersection": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project2.ID})

			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{project2.ID},
			}
			ctx = insertProjectsIntoContext(ctx, []string{project1.ID})
			updatedRole, err := store.UpdateRole(ctx, &r)

			assert.Nil(t, updatedRole)
			assert.Equal(t, storage.ErrNotFound, err)
		},
		"updates successfully when a project filter is specified with an intersection": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			project3 := storage.Project{
				ID:   "project-3",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project3, true)
			require.NoError(t, err)

			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project2.ID, project3.ID})

			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{project2.ID},
			}
			ctx = insertProjectsAndSubjectsIntoContext(context.Background(), []string{project2.ID, project1.ID}, []string{SuperuserSubject})
			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
			assert.ElementsMatch(t, []string{project2.ID}, updatedRole.Projects)
		},
		"updates successfully when a project filter is *": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{project1.ID})

			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{project1.ID},
			}
			ctx = insertProjectsAndSubjectsIntoContext(context.Background(), []string{constants.AllProjectsExternalID}, []string{SuperuserSubject})
			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
			assert.ElementsMatch(t, []string{project1.ID}, updatedRole.Projects)
		},
		"updates successfully when a role has no projects": func(t *testing.T) {
			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})

			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{},
			}
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{constants.UnassignedProjectID}, []string{SuperuserSubject})
			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
			assert.ElementsMatch(t, []string{}, updatedRole.Projects)
		},
		"updates the projects of a role to contain projects from empty": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			require.NoError(t, err)

			project2 := storage.Project{
				ID:   "project-2",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})
			r := storage.Role{
				ID:       dbRole.ID,
				Name:     dbRole.Name,
				Actions:  []string{"newaction"},
				Projects: []string{project1.ID, project2.ID},
			}

			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
			assert.ElementsMatch(t, []string{project1.ID, project2.ID}, updatedRole.Projects)
		},
		"successfully runs even if nothing is actually changed": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})
			r := storage.Role{
				ID:      dbRole.ID,
				Name:    dbRole.Name,
				Actions: dbRole.Actions,
			}

			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, dbRole.Name, updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, dbRole.Actions, updatedRole.Actions)
			assert.ElementsMatch(t, []string{}, updatedRole.Projects)
		},
		"successfully updates multiple properties at once": func(t *testing.T) {
			ctx := insertProjectsAndSubjectsIntoContext(context.Background(), []string{}, []string{SuperuserSubject})
			dbRole := insertTestRole(t, db, "my-id-1", "name", []string{"action1"}, []string{})
			r := storage.Role{
				ID:      dbRole.ID,
				Name:    "new name",
				Actions: []string{"newaction"},
			}

			updatedRole, err := store.UpdateRole(ctx, &r)

			require.NoError(t, err)
			assert.Equal(t, dbRole.ID, updatedRole.ID)
			assert.Equal(t, "new name", updatedRole.Name)
			assert.Equal(t, storage.Custom, updatedRole.Type)
			assert.ElementsMatch(t, []string{"newaction"}, updatedRole.Actions)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}
