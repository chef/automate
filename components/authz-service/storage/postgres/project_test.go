package postgres_test

import (
	"context"
	"math/rand"
	"strconv"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/components/authz-service/testhelpers"
	"github.com/chef/automate/lib/projectassignment"
)

func TestCreateProject(t *testing.T) {
	projectLimit := 7
	store, db, _, _, _ := testhelpers.SetupTestDBWithLimit(t, projectLimit)
	defer db.CloseDB(t)
	defer store.Close()
	ctx := context.Background()

	cases := map[string]func(*testing.T){
		"successfully creates chef-managed project": func(t *testing.T) {
			project := storage.Project{
				ID:     "my-id-1",
				Name:   "name1",
				Type:   storage.ChefManaged,
				Status: storage.NoRules.String(),
			}
			resp, err := store.CreateProject(ctx, &project, true)
			require.NoError(t, err)
			require.Equal(t, &project, resp)

			assertProjectsMatch(t, db, project)
		},
		"successfully creates custom project without supporting policies": func(t *testing.T) {
			project := storage.Project{
				ID:     "my-id-1",
				Name:   "name1",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			resp, err := store.CreateProject(ctx, &project, true)
			require.NoError(t, err)
			require.Equal(t, &project, resp)

			assertProjectsMatch(t, db, project)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policies`))
		},
		"successfully creates custom project with supporting policies": func(t *testing.T) {
			project := storage.Project{
				ID:     "my-id-1",
				Name:   "name1",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_policies`))
			resp, err := store.CreateProject(ctx, &project, false)
			require.NoError(t, err)
			require.Equal(t, &project, resp)

			assertProjectsMatch(t, db, project)
			assertCount(t, 3, db.QueryRow(
				`SELECT count(*) FROM iam_policies
				 WHERE id IN ('my-id-1-project-owners','my-id-1-project-viewers','my-id-1-project-editors')`))
		},
		"does not create project with duplicate ID": func(t *testing.T) {
			projectID := "my-id-1"
			projectOriginal := storage.Project{
				ID:     projectID,
				Name:   "name1",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			resp, err := store.CreateProject(ctx, &projectOriginal, true)
			require.NoError(t, err)
			require.Equal(t, &projectOriginal, resp)

			projectConflict := storage.Project{
				ID:   projectID,
				Name: "Something Else",
				Type: storage.Custom,
			}
			resp, err = store.CreateProject(ctx, &projectConflict, true)
			assert.Error(t, err)
			assert.Equal(t, storage.ErrConflict, err)
			assert.Nil(t, resp)
		},
		"does not create custom project if max number of custom projects allowed has been reached": func(t *testing.T) {
			for i := 1; i <= projectLimit; i++ {
				projectID := "my-id-" + strconv.Itoa(i)
				project := storage.Project{
					ID:     projectID,
					Name:   "name-" + strconv.Itoa(i),
					Type:   storage.Custom,
					Status: storage.NoRules.String(),
				}
				resp, err := store.CreateProject(ctx, &project, true)
				require.NoError(t, err)
				require.Equal(t, &project, resp)
			}

			oneProjectTooManyID := "my-id-" + strconv.Itoa(projectLimit+1)
			oneProjectTooMany := storage.Project{
				ID:     oneProjectTooManyID,
				Name:   "Something Else",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			resp, err := store.CreateProject(ctx, &oneProjectTooMany, true)
			assert.Nil(t, resp)
			assert.Error(t, err)
			_, correctError := err.(*storage.MaxProjectsExceededError)
			assert.True(t, correctError)
		},
		"does create chef-managed project if max number of custom projects allowed has been reached": func(t *testing.T) {
			for i := 1; i <= projectLimit; i++ {
				projectID := "my-id-" + strconv.Itoa(i)
				project := storage.Project{
					ID:     projectID,
					Name:   "name-" + strconv.Itoa(i),
					Type:   storage.Custom,
					Status: storage.NoRules.String(),
				}
				resp, err := store.CreateProject(ctx, &project, true)
				require.NoError(t, err)
				require.Equal(t, &project, resp)
			}

			chefManagedProjectID := "my-id-" + strconv.Itoa(projectLimit+1)
			chefManagedProject := storage.Project{
				ID:     chefManagedProjectID,
				Name:   "Something Else",
				Type:   storage.ChefManaged,
				Status: storage.NoRules.String(),
			}
			resp, err := store.CreateProject(ctx, &chefManagedProject, true)
			require.NoError(t, err)
			require.Equal(t, &chefManagedProject, resp)
		},
	}

	for name, test := range cases {
		insertTestRole(t, db, constants.ViewerRoleID, "viewer", []string{"any"}, []string{})
		insertTestRole(t, db, constants.EditorRoleID, "editor", []string{"any"}, []string{})
		insertTestRole(t, db, constants.ProjectOwnerRoleID, "project owner", []string{"any"}, []string{})
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestUpdateProject(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"successfully updates existing custom project": func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			project := storage.Project{
				ID:     "foo",
				Name:   "updated-name",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			resp, err := store.UpdateProject(ctx, &project)
			require.NoError(t, err)
			require.Equal(t, &project, resp)

			assertProjectsMatch(t, db, project)
		},
		"successfully updates existing custom project with a project filter": func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			project := storage.Project{
				ID:     "foo",
				Name:   "updated-name",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			ctx = insertProjectsIntoContext(ctx, []string{"foo", "bar"})
			resp, err := store.UpdateProject(ctx, &project)
			require.NoError(t, err)
			require.Equal(t, &project, resp)

			assertProjectsMatch(t, db, project)
		},
		"successfully updates existing custom project with a project filter of *": func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			project := storage.Project{
				ID:     "foo",
				Name:   "updated-name",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			ctx = insertProjectsIntoContext(ctx, []string{constants.AllProjectsExternalID})

			resp, err := store.UpdateProject(ctx, &project)
			require.NoError(t, err)
			require.Equal(t, &project, resp)

			assertProjectsMatch(t, db, project)
		},
		"returns ErrNotFound if it doesn't exist": func(t *testing.T) {
			ctx := context.Background()
			project := storage.Project{
				ID:     "not-found",
				Name:   "name1",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			resp, err := store.UpdateProject(ctx, &project)
			assert.Equal(t, storage.ErrNotFound, err)
			assert.Nil(t, resp)
		},
		"returns ErrNotFound if the project exists but does not have a project in the project filter list": func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			project := storage.Project{
				ID:     "foo",
				Name:   "updated-name",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			ctx = insertProjectsIntoContext(ctx, []string{"wrong", "projects"})

			resp, err := store.UpdateProject(ctx, &project)
			assert.Equal(t, storage.ErrNotFound, err)
			assert.Nil(t, resp)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestGetProject(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"when no project exists, returns not found error", func(t *testing.T) {
			ctx := context.Background()
			p, err := store.GetProject(ctx, "id-that-wont-be-found")
			assert.Equal(t, storage.ErrNotFound, err)
			assert.Nil(t, p)
		}},
		{"when a chef-managed project exists, returns that project", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)

			p, err := store.GetProject(ctx, "foo")
			require.NoError(t, err)
			expectedProject := storage.Project{
				ID:     "foo",
				Name:   "my foo project",
				Type:   storage.ChefManaged,
				Status: storage.NoRules.String(),
			}
			assert.Equal(t, &expectedProject, p)
		}},
		{"when a custom project exists with no rules, returns that project", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{"foo", "bar"})

			p, err := store.GetProject(ctx, "foo")
			require.NoError(t, err)
			expectedProject := storage.Project{
				ID:     "foo",
				Name:   "my foo project",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			assert.Equal(t, &expectedProject, p)
		}},
		{"when a custom project exists with only staged rules, status is edits-pending", func(t *testing.T) {
			ctx := context.Background()
			p := insertTestProject(t, db, "foo", "my foo project", storage.Custom)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-1", p.ID, storage.Node, false)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-2", p.ID, storage.Node, false)

			resp, err := store.GetProject(ctx, p.ID)
			require.NoError(t, err)

			p.Status = storage.EditsPending.String()
			assert.Equal(t, &p, resp)
		}},
		{"when a custom project exists with only applied rules, status is applied", func(t *testing.T) {
			ctx := context.Background()
			p := insertTestProject(t, db, "foo", "my foo project", storage.Custom)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-1", p.ID, storage.Node)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-2", p.ID, storage.Node)

			resp, err := store.GetProject(ctx, p.ID)
			require.NoError(t, err)

			p.Status = storage.Applied.String()
			assert.Equal(t, &p, resp)
		}},
		{"when a custom project exists with staged and applied rules (including some deleted rules), status is edits-pending", func(t *testing.T) {
			ctx := context.Background()
			p := insertTestProject(t, db, "foo", "my foo project", storage.Custom)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-1", p.ID, storage.Node, false)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-2", p.ID, storage.Node, true)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-1", p.ID, storage.Node)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-2", p.ID, storage.Node)

			resp, err := store.GetProject(ctx, p.ID)
			require.NoError(t, err)

			p.Status = storage.EditsPending.String()
			assert.Equal(t, &p, resp)
		}},
		{"when a custom project exists with deleted rules and applied rules, status is edits-pending", func(t *testing.T) {
			ctx := context.Background()
			p := insertTestProject(t, db, "foo", "my foo project", storage.Custom)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-1", p.ID, storage.Node, true)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-2", p.ID, storage.Node, true)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-1", p.ID, storage.Node)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-2", p.ID, storage.Node)

			resp, err := store.GetProject(ctx, p.ID)
			require.NoError(t, err)

			p.Status = storage.EditsPending.String()
			assert.Equal(t, &p, resp)
		}},
		{"when a custom project exists with only deleted rules, status is edits-pending", func(t *testing.T) {
			ctx := context.Background()
			p := insertTestProject(t, db, "foo", "my foo project", storage.Custom)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-1", p.ID, storage.Node, true)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-2", p.ID, storage.Node, true)

			resp, err := store.GetProject(ctx, p.ID)
			require.NoError(t, err)

			p.Status = storage.EditsPending.String()
			assert.Equal(t, &p, resp)
		}},
		{"when a custom project exists with a project filter of *, returns that project", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{constants.AllProjectsExternalID})

			p, err := store.GetProject(ctx, "foo")
			require.NoError(t, err)
			expectedProject := storage.Project{
				ID:     "foo",
				Name:   "my foo project",
				Type:   storage.Custom,
				Status: storage.NoRules.String(),
			}
			assert.Equal(t, &expectedProject, p)
		}},
		{"when a custom project exists but the project filter does not overlap, return NotFoundErr", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "foo", "my foo project", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{"wrong", "project"})

			p, err := store.GetProject(ctx, "foo")
			assert.Equal(t, storage.ErrNotFound, err)
			assert.Nil(t, p)
		}},
	}
	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		db.Flush(t)
	}
}

func TestDeleteProject(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"returns project not found error with empty database", func(t *testing.T) {
			ctx := context.Background()
			err := store.DeleteProject(ctx, "test-project")
			assert.Error(t, err)
			assert.Equal(t, storage.ErrNotFound, err)
		}},
		{"returns project not found with several projects in database", func(t *testing.T) {
			ctx := context.Background()
			insertTestProject(t, db, "my-id-1", "name", storage.Custom)
			insertTestProject(t, db, "my-id-2", "name", storage.Custom)
			insertTestProject(t, db, "my-id-3", "name", storage.Custom)

			err := store.DeleteProject(ctx, "test-project")
			assert.Equal(t, storage.ErrNotFound, err)
		}},
		{"when a policy contains a single statement and that statement contains a single project, on project deletion, the statement and policy are deleted", func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			polID := insertTestPolicy(t, db, "testpolicy")
			sID0 := insertTestStatement(t, db,
				polID, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			insertStatementProject(t, db, sID0, project1.ID)

			polIDOther := insertTestPolicy(t, db, "testpolicy2")
			sID0Other := insertTestStatement(t, db,
				polIDOther, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			sID1Other := insertTestStatement(t, db,
				polIDOther, "deny", "", []string{"compliance:profiles:download", "compliance:profiles:delete"}, []string{"compliance:profiles"})
			insertStatementProject(t, db, sID0Other, project2.ID)
			insertStatementProject(t, db, sID1Other, project2.ID)

			assertPolicyChange(t, store, func() {
				err = store.DeleteProject(ctx, project1.ID)
				require.NoError(t, err)
			})

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, project1.ID))
			assertEmpty(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(statementWithID, sID0))

			assertOne(t, db.QueryRow(policyWithID, polIDOther))
			assertOne(t, db.QueryRow(statementWithID, sID0Other))
			assertOne(t, db.QueryRow(statementWithID, sID1Other))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1) AND statement_id=$2`, project2.ID, sID0Other))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1) AND statement_id=$2`, project2.ID, sID1Other))
		}},
		{"when a policy contains multiple statements and those statements contain a single project, on project deletion, the statement and policy are deleted", func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			polID := insertTestPolicy(t, db, "testpolicy")
			sID0 := insertTestStatement(t, db,
				polID, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			sID1 := insertTestStatement(t, db,
				polID, "deny", "", []string{"compliance:profiles:download", "compliance:profiles:delete"}, []string{"compliance:profiles"})
			insertStatementProject(t, db, sID0, project1.ID)
			insertStatementProject(t, db, sID1, project1.ID)

			polIDOther := insertTestPolicy(t, db, "testpolicy2")
			sID0Other := insertTestStatement(t, db,
				polIDOther, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			sID1Other := insertTestStatement(t, db,
				polIDOther, "deny", "", []string{"compliance:profiles:download", "compliance:profiles:delete"}, []string{"compliance:profiles"})
			insertStatementProject(t, db, sID0Other, project2.ID)
			insertStatementProject(t, db, sID1Other, project2.ID)

			assertPolicyChange(t, store, func() {
				err = store.DeleteProject(ctx, project1.ID)
				require.NoError(t, err)
			})

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, project1.ID))
			assertEmpty(t, db.QueryRow(policyWithID, polID))
			assertEmpty(t, db.QueryRow(statementWithID, sID0))
			assertEmpty(t, db.QueryRow(statementWithID, sID1))

			assertOne(t, db.QueryRow(policyWithID, polIDOther))
			assertOne(t, db.QueryRow(statementWithID, sID0Other))
			assertOne(t, db.QueryRow(statementWithID, sID1Other))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1) AND statement_id=$2`, project2.ID, sID0Other))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1) AND statement_id=$2`, project2.ID, sID1Other))
		}},
		{"when a policy contains multiple statements and those statements contain different projects, on one project deletion, one statement is deleted and the policy remains", func(t *testing.T) {
			ctx := context.Background()
			project1 := storage.Project{
				ID:   "project-1",
				Name: "name1",
				Type: storage.Custom,
			}
			_, err := store.CreateProject(ctx, &project1, true)
			project2 := storage.Project{
				ID:   "project-2",
				Name: "name2",
				Type: storage.Custom,
			}
			_, err = store.CreateProject(ctx, &project2, true)
			require.NoError(t, err)

			polID := insertTestPolicy(t, db, "testpolicy")
			sID0 := insertTestStatement(t, db,
				polID, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			sID1 := insertTestStatement(t, db,
				polID, "deny", "", []string{"compliance:profiles:download", "compliance:profiles:delete"}, []string{"compliance:profiles"})
			insertStatementProject(t, db, sID0, project1.ID)
			insertStatementProject(t, db, sID1, project2.ID)

			polIDOther := insertTestPolicy(t, db, "testpolicy2")
			sID0Other := insertTestStatement(t, db,
				polIDOther, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			sID1Other := insertTestStatement(t, db,
				polIDOther, "deny", "", []string{"compliance:profiles:download", "compliance:profiles:delete"}, []string{"compliance:profiles"})
			insertStatementProject(t, db, sID0Other, project2.ID)
			insertStatementProject(t, db, sID1Other, project2.ID)

			assertPolicyChange(t, store, func() {
				err = store.DeleteProject(ctx, project1.ID)
				require.NoError(t, err)
			})

			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, project1.ID))
			assertEmpty(t, db.QueryRow(statementWithID, sID0))

			assertOne(t, db.QueryRow(policyWithID, polID))
			assertOne(t, db.QueryRow(statementWithID, sID1))
			assertOne(t, db.QueryRow(statementWithID, sID1))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1) AND statement_id=$2`, project2.ID, sID1))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1) AND statement_id=$2`, project2.ID, sID1))

			assertOne(t, db.QueryRow(policyWithID, polIDOther))
			assertOne(t, db.QueryRow(statementWithID, sID0Other))
			assertOne(t, db.QueryRow(statementWithID, sID1Other))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1) AND statement_id=$2`, project2.ID, sID0Other))
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_statement_projects WHERE project_id=project_db_id($1) AND statement_id=$2`, project2.ID, sID1Other))
		}},
		{"deletes project with one project in database", func(t *testing.T) {
			ctx := context.Background()
			proj := insertTestProject(t, db, "test-project", "name", storage.Custom)

			err := store.DeleteProject(ctx, "test-project")

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, proj.ID))
		}},
		{"when the project is already deleted but is in the graveyard, the function succeeds", func(t *testing.T) {
			ctx := context.Background()
			projID := "test-project"
			_, err := db.Exec(`INSERT INTO iam_projects_graveyard (id) values ($1)`, projID)
			require.NoError(t, err)

			err = store.DeleteProject(ctx, projID)

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, projID))
		}},
		{"deletes project with several projects in database", func(t *testing.T) {
			ctx := context.Background()
			proj := insertTestProject(t, db, "test-project", "name", storage.Custom)
			insertTestProject(t, db, "my-id-1", "name", storage.Custom)
			insertTestProject(t, db, "my-id-2", "name", storage.Custom)
			insertTestProject(t, db, "my-id-3", "name", storage.Custom)

			err := store.DeleteProject(ctx, "test-project")

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, proj.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_projects`))
		}},
		{"deletes project with several projects in database with a project filter", func(t *testing.T) {
			ctx := context.Background()
			proj := insertTestProject(t, db, "test-project", "name", storage.Custom)
			insertTestProject(t, db, "my-id-1", "name", storage.Custom)
			insertTestProject(t, db, "my-id-2", "name", storage.Custom)
			insertTestProject(t, db, "my-id-3", "name", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{"foo", "test-project"})

			err := store.DeleteProject(ctx, "test-project")

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, proj.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_projects`))
		}},
		{"deletes project with several projects in database with a project filter of *", func(t *testing.T) {
			ctx := context.Background()
			proj := insertTestProject(t, db, "test-project", "name", storage.Custom)
			insertTestProject(t, db, "my-id-1", "name", storage.Custom)
			insertTestProject(t, db, "my-id-2", "name", storage.Custom)
			insertTestProject(t, db, "my-id-3", "name", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{constants.AllProjectsExternalID})

			err := store.DeleteProject(ctx, "test-project")

			require.NoError(t, err)
			assertEmpty(t, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, proj.ID))
			assertCount(t, 3, db.QueryRow(`SELECT count(*) FROM iam_projects`))
		}},
		{"returns 'not found' when the project filter excludes the project in question", func(t *testing.T) {
			ctx := context.Background()
			proj := insertTestProject(t, db, "test-project", "name", storage.Custom)
			insertTestProject(t, db, "my-id-1", "name", storage.Custom)
			insertTestProject(t, db, "my-id-2", "name", storage.Custom)
			insertTestProject(t, db, "my-id-3", "name", storage.Custom)

			ctx = insertProjectsIntoContext(ctx, []string{"my-id-1", "my-id-2"})

			err := store.DeleteProject(ctx, "test-project")
			assert.Equal(t, storage.ErrNotFound, err)
			assertCount(t, 1, db.QueryRow(`SELECT count(*) FROM iam_projects WHERE id=$1`, proj.ID))
			assertCount(t, 4, db.QueryRow(`SELECT count(*) FROM iam_projects`))
		}},
	}
	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})
	for _, test := range cases {
		t.Run(test.desc, test.f)
		db.Flush(t)
	}
}

func TestListProjects(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"when no projects exists, returns empty list", func(t *testing.T) {
			ctx := context.Background()
			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)
			assert.Empty(t, ps)
		}},
		{"when multiple projects exist, filter based on projects lists", func(t *testing.T) {
			ctx := context.Background()
			p1 := insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			p2 := insertTestProject(t, db, "bar", "my bar project", storage.Custom)
			insertTestProject(t, db, "baz", "my baz project", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{"foo", "bar"})

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)
			expectedProjects := []*storage.Project{&p1, &p2}

			assert.ElementsMatch(t, expectedProjects, ps)
		}},
		{"when multiple projects exist, returns all projects when * filter passed", func(t *testing.T) {
			ctx := context.Background()
			p1 := insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			p2 := insertTestProject(t, db, "bar", "my bar project", storage.Custom)
			p3 := insertTestProject(t, db, "baz", "my baz project", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{constants.AllProjectsExternalID})

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)
			expectedProjects := []*storage.Project{&p1, &p2, &p3}

			assert.ElementsMatch(t, expectedProjects, ps)
		}},
		{"returns rule status for projects for none, staged, and applied individually", func(t *testing.T) {
			ctx := context.Background()
			p1 := insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-1", p1.ID, storage.Node, false)
			p2 := insertTestProject(t, db, "bar", "my bar project", storage.Custom)
			insertAppliedRuleWithMultipleConditions(t, db, "staged-rule-2", p2.ID, storage.Node)
			p3 := insertTestProject(t, db, "baz", "my baz project", storage.Custom)

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)

			p1.Status = storage.EditsPending.String()
			p2.Status = storage.Applied.String()
			p3.Status = storage.NoRules.String()
			expectedProjects := []*storage.Project{&p1, &p2, &p3}
			assert.ElementsMatch(t, expectedProjects, ps)
		}},
		{"returns rule status of 'edits-pending' for project with both staged and applied rules", func(t *testing.T) {
			ctx := context.Background()
			p1 := insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-1", p1.ID, storage.Node, false)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-1", p1.ID, storage.Node)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-2", p1.ID, storage.Node)

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)

			p1.Status = storage.EditsPending.String()
			expectedProjects := []*storage.Project{&p1}
			assert.ElementsMatch(t, expectedProjects, ps)
		}},
		{"returns rule status of 'edits-pending' for project with staged new rules, staged deletes, and applied rules", func(t *testing.T) {
			ctx := context.Background()
			p1 := insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			insertStagedRuleWithMultipleConditions(t, db, "staged-rule-1", p1.ID, storage.Node, false)
			insertStagedRuleWithMultipleConditions(t, db, "rule-1", p1.ID, storage.Node, true)
			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", p1.ID, storage.Node)
			insertAppliedRuleWithMultipleConditions(t, db, "applied-rule-2", p1.ID, storage.Node)

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)

			p1.Status = storage.EditsPending.String()
			expectedProjects := []*storage.Project{&p1}
			assert.ElementsMatch(t, expectedProjects, ps)
		}},
		{"returns rule status of 'edits-pending' for project with both staged deletes applied rules", func(t *testing.T) {
			ctx := context.Background()
			p1 := insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			insertStagedRuleWithMultipleConditions(t, db, "rule-1", p1.ID, storage.Node, true)
			insertStagedRuleWithMultipleConditions(t, db, "rule-2", p1.ID, storage.Node, true)
			insertAppliedRuleWithMultipleConditions(t, db, "rule-1", p1.ID, storage.Node)
			insertAppliedRuleWithMultipleConditions(t, db, "rule-2", p1.ID, storage.Node)

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)

			p1.Status = storage.EditsPending.String()
			expectedProjects := []*storage.Project{&p1}
			assert.ElementsMatch(t, expectedProjects, ps)
		}},
		{"returns rule status of 'edits-pending' for project with staged deletes", func(t *testing.T) {
			ctx := context.Background()
			p1 := insertTestProject(t, db, "foo", "my foo project", storage.ChefManaged)
			insertStagedRuleWithMultipleConditions(t, db, "rule-1", p1.ID, storage.Node, true)
			insertStagedRuleWithMultipleConditions(t, db, "rule-2", p1.ID, storage.Node, true)

			ps, err := store.ListProjects(ctx)
			require.NoError(t, err)

			p1.Status = storage.EditsPending.String()
			expectedProjects := []*storage.Project{&p1}
			assert.ElementsMatch(t, expectedProjects, ps)
		}},
	}
	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		db.Flush(t)
	}
}

func TestEnsureNoProjectsMissing(t *testing.T) {
	store, db, _, _, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()
	ctx := context.Background()

	cases := []struct {
		desc string
		f    func(*testing.T)
	}{
		{"when there are no projects, returns ProjectsMissingError", func(t *testing.T) {
			err := store.EnsureNoProjectsMissing(ctx, []string{"missing"})
			assert.Error(t, err)
			_, correctError := err.(*projectassignment.ProjectsMissingError)
			assert.True(t, correctError)
		}},
		{"when some projects don't exist, returns ProjectsMissingError", func(t *testing.T) {
			insertTestProject(t, db, "proj0", "test project 0", storage.Custom)
			insertTestProject(t, db, "proj1", "test project 1", storage.Custom)
			err := store.EnsureNoProjectsMissing(ctx, []string{"proj0", "missing"})
			assert.Error(t, err)
			_, correctError := err.(*projectassignment.ProjectsMissingError)
			assert.True(t, correctError)
		}},
		{"when all projects exist, return nil", func(t *testing.T) {
			insertTestProject(t, db, "proj0", "test project 0", storage.Custom)
			insertTestProject(t, db, "proj1", "test project 1", storage.Custom)
			err := store.EnsureNoProjectsMissing(ctx, []string{"proj0", "proj1"})
			assert.NoError(t, err)
		}},
	}

	rand.Shuffle(len(cases), func(i, j int) {
		cases[i], cases[j] = cases[j], cases[i]
	})

	for _, test := range cases {
		t.Run(test.desc, test.f)
		db.Flush(t)
	}
}
