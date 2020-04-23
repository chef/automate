package postgres_test

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/components/authz-service/testhelpers"
)

func TestListPolicyMembers(t *testing.T) {
	store, db, _, prngSeed, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()
	ctx := context.Background()

	cases := map[string]func(*testing.T){
		"empty database": func(t *testing.T) {
			resp, err := store.ListPolicyMembers(ctx, genSimpleID(t, prngSeed))
			assert.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrNotFound, err)
		},
		"contains policies but looking for a different one": func(t *testing.T) {
			insertTestPolicy(t, db, "testpolicy")

			resp, err := store.ListPolicyMembers(ctx, genSimpleID(t, prngSeed))

			assert.Error(t, err)
			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrNotFound, err)
		},
		"policy with no members": func(t *testing.T) {
			polID := insertTestPolicy(t, db, "testpolicy")

			members, err := store.ListPolicyMembers(ctx, polID)

			require.NoError(t, err)
			assert.Empty(t, members)
		},
		"policy with a single member": func(t *testing.T) {
			polID := insertTestPolicy(t, db, "testpolicy")
			member := insertTestPolicyMember(t, db, polID, "user:local:albertine")

			members, err := store.ListPolicyMembers(ctx, polID)
			require.NoError(t, err)
			assert.Equal(t, []storage.Member{member}, members)
		},
		"policy with multiple members and statements returns alphabetically by name": func(t *testing.T) {
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestStatement(t, db,
				polID, "allow", "", []string{"iam:users:delete', 'iam:users:create"}, []string{"iam:users"})
			insertTestStatement(t, db,
				polID, "deny", "", []string{"infra:nodes:delete", "infra:nodes:rerun"}, []string{"infra:nodes"})

			member0 := insertTestPolicyMember(t, db, polID, "user:local:c")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:a")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:b")

			members, err := store.ListPolicyMembers(ctx, polID)
			require.NoError(t, err)
			assert.Equal(t, []storage.Member{member1, member2, member0}, members)
		},
		"when the policy's projects and the project filter intersect, list members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")

			ctx = insertProjectsIntoContext(ctx, []string{projID1})
			resp, err := store.ListPolicyMembers(ctx, polID)

			require.NoError(t, err)
			assert.Equal(t, []storage.Member{member1, member2}, resp)
		},
		"when the * project filter is passed, list members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")

			ctx = insertProjectsIntoContext(ctx, []string{constants.AllProjectsExternalID})
			resp, err := store.ListPolicyMembers(ctx, polID)

			require.NoError(t, err)
			assert.Equal(t, []storage.Member{member1, member2}, resp)
		},
		"when the policy has no projects and (unassigned) is in the projects filter, list members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID1, constants.UnassignedProjectID})
			resp, err := store.ListPolicyMembers(ctx, polID)

			require.NoError(t, err)
			assert.Equal(t, []storage.Member{member1, member2}, resp)
		},
		"when the policy's projects and projects filter do not intersect, return NotFound": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			projID2 := "team-montag"
			insertTestProject(t, db, projID2, "we like dags", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID2, constants.UnassignedProjectID})
			resp, err := store.ListPolicyMembers(ctx, polID)

			assert.Nil(t, resp)
			assert.Equal(t, storage.ErrNotFound, err)
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestAddPolicyMembers(t *testing.T) {
	store, db, _, prngSeed, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"fails to add with ErrNotFound when the database is empty and members are added to a non-existent policy": func(t *testing.T) {
			ctx := context.Background()
			wrongPolID := genSimpleID(t, prngSeed)
			member := genMember(t, "user:local:test")

			assertNoPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, wrongPolID, []storage.Member{member})

				assert.Error(t, err)
				assert.Nil(t, resp)
				assert.Equal(t, storage.ErrNotFound, err)
			})
		},
		"fails to add with ErrNotFound when a policy exists but members are added to a non-existent policy": func(t *testing.T) {
			ctx := context.Background()
			insertTestPolicy(t, db, "testpolicy")
			member := genMember(t, "user:local:fred")

			assertNoPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, genSimpleID(t, prngSeed), []storage.Member{member})

				assert.Error(t, err)
				assert.Nil(t, resp)
				assert.Equal(t, storage.ErrNotFound, err)
			})
		},
		"adding one member to a policy with NO members results in member being added": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member := genMember(t, "user:local:fred")
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))

			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.NotEmpty(t, resp)
			})

			assertCount(t, 1, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 1, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member})
		},
		"adding several members to a policy with NO members results in members being added": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "user:local:mary")
			member3 := genMember(t, "user:local:max")
			member4 := genMember(t, "user:local:ellen")
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))

			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member1, member2, member3, member4})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.NotEmpty(t, resp)
			})

			assertCount(t, 4, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 4, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2, member3, member4})
		},
		"adding one member to a policy with SOME members succeeds": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2})
			member3 := genMember(t, "user:local:max")

			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member3})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.NotEmpty(t, resp)
			})

			assertCount(t, 3, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 3, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2, member3})
		},
		"adding several members to a policy with SOME members succeeds": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2})
			member3 := genMember(t, "user:local:max")
			member4 := genMember(t, "user:local:ellen")
			member5 := genMember(t, "user:local:barry")
			member6 := genMember(t, "user:local:jack")

			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member3, member4, member5, member6})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.NotEmpty(t, resp)
			})

			assertCount(t, 6, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 6, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2, member3, member4, member5, member6})
		},
		"adding same member more than once w/ single request doesn't result in duplicate policy membership": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
			member3 := genMember(t, "user:local:ellen")
			repeatMember3 := genMember(t, "user:local:ellen")

			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member3, repeatMember3})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 3, len(resp))
			})

			assertCount(t, 3, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 3, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2, member3})
		},
		"adding same member more than once w/ duplicate request doesn't result in duplicate policy membership": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
			member3 := genMember(t, "user:local:ellen")

			assertPolicyChange(t, store, func() {
				// add ellen to the policy
				resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member3})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 3, len(resp))
			})

			assertCount(t, 3, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 3, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2, member3})

			// This isn't quite right. It's better if we don't update the change id, but that requires
			// the transaction to fail
			assertPolicyChange(t, store, func() {
				// attempt to add ellen again in a duplicate request
				resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member3})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 3, len(resp))
			})

			assertCount(t, 3, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 3, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2, member3})
		},
		"adding subset of members that's already part of a different policy succeeds": func(t *testing.T) {
			ctx := context.Background()
			polID1 := insertTestPolicy(t, db, "testpolicy1")
			polID2 := insertTestPolicy(t, db, "testpolicy2")
			member1 := insertTestPolicyMember(t, db, polID1, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID1, "user:local:mary")
			member3 := insertTestPolicyMember(t, db, polID1, "user:local:ellen")
			member4 := genMember(t, "user:local:max")
			assertCount(t, 3, db.QueryRow(policyMembersByPolicyID, polID1))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID2))

			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID2, []storage.Member{member1, member2, member3, member4})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 4, len(resp))
			})

			assertMembers(t, db, polID1, []storage.Member{member1, member2, member3})
			assertMembers(t, db, polID2, []storage.Member{member1, member2, member3, member4})
			assertCount(t, 3, db.QueryRow(policyMembersByPolicyID, polID1))
			assertCount(t, 4, db.QueryRow(policyMembersByPolicyID, polID2))
			assertCount(t, 4, db.QueryRow(membersCount))
		},
		"adding members where the members match the existing policy membership results in no new members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, []storage.Member{member1, member2})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 2, len(resp))
			})

			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2})
		},
		"adding member that's already a member of a different policy doesn't result in additional iam_members entry": func(t *testing.T) {
			ctx := context.Background()
			polID1 := insertTestPolicy(t, db, "testpolicy1")
			polID2 := insertTestPolicy(t, db, "testpolicy2")
			member := insertTestPolicyMember(t, db, polID1, "user:local:fred")
			assertOne(t, db.QueryRow(membersCount))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, polID1))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID2))

			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID2, []storage.Member{member})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 1, len(resp))
			})

			assertMembers(t, db, polID1, []storage.Member{member})
			assertMembers(t, db, polID2, []storage.Member{member})
			assertOne(t, db.QueryRow(membersCount))
			assertOne(t, db.QueryRow(policyMembersFull, polID1, member.Name))
			assertOne(t, db.QueryRow(policyMembersFull, polID2, member.Name))
		},
		"when the policy's projects and the project filter intersect, add members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			member1 := genMember(t, "user:local:max")
			member2 := genMember(t, "user:local:sue")
			members := []storage.Member{member1, member2}

			assertPolicyChange(t, store, func() {
				ctx = insertProjectsIntoContext(ctx, []string{projID1})
				resp, err := store.AddPolicyMembers(ctx, polID, members)
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertCount(t, 4, db.QueryRow(policyMembersByPolicyID, polID))
		},
		"when the * project filter is passed, add members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			member1 := genMember(t, "user:local:max")
			member2 := genMember(t, "user:local:sue")
			members := []storage.Member{member1, member2}

			ctx = insertProjectsIntoContext(ctx, []string{constants.AllProjectsExternalID})
			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, members)
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertCount(t, 4, db.QueryRow(policyMembersByPolicyID, polID))
		},
		"when the policy has no projects and (unassigned) is in the projects filter, add members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			member1 := genMember(t, "user:local:max")
			member2 := genMember(t, "user:local:sue")
			members := []storage.Member{member1, member2}

			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID1, constants.UnassignedProjectID})
			assertPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, members)
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertCount(t, 4, db.QueryRow(policyMembersByPolicyID, polID))
		},
		"when the policy's projects and projects filter do not intersect, return NotFound": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			member1 := genMember(t, "user:local:max")
			member2 := genMember(t, "user:local:sue")
			members := []storage.Member{member1, member2}

			projID2 := "team-montag"
			insertTestProject(t, db, projID2, "we like dags", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID2, constants.UnassignedProjectID})
			assertNoPolicyChange(t, store, func() {
				resp, err := store.AddPolicyMembers(ctx, polID, members)
				assert.Nil(t, resp)
				assert.Equal(t, storage.ErrNotFound, err)
			})
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestReplacePolicyMembers(t *testing.T) {
	store, db, _, prngSeed, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"empty database": func(t *testing.T) {
			ctx := context.Background()
			wrongPolID := genSimpleID(t, prngSeed)
			member := genMember(t, "user:local:test")

			assertNoPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, wrongPolID, []storage.Member{member})
				assert.Error(t, err)
				assert.Empty(t, resp)
			})
		},
		"policy not found reports an error": func(t *testing.T) {
			ctx := context.Background()
			insertTestPolicy(t, db, "testpolicy")
			member1 := genMember(t, "user:local:fred")

			assertNoPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, genSimpleID(t, prngSeed), []storage.Member{member1})

				assert.Error(t, err)
				assert.Nil(t, resp)
				assert.Equal(t, storage.ErrNotFound, err)
			})
		},
		"updating policy with NO members to SOME members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "user:local:mary")
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))

			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{member1, member2})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{member1, member2})
		},
		"updating policy with SOME members to NO members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			// deleting last use of member does NOT delete member
			assertCount(t, 2, db.QueryRow(membersCount))
		},
		"updating policy with SOME members to NEW members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			polMember1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			polMember2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
			member1 := genMember(t, "user:local:foo")
			member2 := genMember(t, "user:local:bar")
			member3 := genMember(t, "team:saml:gophers")
			member4 := genMember(t, "team:saml:editors")
			members := []storage.Member{member1, member2, member3, member4}

			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, members)
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertCount(t, len(members), db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, len(members)+2, db.QueryRow(membersCount))

			// old members no longer associated with policy
			assertEmpty(t, db.QueryRow(policyMembersFull, polID, polMember1.Name))
			assertEmpty(t, db.QueryRow(policyMembersFull, polID, polMember2.Name))

			// new members
			assertMembers(t, db, polID, members)
		},
		"updating policy with SOME members to NEW members with some REUSED members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			polMember1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			polMember2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			polMember3 := insertTestPolicyMember(t, db, polID, "team:local:friends")
			assertCount(t, 3, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 3, db.QueryRow(membersCount))
			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "user:local:mary")
			member3 := genMember(t, "team:saml:gophers")
			member4 := genMember(t, "team:saml:editors")
			members := []storage.Member{member1, member2, member3, member4}

			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, members)
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertCount(t, 4, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 5, db.QueryRow(membersCount))

			// re-used members plus new members
			assertMembers(t, db, polID, []storage.Member{polMember1, polMember2, member3, member4})

			// member still exists but disassociated from policy
			assertOne(t, db.QueryRow(`SELECT count(*) FROM iam_members WHERE name=$1`, polMember3.Name))
			assertEmpty(t, db.QueryRow(policyMembersFull, polID, polMember3.Name))
		},
		"updating policy by ADDING member from ANOTHER policy": func(t *testing.T) {
			ctx := context.Background()
			otherPolID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, otherPolID, "user:local:originaluser")

			polID := insertTestPolicy(t, db, "otherTestPolicy")
			member := genMember(t, "user:local:originaluser")

			// baseline: member is in just one policy
			assertOne(t, db.QueryRow(membersCount))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, otherPolID))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))

			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{member})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			// now still just one member but in two policies
			assertOne(t, db.QueryRow(membersCount))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, otherPolID))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, polID))
		},
		"updating policy by REMOVING member from ANOTHER policy": func(t *testing.T) {
			ctx := context.Background()
			otherPolID := insertTestPolicy(t, db, "testpolicy")
			member := insertTestPolicyMember(t, db, otherPolID, "user:local:originaluser")

			polID := insertTestPolicy(t, db, "otherTestPolicy")
			_, err := db.Query(`INSERT INTO iam_policy_members (policy_id, member_id) values(policy_db_id($1), member_db_id($2))`, polID, member.Name)
			require.NoError(t, err)

			// baseline: member is in two policies
			assertOne(t, db.QueryRow(membersCount))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, otherPolID))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, polID))

			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			// now member remains in just one policy
			assertOne(t, db.QueryRow(membersCount))
			assertOne(t, db.QueryRow(policyMembersByPolicyID, otherPolID))
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
		},
		"when the policy's projects and the project filter intersect, replace members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			ctx = insertProjectsIntoContext(ctx, []string{projID1})
			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			// deleting last use of member does NOT delete member
			assertCount(t, 2, db.QueryRow(membersCount))
		},
		"when the * project filter is passed, replace members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			ctx = insertProjectsIntoContext(ctx, []string{constants.AllProjectsExternalID})
			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			// deleting last use of member does NOT delete member
			assertCount(t, 2, db.QueryRow(membersCount))
		},
		"when the policy has no projects and (unassigned) is in the projects filter, replace members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID1, constants.UnassignedProjectID})
			assertPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			// deleting last use of member does NOT delete member
			assertCount(t, 2, db.QueryRow(membersCount))
		},
		"when the policy's projects and projects filter do not intersect, return NotFound": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			projID2 := "team-montag"
			insertTestProject(t, db, projID2, "we like dags", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID2, constants.UnassignedProjectID})
			assertNoPolicyChange(t, store, func() {
				resp, err := store.ReplacePolicyMembers(ctx, polID, []storage.Member{})
				assert.Nil(t, resp)
				assert.Equal(t, storage.ErrNotFound, err)
			})
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}

func TestRemovePolicyMembers(t *testing.T) {
	store, db, _, prngSeed, _ := testhelpers.SetupTestDB(t)
	defer db.CloseDB(t)
	defer store.Close()

	cases := map[string]func(*testing.T){
		"empty database": func(t *testing.T) {
			ctx := context.Background()
			wrongPolID := genSimpleID(t, prngSeed)
			member := genMember(t, "user:local:test")

			assertNoPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, wrongPolID, []storage.Member{member})
				assert.Error(t, err)
				assert.Empty(t, resp)
			})
		},
		"policy not found reports an error": func(t *testing.T) {
			ctx := context.Background()
			insertTestPolicy(t, db, "testpolicy")
			member := genMember(t, "user:local:fred")

			assertNoPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, genSimpleID(t, prngSeed), []storage.Member{member})

				assert.Error(t, err)
				assert.Nil(t, resp)
				assert.Equal(t, storage.ErrNotFound, err)
			})
		},
		"removing members from policy with NO members results in an empty member list": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member := genMember(t, "user:local:fred")
			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))

			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Empty(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertEmpty(t, db.QueryRow(membersCount))
		},
		"removing members from policy with SOME members to now have NO members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "user:local:mary")
			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1, member2})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Empty(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
		},
		"removing repeat members from policy with SOME members to now have LESS members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			remainingMember := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "user:local:fred")
			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1, member2})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 1, len(resp))
				require.Contains(t, resp, remainingMember)
			})

			assertOne(t,
				db.QueryRow(policyMembersFull,
					polID, remainingMember.Name))
			assertCount(t, 2, db.QueryRow(membersCount))
		},
		"removing members from policy with ONE member to now have NO members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			assertCount(t, 1, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 1, db.QueryRow(membersCount))

			member1 := genMember(t, "user:local:fred")
			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Empty(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 1, db.QueryRow(membersCount))
		},
		"removing only non-members from policy results in no change to policy membership": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{})
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 2, len(resp))
				require.Contains(t, resp, member1)
				require.Contains(t, resp, member2)
			})

			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
		},
		"removing members from policy with SOME members to now have less members with some ignored": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			insertTestPolicyMember(t, db, polID, "user:local:fred")
			insertTestPolicyMember(t, db, polID, "user:local:mary")
			polMember3 := insertTestPolicyMember(t, db, polID, "user:local:charmander")
			polMember4 := insertTestPolicyMember(t, db, polID, "user:local:squirtle")
			polMember5 := insertTestPolicyMember(t, db, polID, "user:local:bulbasaur")
			assertCount(t, 5, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 5, db.QueryRow(membersCount))
			member1 := genMember(t, "user:local:fred")
			member2 := genMember(t, "team:saml:notfound1")
			member3 := genMember(t, "user:local:mary")
			member4 := genMember(t, "team:saml:notfound2")
			members := []storage.Member{member1, member2, member3, member4}

			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, members)
				require.NoError(t, err)
				require.NotNil(t, resp)
				require.Equal(t, 3, len(resp))
				require.Contains(t, resp, polMember3)
				require.Contains(t, resp, polMember4)
				require.Contains(t, resp, polMember5)
			})

			assertCount(t, 3, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 5, db.QueryRow(membersCount))
			assertMembers(t, db, polID, []storage.Member{polMember3, polMember4, polMember5})
		},
		"when the policy's projects and the project filter intersect, remove members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))
			members := []storage.Member{member1, member2}

			ctx = insertProjectsIntoContext(ctx, []string{projID1})
			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, members)
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
		},
		"when the * project filter is passed, remove members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			ctx = insertProjectsIntoContext(ctx, []string{constants.AllProjectsExternalID})
			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1, member2})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
		},
		"when the policy has no projects and (unassigned) is in the projects filter, remove members": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID1, constants.UnassignedProjectID})
			assertPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1, member2})
				require.NoError(t, err)
				require.NotNil(t, resp)
			})

			assertEmpty(t, db.QueryRow(policyMembersByPolicyID, polID))
		},
		"when the policy's projects and projects filter do not intersect, return NotFound": func(t *testing.T) {
			ctx := context.Background()
			polID := insertTestPolicy(t, db, "testpolicy")
			projID1 := "team-rocket"
			insertTestProject(t, db, projID1, "blasting off again", storage.Custom)
			insertPolicyProject(t, db, polID, projID1)
			member1 := insertTestPolicyMember(t, db, polID, "user:local:fred")
			member2 := insertTestPolicyMember(t, db, polID, "user:local:mary")
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
			assertCount(t, 2, db.QueryRow(membersCount))

			projID2 := "team-montag"
			insertTestProject(t, db, projID2, "we like dags", storage.Custom)
			ctx = insertProjectsIntoContext(ctx, []string{projID2, constants.UnassignedProjectID})
			assertNoPolicyChange(t, store, func() {
				resp, err := store.RemovePolicyMembers(ctx, polID, []storage.Member{member1, member2})
				assert.Nil(t, resp)
				assert.Equal(t, storage.ErrNotFound, err)
			})
			assertCount(t, 2, db.QueryRow(policyMembersByPolicyID, polID))
		},
	}

	for name, test := range cases {
		t.Run(name, test)
		db.Flush(t)
	}
}
