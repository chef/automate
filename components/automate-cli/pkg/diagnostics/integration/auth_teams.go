package integration

import (
	"fmt"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const createTeamTemplate = `
{
	"id": "{{ .ID }}",
	"name": "diagnostics integration test team"
}
`

// TeamInfo contains information about a team. This is returned when
// creating a team and getting a team
type TeamInfo struct {
	Team struct {
		ID       string   `json:"id"`
		Name     string   `json:"name"`
		Projects []string `json:"projects"`
	} `json:"team"`
}

type authTeamSave struct {
	ID   string `json:"id"`
	Name string `json:"name"`
}

// CreateRandomTeam creates a team with a random name
func CreateRandomTeam(tstCtx diagnostics.TestContext) (*TeamInfo, error) {
	teamInfo := TeamInfo{}
	err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			"/apis/iam/v2/teams",
			lbrequest.WithMethod("POST"),
			lbrequest.WithJSONStringTemplateBody(createTeamTemplate, struct {
				ID string
			}{
				ID: TimestampName(),
			}),
		)).WithValue(&teamInfo)

	if err != nil {
		return nil, errors.Wrap(err, "Could not create team")
	}
	return &teamInfo, nil
}

// GetTeam gets the team with the given id
func GetTeam(tstCtx diagnostics.TestContext, id string) (*TeamInfo, error) {
	teamInfo := TeamInfo{}
	err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			fmt.Sprintf("/apis/iam/v2/teams/%s", id),
		)).WithValue(&teamInfo)

	if err != nil {
		return nil, errors.Wrap(err, "Could fetch team")
	}
	return &teamInfo, nil
}

// DeleteTeam deletes the team with the given id
func DeleteTeam(tstCtx diagnostics.TestContext, id string) error {
	err := MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			fmt.Sprintf("/apis/iam/v2/teams/%s", id),
			lbrequest.WithMethod("DELETE"),
		)).Error()

	if err != nil {
		return errors.Wrap(err, "Could fetch team")
	}
	return nil
}

// CreateAuthTeamsDiagnostic creates a diagnostic for auth teams
func CreateAuthTeamsDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "auth-teams",
		Tags: diagnostics.Tags{"auth", "iam-v1"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			teamInfo, err := CreateRandomTeam(tstCtx)
			if err != nil {
				return err
			}
			tstCtx.SetValue("auth-teams", &authTeamSave{
				ID:   teamInfo.Team.ID,
				Name: teamInfo.Team.Name,
			})
			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := authTeamSave{}
			err := tstCtx.GetValue("auth-teams", &loaded)
			require.NoError(tstCtx, err, "Could not load generated context")
			teamInfo, err := GetTeam(tstCtx, loaded.ID)
			require.NoError(tstCtx, err)
			assert.Equal(tstCtx, loaded.ID, teamInfo.Team.ID, "Received unexpected team ID")
			assert.Equal(tstCtx, loaded.Name, teamInfo.Team.Name, "Received unexpected team Name")
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := authTeamSave{}
			err := tstCtx.GetValue("auth-teams", &loaded)
			if err != nil {
				return errors.Wrap(err, "Could not load generated context")
			}
			return DeleteTeam(tstCtx, loaded.ID)
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateAuthTeamsDiagnostic(),
	)
}
