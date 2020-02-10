package integration

import (
	"fmt"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics/lbrequest"
)

const createV1TeamTemplate = `
{
	"name": "{{ .Name }}",
	"description": "{{ .Name }} V1 diagnostics test team"
}
`

const createV2TeamTemplate = `
{
	"id": "{{ .ID }}",
	"name": "{{ .ID }} V2 diagnostics test team"
}
`

// TeamInfo contains information about a team, icluding v1 and v2 fields. This
// is returned when creating a team and getting a team
type TeamInfo struct {
	IsV2 bool
	Team struct {
		ID          string   `json:"id"`
		Name        string   `json:"name"`
		Description string   `json:"description"`
		Projects    []string `json:"projects"`
	} `json:"team"`
}

type authTeamSave struct {
	ID   string `json:"id"`
	Name string `json:"name"`
	IsV2 bool   `json:"is_v2"`
}

// CreateRandomTeam creates a team with a random identifier
func CreateRandomTeam(tstCtx diagnostics.TestContext) (*TeamInfo, error) {
	var err error
	isV2, err := tstCtx.IsIAMV2()
	if err != nil {
		return nil, err
	}

	teamInfo := TeamInfo{IsV2: isV2}
	if isV2 {
		err = MustJSONDecodeSuccess(
			tstCtx.DoLBRequest(
				"/apis/iam/v2/teams",
				lbrequest.WithMethod("POST"),
				lbrequest.WithJSONStringTemplateBody(createV2TeamTemplate, struct {
					ID string
				}{
					ID: TimestampName(),
				}),
			)).WithValue(&teamInfo)
	} else {
		err = MustJSONDecodeSuccess(
			tstCtx.DoLBRequest(
				"/api/v0/auth/teams",
				lbrequest.WithMethod("POST"),
				lbrequest.WithJSONStringTemplateBody(createV1TeamTemplate, struct {
					Name string
				}{
					Name: TimestampName(),
				}),
			)).WithValue(&teamInfo)
	}

	if err != nil {
		return nil, errors.Wrap(err, "Could not create team")
	}
	return &teamInfo, nil
}

// GetTeam gets the team with the given id
func GetTeam(tstCtx diagnostics.TestContext, id string) (*TeamInfo, error) {
	var err error
	isV2, err := tstCtx.IsIAMV2()
	if err != nil {
		return nil, err
	}

	var reqPath string
	if isV2 {
		reqPath = fmt.Sprintf("/apis/iam/v2/teams/%s", id)
	} else {
		reqPath = fmt.Sprintf("/api/v0/auth/teams/%s", id)
	}

	teamInfo := TeamInfo{}
	err = MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(reqPath)).WithValue(&teamInfo)

	if err != nil {
		return nil, errors.Wrap(err, "Could not fetch team")
	}
	return &teamInfo, nil
}

// DeleteTeam deletes the team with the given id
func DeleteTeam(tstCtx diagnostics.TestContext, id string) error {
	var err error
	isV2, err := tstCtx.IsIAMV2()
	if err != nil {
		return err
	}

	var reqPath string
	if isV2 {
		reqPath = fmt.Sprintf("/apis/iam/v2/teams/%s", id)
	} else {
		reqPath = fmt.Sprintf("/api/v0/auth/teams/%s", id)
	}

	err = MustJSONDecodeSuccess(
		tstCtx.DoLBRequest(
			reqPath,
			lbrequest.WithMethod("DELETE"),
		)).Error()

	if err != nil {
		return errors.Wrap(err, "Could not delete team")
	}
	return nil
}

// GetTeamID determines which identifier to use when fetching the team, since
// the ID field is different across IAM v1 and and v2
func GetTeamID(tstCtx diagnostics.TestContext, team authTeamSave) (string, error) {
	// if the team was saved as a v1 team, it has an auto-generated guid ID and unique name
	if !team.IsV2 {
		upgradedToV2, err := tstCtx.IsIAMV2()
		if err != nil {
			return "", err
		}
		if upgradedToV2 {
			// after upgrade to v2, a v1 team's Name becomes its v2 team ID
			return team.Name, nil
		}
	}

	// if there was no change in IAM version between the generation and verify steps
	// then we can use the same team ID
	return team.ID, nil
}

// CreateAuthTeamsDiagnostic creates a diagnostic for auth teams
func CreateAuthTeamsDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "auth-teams",
		Tags: diagnostics.Tags{"auth"},
		Generate: func(tstCtx diagnostics.TestContext) error {
			teamInfo, err := CreateRandomTeam(tstCtx)
			if err != nil {
				return err
			}
			tstCtx.SetValue("auth-teams", &authTeamSave{
				ID:   teamInfo.Team.ID,
				Name: teamInfo.Team.Name,
				IsV2: teamInfo.IsV2,
			})
			return nil
		},
		Verify: func(tstCtx diagnostics.VerificationTestContext) {
			loaded := authTeamSave{}
			err := tstCtx.GetValue("auth-teams", &loaded)
			require.NoError(tstCtx, err, "Could not load generated context")

			id, err := GetTeamID(tstCtx, loaded)
			require.NoError(tstCtx, err, "Could not determine team identifier")
			teamInfo, err := GetTeam(tstCtx, id)
			require.NoError(tstCtx, err)
			assert.Equal(tstCtx, id, teamInfo.Team.ID, "Received unexpected team ID")
		},
		Cleanup: func(tstCtx diagnostics.TestContext) error {
			loaded := authTeamSave{}
			err := tstCtx.GetValue("auth-teams", &loaded)
			if err != nil {
				return errors.Wrap(err, "Could not load generated context")
			}

			id, err := GetTeamID(tstCtx, loaded)
			if err != nil {
				return errors.Wrap(err, "Could not determine team identifier")
			}
			return DeleteTeam(tstCtx, id)
		},
	}
}

func init() {
	diagnostics.RegisterDiagnostic(
		CreateAuthTeamsDiagnostic(),
	)
}
