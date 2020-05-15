package runner

import (
	"testing"

	ingest_inspec "github.com/chef/automate/api/interservice/compliance/ingest/events/inspec"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/stretchr/testify/assert"
)

func TestNodeHasSecrets(t *testing.T) {
	tc := inspec.TargetConfig{
		TargetBaseConfig: inspec.TargetBaseConfig{
			Backend: "ssh",
		},
		SecretsArr: []*inspec.Secrets{},
	}
	res := nodeHasSecrets(&tc)
	assert.Equal(t, false, res)

	tc.User = "my-user"
	res = nodeHasSecrets(&tc)
	assert.Equal(t, false, res)

	tc.Password = "my-password"
	res = nodeHasSecrets(&tc)
	assert.Equal(t, true, res)
}

func TestPotentialTargetConfigs(t *testing.T) {
	job := types.InspecJob{
		TargetConfig: inspec.TargetConfig{
			TargetBaseConfig: inspec.TargetBaseConfig{
				Backend:  "ssh",
				Hostname: "blabla",
			},
			SecretsArr: []*inspec.Secrets{
				{User: "a", Password: "b"},
				{User: "c", Password: "d"},
			},
		},
	}

	targetConfigs := potentialTargetConfigs(&job)

	assert.Equal(t, inspec.TargetConfig{
		TargetBaseConfig: inspec.TargetBaseConfig{
			Backend:  "ssh",
			Hostname: "blabla",
		}, Secrets: inspec.Secrets{
			User:     "a",
			Password: "b",
		},
	}, targetConfigs[0])

	assert.Equal(t, inspec.TargetConfig{
		TargetBaseConfig: inspec.TargetBaseConfig{
			Backend:  "ssh",
			Hostname: "blabla",
		}, Secrets: inspec.Secrets{
			User:     "c",
			Password: "d",
		},
	}, targetConfigs[1])
}

func TestRemoveResults(t *testing.T) {
	profiles := []*ingest_inspec.Profile{
		{
			Title: "Test Profile 1",
			Controls: []*ingest_inspec.Control{
				{
					Id: "p1c1",
					Results: []*ingest_inspec.Result{
						{
							Message: "Resource is as expected",
							Status:  "passed",
						},
						{Status: "passed"},
						{Status: "failed"},
						{Status: "skipped"},
						{Status: "failed"},
						{Status: "skipped"},
						{Status: "skipped"},
						{Status: "failed"},
						{Status: "passed"},
						{Status: "skipped"},
						{Status: "skipped"},
						{Status: "failed"},
						{Status: "passed"},
						{Status: "failed"},
						{Status: "skipped"},
						{Status: "failed"},
						{Status: "failed"},
						{Status: "skipped"},
						{Status: "failed"},
						{Status: "passed"},
						{Status: "passed"},
						{Status: "passed"},
						{Status: "passed"},
						{Status: "failed"},
						{Status: "failed"},
						{Status: "failed"},
						{Status: "failed"},
						{Status: "skipped"},
						{Status: "skipped"},
						{Status: "skipped"},
						{Status: "skipped"},
					},
				},
				{
					Id: "p1c2",
				},
			},
		},
		{
			Title: "Test Profile 2",
		},
	}

	removeResults("test-report-id", profiles, 25)
	assert.Equal(t, []*ingest_inspec.Profile{
		{
			Title: "Test Profile 1",
			Controls: []*ingest_inspec.Control{
				{
					Id: "p1c1",
					Results: []*ingest_inspec.Result{
						{Status: "failed"},
						{Status: "failed"},
						{Status: "failed"},
						{Status: "failed"},
						{Status: "failed"},
						{Status: "failed"},
						{Status: "failed"},
						{Status: "failed"},
						{Status: "failed"},
						{Status: "failed"},
						{Status: "failed"},
						{Status: "failed"},
						{Status: "skipped"},
						{Status: "skipped"},
						{Status: "skipped"},
						{Status: "skipped"},
						{Status: "skipped"},
						{Status: "skipped"},
						{Status: "skipped"},
						{Status: "skipped"},
						{Status: "skipped"},
						{Status: "skipped"},
						{Status: "skipped"},
						{Status: "passed"},
						{Status: "passed"},
					},
					RemovedResultsCounts: &ingest_inspec.RemovedResultsCounts{
						Passed: 6,
					},
				},
				{
					Id: "p1c2",
				},
			},
		},
		{
			Title: "Test Profile 2",
		},
	}, profiles)

	removeResults("test-report-id", profiles, 4)
	assert.Equal(t, []*ingest_inspec.Profile{
		{
			Title: "Test Profile 1",
			Controls: []*ingest_inspec.Control{
				{
					Id: "p1c1",
					Results: []*ingest_inspec.Result{
						{Status: "failed"},
						{Status: "failed"},
						{Status: "failed"},
						{Status: "failed"},
					},
					RemovedResultsCounts: &ingest_inspec.RemovedResultsCounts{
						Failed:  8,
						Skipped: 11,
						Passed:  2,
					},
				},
				{
					Id: "p1c2",
				},
			},
		},
		{
			Title: "Test Profile 2",
		},
	}, profiles)

}
