package runner

import (
	"testing"

	ingest_events_compliance_api "github.com/chef/automate/api/interservice/compliance/ingest/events/compliance"
	ingest_inspec "github.com/chef/automate/api/interservice/compliance/ingest/events/inspec"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/golang/protobuf/ptypes/struct"
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

func TestStripProfilesMetadata(t *testing.T) {
	report := ingest_events_compliance_api.Report{
		ReportUuid: "test1",
		Profiles: []*ingest_inspec.Profile{
			{
				Name:           "test-profile-1",
				Sha256:         "test-profile-1-sha256-id",
				Version:        "4.5.6",
				Maintainer:     "Testus",
				Copyright:      "Testus Biz",
				CopyrightEmail: "Testus Biz",
				Title:          "Test Profile 1",
				Depends: []*ingest_inspec.Dependency{
					{
						Name: "test",
					},
				},
				Supports: []*ingest_inspec.Support{
					{
						Inspec: "2.3.4",
					},
				},
				Controls: []*ingest_inspec.Control{
					{
						Id:     "p1c1",
						Title:  "p1c1 title",
						Impact: 1,
						Code:   "some code",
						Desc:   "some desc",
						SourceLocation: &ingest_inspec.SourceLocation{
							Ref:  "123",
							Line: 456,
						},
						Refs: []*structpb.Struct{},
						Tags: &structpb.Struct{},
						Results: []*ingest_inspec.Result{
							{
								Message:   "Resource is as expected",
								Status:    "passed",
								RunTime:   0.12345,
								StartTime: "2020-05-15T18:37:19+01:00",
							},
							{
								Message:   "Resource is as expected",
								Status:    "passed",
								RunTime:   1.12345,
								StartTime: "2020-05-15T18:37:20+01:00",
							},
						},
					},
					{
						Id:     "p1c2",
						Title:  "p1c2 title",
						Impact: 1,
						Code:   "some code",
						Desc:   "some desc",
						Results: []*ingest_inspec.Result{
							{
								Message:   "Resource is as expected",
								Status:    "skipped",
								RunTime:   0.42345,
								StartTime: "2020-05-15T18:37:21+01:00",
							},
						},
					},
				},
			},
			{
				Name:           "test-profile-2",
				Sha256:         "test-profile-2-sha256-id",
				Version:        "4.5.6",
				Maintainer:     "Testus2",
				Copyright:      "Testus Biz2",
				CopyrightEmail: "Testus Biz2",
				Title:          "Test Profile 2",
				Depends: []*ingest_inspec.Dependency{
					{
						Name: "test2",
					},
				},
				Supports: []*ingest_inspec.Support{
					{
						Inspec: "2.3.5",
					},
				},
				Controls: []*ingest_inspec.Control{
					{
						Id:     "p2c1",
						Title:  "p2c1 title",
						Impact: 0.6,
						Code:   "some code",
						Desc:   "some desc",
						SourceLocation: &ingest_inspec.SourceLocation{
							Ref:  "12",
							Line: 45,
						},
						Refs: []*structpb.Struct{},
						Tags: &structpb.Struct{},
						Results: []*ingest_inspec.Result{
							{
								Message:   "Resource is as expected",
								Status:    "passed",
								RunTime:   0.12345,
								StartTime: "2020-05-15T18:37:19+01:00",
							},
							{
								Message:   "Resource is as expected",
								Status:    "passed",
								RunTime:   1.12345,
								StartTime: "2020-05-15T18:37:20+01:00",
							},
						},
					},
				},
			},
		},
	}
	var missingProfilesMap = map[string]struct{}{
		"test-profile-2-sha256-id": {},
	}

	stripProfilesMetadata(&report, missingProfilesMap, 1.1)

	expectedReport := ingest_events_compliance_api.Report{
		ReportUuid: "test1",
		Profiles: []*ingest_inspec.Profile{
			{
				Sha256:  "test-profile-1-sha256-id",
				Version: "4.5.6",
				Title:   "Test Profile 1",
				Controls: []*ingest_inspec.Control{
					{
						Id: "p1c1",
						Results: []*ingest_inspec.Result{
							{
								Message: "Resource is as expected",
								Status:  "passed",
							},
							{
								Message:   "Resource is as expected",
								Status:    "passed",
								RunTime:   1.12345,
								StartTime: "2020-05-15T18:37:20+01:00",
							},
						},
					},
					{
						Id: "p1c2",
						Results: []*ingest_inspec.Result{
							{
								Message: "Resource is as expected",
								Status:  "skipped",
							},
						},
					},
				},
			},
			{
				Name:           "test-profile-2",
				Sha256:         "test-profile-2-sha256-id",
				Version:        "4.5.6",
				Maintainer:     "Testus2",
				Copyright:      "Testus Biz2",
				CopyrightEmail: "Testus Biz2",
				Title:          "Test Profile 2",
				Depends: []*ingest_inspec.Dependency{
					{
						Name: "test2",
					},
				},
				Supports: []*ingest_inspec.Support{
					{
						Inspec: "2.3.5",
					},
				},
				Controls: []*ingest_inspec.Control{
					{
						Id:     "p2c1",
						Title:  "p2c1 title",
						Impact: 0.6,
						Code:   "some code",
						Desc:   "some desc",
						SourceLocation: &ingest_inspec.SourceLocation{
							Ref:  "12",
							Line: 45,
						},
						Refs: []*structpb.Struct{},
						Tags: &structpb.Struct{},
						Results: []*ingest_inspec.Result{
							{
								Message:   "Resource is as expected",
								Status:    "passed",
								RunTime:   0.12345,
								StartTime: "2020-05-15T18:37:19+01:00",
							},
							{
								Message:   "Resource is as expected",
								Status:    "passed",
								RunTime:   1.12345,
								StartTime: "2020-05-15T18:37:20+01:00",
							},
						},
					},
				},
			},
		},
		RunTimeLimit: 1.1,
	}

	assert.Equal(t, expectedReport, report)
}
