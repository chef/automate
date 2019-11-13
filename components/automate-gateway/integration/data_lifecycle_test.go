package integration

import (
	"time"

	rrule "github.com/teambition/rrule-go"

	"github.com/chef/automate/api/external/data_lifecycle"
	datalifecycle "github.com/chef/automate/api/interservice/data_lifecycle"
)

const (
	compliancePurgeJobName           = "periodic_purge"
	complianceScansPolicyName        = "compliance-scans"
	complianceReportsPolicyName      = "compliance-reports"
	complianceReportsOlderThanDays   = 123
	complianceScansOlderThanDays     = 456
	eventFeedPurgeJobName            = "periodic_purge"
	eventFeedPurgePolicyName         = "feed"
	eventFeedOlderThanDays           = 789
	infraPurgeJobName                = "periodic_purge_timeseries"
	infraActionsPurgePolicyName      = "actions"
	infraActionsOlderThanDays        = 234
	infraConvergePurgePolicyName     = "converge-history"
	infraConvergeOlderThanDays       = 567
	infraDeleteNodesJobName          = "delete_nodes"
	infraDeleteNodesThreshold        = "14d"
	infraMissingNodesJobName         = "missing_nodes"
	infraMissingNodesThreshold       = "21d"
	infraDeleteMissingNodesJobName   = "missing_nodes_for_deletion"
	infraDeleteMissingNodesThreshold = "1m"

	testRecurrence = "FREQ=DAILY;DTSTART=20191106T180323Z;INTERVAL=2"
)

// TestDataLifecycleConfigure tests configuring the data lifecycle jobs. We'll
// test updating and showing for individual data types and from the global
// endpoints.
// NOTE:
// * services are in beta and have node been added to this endpoint
// * postgres purge policies have not been implemented, as such they are not tested yet.
func (suite *GatewayTestSuite) TestDataLifecycleConfigure() {
	dlClient := data_lifecycle.NewDataLifecycleClient(suite.gwConn)

	// Get the existing status/config
	oldStatus, err := dlClient.GetStatus(suite.ctx, &data_lifecycle.GetStatusRequest{})
	suite.Require().NoError(err)

	// Set each config individually
	newComplianceConfig := &data_lifecycle.SetComplianceConfigRequest{
		JobSettings: []*data_lifecycle.JobSettings{
			&data_lifecycle.JobSettings{
				Name:       compliancePurgeJobName,
				Disabled:   true,
				Recurrence: testRecurrence,
				PurgePolicies: &data_lifecycle.PurgePolicyUpdate{
					Elasticsearch: []*datalifecycle.EsPolicyUpdate{
						&datalifecycle.EsPolicyUpdate{
							PolicyName:    complianceReportsPolicyName,
							OlderThanDays: complianceReportsOlderThanDays,
							Disabled:      true,
						},
						&datalifecycle.EsPolicyUpdate{
							PolicyName:    complianceScansPolicyName,
							OlderThanDays: complianceScansOlderThanDays,
							Disabled:      true,
						},
					},
				},
			},
		},
	}

	newEventFeedConfig := &data_lifecycle.SetEventFeedConfigRequest{
		JobSettings: []*data_lifecycle.JobSettings{
			&data_lifecycle.JobSettings{
				Name:       eventFeedPurgeJobName,
				Disabled:   true,
				Recurrence: testRecurrence,
				PurgePolicies: &data_lifecycle.PurgePolicyUpdate{
					Elasticsearch: []*datalifecycle.EsPolicyUpdate{
						&datalifecycle.EsPolicyUpdate{
							PolicyName:    eventFeedPurgePolicyName,
							OlderThanDays: eventFeedOlderThanDays,
							Disabled:      true,
						},
					},
				},
			},
		},
	}

	newInfraConfig := &data_lifecycle.SetInfraConfigRequest{
		JobSettings: []*data_lifecycle.JobSettings{
			&data_lifecycle.JobSettings{
				Name:       infraPurgeJobName,
				Disabled:   true,
				Recurrence: testRecurrence,
				PurgePolicies: &data_lifecycle.PurgePolicyUpdate{
					Elasticsearch: []*datalifecycle.EsPolicyUpdate{
						&datalifecycle.EsPolicyUpdate{
							PolicyName:    infraActionsPurgePolicyName,
							OlderThanDays: infraActionsOlderThanDays,
							Disabled:      true,
						},
						&datalifecycle.EsPolicyUpdate{
							PolicyName:    infraConvergePurgePolicyName,
							OlderThanDays: infraConvergeOlderThanDays,
							Disabled:      true,
						},
					},
				},
			},
			&data_lifecycle.JobSettings{
				Name:       infraDeleteNodesJobName,
				Disabled:   true,
				Recurrence: testRecurrence,
				Threshold:  infraDeleteNodesThreshold,
			},
			&data_lifecycle.JobSettings{
				Name:       infraMissingNodesJobName,
				Disabled:   true,
				Recurrence: testRecurrence,
				Threshold:  infraMissingNodesThreshold,
			},
			&data_lifecycle.JobSettings{
				Name:       infraDeleteMissingNodesJobName,
				Disabled:   true,
				Recurrence: testRecurrence,
				Threshold:  infraDeleteMissingNodesThreshold,
			},
		},
	}

	_, err = dlClient.SetComplianceConfig(suite.ctx, newComplianceConfig)
	suite.Require().NoError(err)

	_, err = dlClient.SetInfraConfig(suite.ctx, newInfraConfig)
	suite.Require().NoError(err)

	_, err = dlClient.SetEventFeedConfig(suite.ctx, newEventFeedConfig)
	suite.Require().NoError(err)

	// Check each config individually
	complianceStatus, err := dlClient.GetComplianceStatus(suite.ctx, &data_lifecycle.GetComplianceStatusRequest{})
	suite.Require().NoError(err)
	suite.assertJobSettingsMatchJobStatus(newComplianceConfig.JobSettings, complianceStatus.Jobs)

	eventFeedStatus, err := dlClient.GetEventFeedStatus(suite.ctx, &data_lifecycle.GetEventFeedStatusRequest{})
	suite.Require().NoError(err)
	suite.assertJobSettingsMatchJobStatus(newEventFeedConfig.JobSettings, eventFeedStatus.Jobs)

	infraStatus, err := dlClient.GetInfraStatus(suite.ctx, &data_lifecycle.GetInfraStatusRequest{})
	suite.Require().NoError(err)
	suite.assertJobSettingsMatchJobStatus(newInfraConfig.JobSettings, infraStatus.Jobs)

	// Reset each individually to the original config
	oldComplianceSettings := jobStatusesToJobSettings(oldStatus.GetCompliance().GetJobs())
	oldComplianceConfigRequest := &data_lifecycle.SetComplianceConfigRequest{
		JobSettings: oldComplianceSettings,
	}
	oldEventFeedSettings := jobStatusesToJobSettings(oldStatus.GetEventFeed().GetJobs())
	oldEventFeedConfigRequest := &data_lifecycle.SetEventFeedConfigRequest{
		JobSettings: oldEventFeedSettings,
	}
	oldInfraSettings := jobStatusesToJobSettings(oldStatus.GetInfra().GetJobs())
	oldInfraConfigRequest := &data_lifecycle.SetInfraConfigRequest{
		JobSettings: oldInfraSettings,
	}

	_, err = dlClient.SetComplianceConfig(suite.ctx, oldComplianceConfigRequest)
	suite.Require().NoError(err)
	_, err = dlClient.SetInfraConfig(suite.ctx, oldInfraConfigRequest)
	suite.Require().NoError(err)
	_, err = dlClient.SetEventFeedConfig(suite.ctx, oldEventFeedConfigRequest)
	suite.Require().NoError(err)

	// Set the config globally
	_, err = dlClient.SetConfig(suite.ctx, &data_lifecycle.SetConfigRequest{
		Compliance: newComplianceConfig,
		EventFeed:  newEventFeedConfig,
		Infra:      newInfraConfig,
	})

	// Ensure the config is set
	status, err := dlClient.GetStatus(suite.ctx, &data_lifecycle.GetStatusRequest{})
	suite.Require().NoError(err)
	suite.assertJobSettingsMatchJobStatus(newComplianceConfig.JobSettings, status.Compliance.Jobs)
	suite.assertJobSettingsMatchJobStatus(newEventFeedConfig.JobSettings, status.EventFeed.Jobs)
	suite.assertJobSettingsMatchJobStatus(newInfraConfig.JobSettings, status.Infra.Jobs)

	// Set it back to the original config globally
	_, err = dlClient.SetConfig(suite.ctx, &data_lifecycle.SetConfigRequest{
		Compliance: oldComplianceConfigRequest,
		EventFeed:  oldEventFeedConfigRequest,
		Infra:      oldInfraConfigRequest,
	})
	suite.Require().NoError(err)
}

// TestDataLifecycleRun ensures that when run is called that we request a purge
// TODO: Generating every data type, refreshing indices and ensuring that
// data is deleted is a large scope. I feel it probably best to create a separate
// library for creating data that we can eventually plug into this.
func (suite *GatewayTestSuite) TestDataLifecycleRun() {
	dlClient := data_lifecycle.NewDataLifecycleClient(suite.gwConn)

	// Get the existing status/config
	oldStatus, err := dlClient.GetStatus(suite.ctx, &data_lifecycle.GetStatusRequest{})
	suite.Require().NoError(err)

	// Enable everything but make the recurrence rule an hour from now to be
	// sure that a scheduled purge doesn't run during our testing.
	// that a schedule wont get triggered.
	recurrence, err := rrule.NewRRule(rrule.ROption{
		Freq:     rrule.HOURLY,
		Interval: 1,
		Dtstart:  time.Now(),
	})
	suite.Require().NoError(err)
	r := recurrence.String()

	_, err = dlClient.SetConfig(suite.ctx, &data_lifecycle.SetConfigRequest{
		Compliance: &data_lifecycle.SetComplianceConfigRequest{
			JobSettings: []*data_lifecycle.JobSettings{
				&data_lifecycle.JobSettings{
					Name:       compliancePurgeJobName,
					Recurrence: r,
					Disabled:   false,
				},
			},
		},
		EventFeed: &data_lifecycle.SetEventFeedConfigRequest{
			JobSettings: []*data_lifecycle.JobSettings{
				&data_lifecycle.JobSettings{
					Name:       eventFeedPurgeJobName,
					Recurrence: r,
					Disabled:   false,
				},
			},
		},
		Infra: &data_lifecycle.SetInfraConfigRequest{
			JobSettings: []*data_lifecycle.JobSettings{
				&data_lifecycle.JobSettings{
					Name:       infraPurgeJobName,
					Recurrence: r,
					Disabled:   false,
					PurgePolicies: &data_lifecycle.PurgePolicyUpdate{
						Elasticsearch: []*datalifecycle.EsPolicyUpdate{
							&datalifecycle.EsPolicyUpdate{
								PolicyName:    infraActionsPurgePolicyName,
								OlderThanDays: 0,
								Disabled:      false,
							},
							&datalifecycle.EsPolicyUpdate{
								PolicyName:    infraConvergePurgePolicyName,
								OlderThanDays: 0,
								Disabled:      true,
							},
						},
					},
				},
				&data_lifecycle.JobSettings{
					Name:       infraDeleteNodesJobName,
					Recurrence: r,
					Disabled:   false,
				},
				&data_lifecycle.JobSettings{
					Name:       infraMissingNodesJobName,
					Recurrence: r,
					Disabled:   false,
				},
				&data_lifecycle.JobSettings{
					Name:       infraDeleteMissingNodesJobName,
					Recurrence: r,
					Disabled:   false,
				},
			},
		},
	})

	// TODO: create some data

	// Run purge
	_, err = dlClient.Run(suite.ctx, &data_lifecycle.RunRequest{})
	suite.Require().NoError(err)

	// Set the old config in place
	oldComplianceSettings := jobStatusesToJobSettings(oldStatus.GetCompliance().GetJobs())
	oldComplianceConfigRequest := &data_lifecycle.SetComplianceConfigRequest{
		JobSettings: oldComplianceSettings,
	}
	oldEventFeedSettings := jobStatusesToJobSettings(oldStatus.GetEventFeed().GetJobs())
	oldEventFeedConfigRequest := &data_lifecycle.SetEventFeedConfigRequest{
		JobSettings: oldEventFeedSettings,
	}
	oldInfraSettings := jobStatusesToJobSettings(oldStatus.GetInfra().GetJobs())
	oldInfraConfigRequest := &data_lifecycle.SetInfraConfigRequest{
		JobSettings: oldInfraSettings,
	}
	_, err = dlClient.SetConfig(suite.ctx, &data_lifecycle.SetConfigRequest{
		Compliance: oldComplianceConfigRequest,
		EventFeed:  oldEventFeedConfigRequest,
		Infra:      oldInfraConfigRequest,
	})
	suite.Require().NoError(err)
}

func (suite *GatewayTestSuite) assertJobSettingsMatchJobStatus(expected []*data_lifecycle.JobSettings, actual []*data_lifecycle.JobStatus) {
	if expected == nil {
		suite.T().Log("no job settings given")
		suite.T().Fail()
		return
	}

	if actual == nil {
		suite.T().Log("no job statuses given")
		suite.T().Fail()
		return
	}

	if len(expected) != len(actual) {
		suite.T().Logf("expected %d jobs statuses, actual: %d", len(expected), len(actual))
		suite.T().Fail()
		return
	}

	for _, eJob := range expected {
		found := false
		for _, aJob := range actual {
			if eJob.Name == aJob.Name {
				found = true

				if eJob.Disabled != aJob.Disabled {
					suite.T().Logf("expected job %s to be %t", eJob.Name, aJob.Disabled)
					suite.T().Fail()
					return
				}

				if eJob.Recurrence != aJob.Recurrence {
					suite.T().Logf("job %s expected recurrence %s, actual: %s", eJob.Name, eJob.Recurrence, aJob.Recurrence)
					suite.T().Fail()
					return
				}

				if eJob.Threshold != aJob.Threshold {
					suite.T().Logf("job %s expected threshold %s, actual: %s", eJob.Name, eJob.Threshold, aJob.Threshold)
					suite.T().Fail()
					return
				}

				expectedEsPolicies := eJob.GetPurgePolicies().GetElasticsearch()
				actualEsPolicies := aJob.GetPurgePolicies().GetElasticsearch()

				if expectedEsPolicies == nil && actualEsPolicies != nil {
					suite.T().Logf("have %d elasticsearch purge policies, expected none", len(actualEsPolicies))
					suite.T().Fail()
					return
				}

				if expectedEsPolicies != nil && actualEsPolicies == nil {
					suite.T().Logf("have node elasticsearch purge policies, expected %d", len(expectedEsPolicies))
					suite.T().Fail()
					return
				}

				if len(expectedEsPolicies) != len(actualEsPolicies) {
					suite.T().Logf("have %d elasticsearch purge policies, expected %d purge policies",
						len(actualEsPolicies), len(expectedEsPolicies),
					)
					suite.T().Fail()
					return
				}

				for _, policyUpdate := range expectedEsPolicies {
					found := false

					for _, policySetting := range actualEsPolicies {
						if policyUpdate.PolicyName == policySetting.Name {
							found = true

							if policyUpdate.Disabled != policySetting.Disabled {
								suite.T().Logf("expected policy setting %s to be %t", policyUpdate.PolicyName, policySetting.Disabled)
								suite.T().Fail()
								return
							}

							if policyUpdate.OlderThanDays != policySetting.OlderThanDays {
								suite.T().Logf("expected policy older than days %d to be %d", policyUpdate.OlderThanDays, policySetting.OlderThanDays)
								suite.T().Fail()
								return
							}

							break
						}
					}

					if !found {
						suite.T().Logf("unable to elasticsearch policy %s", policyUpdate.PolicyName)
						suite.T().Fail()
						return
					}
				}

				// TODO Postgres jobs are not fully implemented yet

				break
			}
		}

		if found != true {
			suite.T().Logf("unable to find job %s", eJob.Name)
			suite.T().Fail()
			return
		}
	}
}

func (suite *GatewayTestSuite) TestConvertJobStatusToJobSetting() {
	statuses := []*data_lifecycle.JobStatus{
		&data_lifecycle.JobStatus{
			Name:       "job1",
			Disabled:   false,
			Recurrence: "FREQ=HOURLY;DTSTART=20191106T180240Z;INTERVAL=500",
			Threshold:  "1m",
		},
		&data_lifecycle.JobStatus{
			Name:       "job2",
			Disabled:   true,
			Recurrence: "FREQ=MINUTELY;DTSTART=20191106T180240Z;INTERVAL=400",
			PurgePolicies: &data_lifecycle.PurgePolicies{
				Elasticsearch: []*datalifecycle.EsPolicy{
					&datalifecycle.EsPolicy{
						Name:          "policy1",
						OlderThanDays: 123,
						Disabled:      true,
					},
					&datalifecycle.EsPolicy{
						Name:          "policy2",
						OlderThanDays: 456,
						Disabled:      false,
					},
				},
			},
		},
	}

	expectedSettings := []*data_lifecycle.JobSettings{
		&data_lifecycle.JobSettings{
			Name:       "job1",
			Disabled:   false,
			Recurrence: "FREQ=HOURLY;DTSTART=20191106T180240Z;INTERVAL=500",
			Threshold:  "1m",
		},
		&data_lifecycle.JobSettings{
			Name:       "job2",
			Disabled:   true,
			Recurrence: "FREQ=MINUTELY;DTSTART=20191106T180240Z;INTERVAL=400",
			PurgePolicies: &data_lifecycle.PurgePolicyUpdate{
				Elasticsearch: []*datalifecycle.EsPolicyUpdate{
					&datalifecycle.EsPolicyUpdate{
						PolicyName:    "policy1",
						OlderThanDays: 123,
						Disabled:      true,
					},
					&datalifecycle.EsPolicyUpdate{
						PolicyName:    "policy2",
						OlderThanDays: 456,
						Disabled:      false,
					},
				},
			},
		},
	}

	generatedSettings := jobStatusesToJobSettings(statuses)

	// Rather than deep compare our expected settings with the generated settings
	// we can assert that both match the statuses.
	suite.assertJobSettingsMatchJobStatus(generatedSettings, statuses)
	suite.assertJobSettingsMatchJobStatus(expectedSettings, statuses)
}

func jobStatusesToJobSettings(in []*data_lifecycle.JobStatus) []*data_lifecycle.JobSettings {
	out := []*data_lifecycle.JobSettings{}

	for _, status := range in {
		setting := &data_lifecycle.JobSettings{}
		setting.Name = status.Name
		setting.Disabled = status.Disabled
		setting.Recurrence = status.Recurrence
		setting.Threshold = status.Threshold

		if esPolicies := status.GetPurgePolicies().GetElasticsearch(); esPolicies != nil && len(esPolicies) > 0 {
			setting.PurgePolicies = &data_lifecycle.PurgePolicyUpdate{
				Elasticsearch: []*datalifecycle.EsPolicyUpdate{},
			}

			for _, policy := range esPolicies {
				setting.PurgePolicies.Elasticsearch = append(setting.PurgePolicies.Elasticsearch, &datalifecycle.EsPolicyUpdate{
					PolicyName:    policy.Name,
					Disabled:      policy.Disabled,
					OlderThanDays: policy.OlderThanDays,
				})
			}
		}

		out = append(out, setting)
	}

	return out
}
