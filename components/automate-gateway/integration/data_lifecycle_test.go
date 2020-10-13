package integration

import (
	"time"

	"github.com/golang/protobuf/ptypes"
	rrule "github.com/teambition/rrule-go"

	"github.com/chef/automate/api/external/data_lifecycle"
)

const (
	compliancePurgeJobName                     = "periodic_purge"
	complianceScansPolicyName                  = "compliance-scans"
	complianceReportsPolicyName                = "compliance-reports"
	complianceReportsOlderThanDays             = 123
	complianceScansOlderThanDays               = 456
	eventFeedPurgeJobName                      = "periodic_purge"
	eventFeedPurgePolicyName                   = "feed"
	eventFeedOlderThanDays                     = 789
	infraPurgeJobName                          = "periodic_purge_timeseries"
	infraActionsPurgePolicyName                = "actions"
	infraActionsOlderThanDays                  = 234
	infraConvergePurgePolicyName               = "converge-history"
	infraConvergeOlderThanDays                 = 567
	infraDeleteNodesJobName                    = "delete_nodes"
	infraDeleteNodesThreshold                  = "14d"
	infraMissingNodesJobName                   = "missing_nodes"
	infraMissingNodesThreshold                 = "21d"
	infraDeleteMissingNodesJobName             = "missing_nodes_for_deletion"
	infraDeleteMissingNodesThreshold           = "1m"
	svcsDisconnectedServicesJobName            = "disconnected_services"
	svcsDisconnectedServicesJobThreshold       = "9m"
	svcsDeleteDisconnectedServicesJobName      = "delete_disconnected_services"
	svcsDeleteDisconnectedServicesJobThreshold = "23d"
	testRecurrence                             = "FREQ=DAILY;DTSTART=20191106T180323Z;INTERVAL=2"
)

// TestDataLifecycleConfigure tests configuring data lifecycle jobs in all
// four possible ways: global, infra only, event-feed only, and compliance only.
// services has not been implemented yet, nor have postgres purge policies.
func (suite *GatewayTestSuite) TestDataLifecycleConfigure() {
	dlClient := data_lifecycle.NewDataLifecycleClient(suite.gwConn)

	// Get the existing global status so we can set everything back to the
	// way we found it later.
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
					Elasticsearch: []*data_lifecycle.EsPolicyUpdate{
						&data_lifecycle.EsPolicyUpdate{
							PolicyName:    complianceReportsPolicyName,
							OlderThanDays: complianceReportsOlderThanDays,
							Disabled:      true,
						},
						&data_lifecycle.EsPolicyUpdate{
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
					Elasticsearch: []*data_lifecycle.EsPolicyUpdate{
						&data_lifecycle.EsPolicyUpdate{
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
					Elasticsearch: []*data_lifecycle.EsPolicyUpdate{
						&data_lifecycle.EsPolicyUpdate{
							PolicyName:    infraActionsPurgePolicyName,
							OlderThanDays: infraActionsOlderThanDays,
							Disabled:      true,
						},
						&data_lifecycle.EsPolicyUpdate{
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

	newServicesConfig := &data_lifecycle.SetServicesConfigRequest{
		JobSettings: []*data_lifecycle.JobSettings{
			&data_lifecycle.JobSettings{
				Name:       svcsDisconnectedServicesJobName,
				Disabled:   true,
				Recurrence: testRecurrence,
				Threshold:  svcsDisconnectedServicesJobThreshold,
			},
			&data_lifecycle.JobSettings{
				Name:       svcsDeleteDisconnectedServicesJobName,
				Disabled:   true,
				Recurrence: testRecurrence,
				Threshold:  svcsDeleteDisconnectedServicesJobThreshold,
			},
		},
	}

	_, err = dlClient.SetComplianceConfig(suite.ctx, newComplianceConfig)
	suite.Require().NoError(err)

	_, err = dlClient.SetInfraConfig(suite.ctx, newInfraConfig)
	suite.Require().NoError(err)

	_, err = dlClient.SetEventFeedConfig(suite.ctx, newEventFeedConfig)
	suite.Require().NoError(err)

	_, err = dlClient.SetServicesConfig(suite.ctx, newServicesConfig)
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

	svcsStatus, err := dlClient.GetServicesStatus(suite.ctx, &data_lifecycle.GetServicesStatusRequest{})
	suite.Require().NoError(err)
	suite.assertJobSettingsMatchJobStatus(newServicesConfig.JobSettings, svcsStatus.Jobs)

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
	oldSvcsSettings := jobStatusesToJobSettings(oldStatus.GetServices().GetJobs())
	oldSvcsConfigRequest := &data_lifecycle.SetServicesConfigRequest{
		JobSettings: oldSvcsSettings,
	}

	_, err = dlClient.SetComplianceConfig(suite.ctx, oldComplianceConfigRequest)
	suite.Require().NoError(err)
	_, err = dlClient.SetInfraConfig(suite.ctx, oldInfraConfigRequest)
	suite.Require().NoError(err)
	_, err = dlClient.SetEventFeedConfig(suite.ctx, oldEventFeedConfigRequest)
	suite.Require().NoError(err)
	_, err = dlClient.SetServicesConfig(suite.ctx, oldSvcsConfigRequest)
	suite.Require().NoError(err)

	// Set the config globally
	_, err = dlClient.SetConfig(suite.ctx, &data_lifecycle.SetConfigRequest{
		Compliance: newComplianceConfig,
		EventFeed:  newEventFeedConfig,
		Infra:      newInfraConfig,
		Services:   newServicesConfig,
	})

	// Get the status globally and ensure the config is set properly
	status, err := dlClient.GetStatus(suite.ctx, &data_lifecycle.GetStatusRequest{})
	suite.Require().NoError(err)
	suite.assertJobSettingsMatchJobStatus(newComplianceConfig.JobSettings, status.Compliance.Jobs)
	suite.assertJobSettingsMatchJobStatus(newEventFeedConfig.JobSettings, status.EventFeed.Jobs)
	suite.assertJobSettingsMatchJobStatus(newInfraConfig.JobSettings, status.Infra.Jobs)
	suite.assertJobSettingsMatchJobStatus(newServicesConfig.JobSettings, status.Services.Jobs)

	// Set it back to the original config
	_, err = dlClient.SetConfig(suite.ctx, &data_lifecycle.SetConfigRequest{
		Compliance: oldComplianceConfigRequest,
		EventFeed:  oldEventFeedConfigRequest,
		Infra:      oldInfraConfigRequest,
		Services:   oldSvcsConfigRequest,
	})
	suite.Require().NoError(err)
}

// TestDataLifecycleRun ensures that when "run" is called that we actually run
// data lifecycle jobs on the backend. Each individual service already has
// integrations tests for their data lifecycle jobs so we only need to test
// if the jobs are being triggered successfully. The purge jobs record their
// start and end times in the database so we can use the timestamps to determine
// if our job was successfully run.
func (suite *GatewayTestSuite) TestDataLifecycleRun() {
	dlClient := data_lifecycle.NewDataLifecycleClient(suite.gwConn)

	oldStatus, err := dlClient.GetStatus(suite.ctx, &data_lifecycle.GetStatusRequest{})
	suite.Require().NoError(err)

	// Disable everyhing except for the purge jobs. Make sure each purge policy
	// is disabled so we don't actually delete data but still run the job.
	startTime := time.Now()
	recurrence, err := rrule.NewRRule(rrule.ROption{
		Freq:     rrule.HOURLY,
		Interval: 1,
		Dtstart:  startTime,
	})
	suite.Require().NoError(err)
	r := recurrence.String()

	_, err = dlClient.SetConfig(suite.ctx, &data_lifecycle.SetConfigRequest{
		Compliance: &data_lifecycle.SetComplianceConfigRequest{
			JobSettings: []*data_lifecycle.JobSettings{
				&data_lifecycle.JobSettings{
					Name:       compliancePurgeJobName,
					Disabled:   false,
					Recurrence: r,
					PurgePolicies: &data_lifecycle.PurgePolicyUpdate{
						Elasticsearch: []*data_lifecycle.EsPolicyUpdate{
							&data_lifecycle.EsPolicyUpdate{
								PolicyName: complianceReportsPolicyName,
								Disabled:   true,
							},
							&data_lifecycle.EsPolicyUpdate{
								PolicyName: complianceScansPolicyName,
								Disabled:   true,
							},
						},
					},
				},
			},
		},
		EventFeed: &data_lifecycle.SetEventFeedConfigRequest{
			JobSettings: []*data_lifecycle.JobSettings{
				&data_lifecycle.JobSettings{
					Name:       eventFeedPurgeJobName,
					Disabled:   false,
					Recurrence: r,
					PurgePolicies: &data_lifecycle.PurgePolicyUpdate{
						Elasticsearch: []*data_lifecycle.EsPolicyUpdate{
							&data_lifecycle.EsPolicyUpdate{
								PolicyName: eventFeedPurgePolicyName,
								Disabled:   true,
							},
						},
					},
				},
			},
		},
		Infra: &data_lifecycle.SetInfraConfigRequest{
			JobSettings: []*data_lifecycle.JobSettings{
				&data_lifecycle.JobSettings{
					Name:       infraPurgeJobName,
					Disabled:   false,
					Recurrence: r,
					PurgePolicies: &data_lifecycle.PurgePolicyUpdate{
						Elasticsearch: []*data_lifecycle.EsPolicyUpdate{
							&data_lifecycle.EsPolicyUpdate{
								PolicyName: infraActionsPurgePolicyName,
								Disabled:   true,
							},
							&data_lifecycle.EsPolicyUpdate{
								PolicyName: infraConvergePurgePolicyName,
								Disabled:   true,
							},
						},
					},
				},
				&data_lifecycle.JobSettings{
					Name:       infraDeleteNodesJobName,
					Disabled:   true,
					Recurrence: r,
				},
				&data_lifecycle.JobSettings{
					Name:       infraMissingNodesJobName,
					Disabled:   true,
					Recurrence: r,
				},
				&data_lifecycle.JobSettings{
					Name:       infraDeleteMissingNodesJobName,
					Disabled:   true,
					Recurrence: r,
				},
			},
		},
		Services: &data_lifecycle.SetServicesConfigRequest{
			JobSettings: []*data_lifecycle.JobSettings{
				&data_lifecycle.JobSettings{
					Name:       svcsDisconnectedServicesJobName,
					Disabled:   true,
					Recurrence: r,
					Threshold:  svcsDisconnectedServicesJobThreshold,
				},
				&data_lifecycle.JobSettings{
					Name:       svcsDeleteDisconnectedServicesJobName,
					Disabled:   true,
					Recurrence: r,
					Threshold:  svcsDeleteDisconnectedServicesJobThreshold,
				},
			},
		},
	})

	// Config changes will kick off enabled jobs. Let's wait for them to quickly
	// finish and then reset the start time for our next jobs.
	time.Sleep(1 * time.Second)
	startTime = time.Now()

	// Run each individually and verify that they were started after we began
	_, err = dlClient.RunCompliance(suite.ctx, &data_lifecycle.RunComplianceRequest{})
	suite.Require().NoError(err)

	_, err = dlClient.RunEventFeed(suite.ctx, &data_lifecycle.RunEventFeedRequest{})
	suite.Require().NoError(err)

	_, err = dlClient.RunInfra(suite.ctx, &data_lifecycle.RunInfraRequest{})
	suite.Require().NoError(err)

	_, err = dlClient.RunServices(suite.ctx, &data_lifecycle.RunServicesRequest{})
	suite.Require().NoError(err)

	// Give the jobs some time to run and then get their updated status
	time.Sleep(1 * time.Second)
	complianceStatus, err := dlClient.GetComplianceStatus(suite.ctx, &data_lifecycle.GetComplianceStatusRequest{})
	suite.Require().NoError(err)

	eventFeedStatus, err := dlClient.GetEventFeedStatus(suite.ctx, &data_lifecycle.GetEventFeedStatusRequest{})
	suite.Require().NoError(err)

	infraStatus, err := dlClient.GetInfraStatus(suite.ctx, &data_lifecycle.GetInfraStatusRequest{})
	suite.Require().NoError(err)

	svcsStatus, err := dlClient.GetServicesStatus(suite.ctx, &data_lifecycle.GetServicesStatusRequest{})
	suite.Require().NoError(err)

	// Assert that they ran successfully
	suite.assertStartedAfter(complianceStatus.Jobs, startTime)
	suite.assertStartedAfter(eventFeedStatus.Jobs, startTime)
	for _, j := range infraStatus.Jobs {
		if j.Name == infraPurgeJobName {
			suite.assertStartedAfter([]*data_lifecycle.JobStatus{j}, startTime)
		}
	}
	suite.assertStartedAfter(svcsStatus.Jobs, startTime)

	// Reset the startTime and run them from the global endpoint
	startTime = time.Now()
	_, err = dlClient.Run(suite.ctx, &data_lifecycle.RunRequest{})
	suite.Require().NoError(err)

	// Give the jobs some time to run and then get their statuses
	time.Sleep(1 * time.Second)

	status, err := dlClient.GetStatus(suite.ctx, &data_lifecycle.GetStatusRequest{})
	suite.Require().NoError(err)

	// Get all purge jobs and assert they ran after the start time
	purgeJobs := append(status.Compliance.Jobs, status.EventFeed.Jobs...)
	purgeJobs = append(purgeJobs, status.Services.Jobs...)
	for _, j := range status.Infra.Jobs {
		if j.Name == infraPurgeJobName {
			purgeJobs = append(purgeJobs, j)
		}
	}
	suite.assertStartedAfter(purgeJobs, startTime)

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
	oldSvcsSettings := jobStatusesToJobSettings(oldStatus.GetServices().GetJobs())
	oldSvcsConfigRequest := &data_lifecycle.SetServicesConfigRequest{
		JobSettings: oldSvcsSettings,
	}
	_, err = dlClient.SetConfig(suite.ctx, &data_lifecycle.SetConfigRequest{
		Compliance: oldComplianceConfigRequest,
		EventFeed:  oldEventFeedConfigRequest,
		Infra:      oldInfraConfigRequest,
		Services:   oldSvcsConfigRequest,
	})
	suite.Require().NoError(err)
}

func (suite *GatewayTestSuite) assertStartedAfter(jobs []*data_lifecycle.JobStatus, start time.Time) {
	for _, job := range jobs {
		ranAfter := job.GetLastStartedAt()
		if ranAfter == nil {
			suite.T().Logf("job %s has not ran yet", job.Name)
			suite.T().Fail()
		}

		after, err := ptypes.Timestamp(ranAfter)
		if ranAfter == nil {
			suite.T().Logf("failed to parsed job %s timestamp: %s", job.Name, err.Error())
			suite.T().Fail()
		}

		if after.Before(start) {
			suite.T().Logf("job %s last run started (%d) before start time (%d)", job.Name, after.Unix(), start.Unix())
			suite.T().Fail()
		}
	}
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
				Elasticsearch: []*data_lifecycle.EsPolicy{
					&data_lifecycle.EsPolicy{
						Name:          "policy1",
						OlderThanDays: 123,
						Disabled:      true,
					},
					&data_lifecycle.EsPolicy{
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
				Elasticsearch: []*data_lifecycle.EsPolicyUpdate{
					&data_lifecycle.EsPolicyUpdate{
						PolicyName:    "policy1",
						OlderThanDays: 123,
						Disabled:      true,
					},
					&data_lifecycle.EsPolicyUpdate{
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
				Elasticsearch: []*data_lifecycle.EsPolicyUpdate{},
			}

			for _, policy := range esPolicies {
				setting.PurgePolicies.Elasticsearch = append(setting.PurgePolicies.Elasticsearch, &data_lifecycle.EsPolicyUpdate{
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
