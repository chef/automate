package projectupdater

import (
	"fmt"
	"testing"

	"github.com/chef/automate/components/compliance-service/ingest/ingestic"
	"github.com/stretchr/testify/assert"
)

func TestManagerMergeJobStatus(t *testing.T) {
	cases := []struct {
		description       string
		jobStatuses       []ingestic.JobStatus
		expectedJobStatus ingestic.JobStatus
	}{
		{
			description: "Empty Job Statuses",
			jobStatuses: []ingestic.JobStatus{},
			expectedJobStatus: ingestic.JobStatus{
				Completed:             true,
				PercentageComplete:    1.0,
				EstimatedEndTimeInSec: 0,
			},
		},
		{
			description: "Two complete Jobs",
			jobStatuses: []ingestic.JobStatus{
				ingestic.JobStatus{
					Completed:             true,
					PercentageComplete:    1.0,
					EstimatedEndTimeInSec: 0,
				},
				ingestic.JobStatus{
					Completed:             true,
					PercentageComplete:    1.0,
					EstimatedEndTimeInSec: 0,
				},
			},
			expectedJobStatus: ingestic.JobStatus{
				Completed:             true,
				PercentageComplete:    1.0,
				EstimatedEndTimeInSec: 0,
			},
		},
		{
			description: "One complete Job",
			jobStatuses: []ingestic.JobStatus{
				ingestic.JobStatus{
					Completed:             true,
					PercentageComplete:    1.0,
					EstimatedEndTimeInSec: 0,
				},
			},
			expectedJobStatus: ingestic.JobStatus{
				Completed:             true,
				PercentageComplete:    1.0,
				EstimatedEndTimeInSec: 0,
			},
		},
		{
			description: "First complete and second incomplete",
			jobStatuses: []ingestic.JobStatus{
				ingestic.JobStatus{
					Completed:             true,
					PercentageComplete:    1.0,
					EstimatedEndTimeInSec: 0,
				},
				ingestic.JobStatus{
					Completed:             false,
					PercentageComplete:    0.5,
					EstimatedEndTimeInSec: 1555520534,
				},
			},
			expectedJobStatus: ingestic.JobStatus{
				Completed:             false,
				PercentageComplete:    0.5,
				EstimatedEndTimeInSec: 1555520534,
			},
		},
		{
			description: "First incomplete and second complete",
			jobStatuses: []ingestic.JobStatus{
				ingestic.JobStatus{
					Completed:             false,
					PercentageComplete:    0.5,
					EstimatedEndTimeInSec: 1555520534,
				},
				ingestic.JobStatus{
					Completed:             true,
					PercentageComplete:    1.0,
					EstimatedEndTimeInSec: 0,
				},
			},
			expectedJobStatus: ingestic.JobStatus{
				Completed:             false,
				PercentageComplete:    0.5,
				EstimatedEndTimeInSec: 1555520534,
			},
		},
		{
			description: "Two incomplete Jobs",
			jobStatuses: []ingestic.JobStatus{
				ingestic.JobStatus{
					Completed:             false,
					PercentageComplete:    0.7,
					EstimatedEndTimeInSec: 1555520534,
				},
				ingestic.JobStatus{
					Completed:             false,
					PercentageComplete:    0.3,
					EstimatedEndTimeInSec: 1555520534 + 20,
				},
			},
			expectedJobStatus: ingestic.JobStatus{
				Completed:             false,
				PercentageComplete:    0.3,
				EstimatedEndTimeInSec: 1555520534 + 20,
			},
		},
		{
			description: "Two incomplete Jobs; use the job with the latest estimated complete date",
			jobStatuses: []ingestic.JobStatus{
				ingestic.JobStatus{
					Completed:             false,
					PercentageComplete:    0.7,
					EstimatedEndTimeInSec: 1555520534,
				},
				ingestic.JobStatus{
					Completed:             false,
					PercentageComplete:    0.8,
					EstimatedEndTimeInSec: 1555520534 + 20,
				},
			},
			expectedJobStatus: ingestic.JobStatus{
				Completed:             false,
				PercentageComplete:    0.8,
				EstimatedEndTimeInSec: 1555520534 + 20,
			},
		},
		{
			description: "Two incomplete Jobs; use the job with the latest estimated complete date; opposite order",
			jobStatuses: []ingestic.JobStatus{
				ingestic.JobStatus{
					Completed:             false,
					PercentageComplete:    0.8,
					EstimatedEndTimeInSec: 1555520534 + 20,
				},
				ingestic.JobStatus{
					Completed:             false,
					PercentageComplete:    0.7,
					EstimatedEndTimeInSec: 1555520534,
				},
			},
			expectedJobStatus: ingestic.JobStatus{
				Completed:             false,
				PercentageComplete:    0.8,
				EstimatedEndTimeInSec: 1555520534 + 20,
			},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("MergeJobStatus: %s", test.description),
			func(t *testing.T) {
				mergedJobStatus := MergeJobStatus(test.jobStatuses)

				assert.Equal(t, test.expectedJobStatus, mergedJobStatus)
			})
	}
}
