package authz

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestManagerMergeJobStatus(t *testing.T) {
	cases := []struct {
		description       string
		jobStatuses       []JobStatus
		expectedJobStatus JobStatus
	}{
		{
			description: "Empty Job Statuses",
			jobStatuses: []JobStatus{},
			expectedJobStatus: JobStatus{
				Completed:             true,
				PercentageComplete:    1.0,
				EstimatedEndTimeInSec: 0,
			},
		},
		{
			description: "Two complete Jobs",
			jobStatuses: []JobStatus{
				{
					Completed:             true,
					PercentageComplete:    1.0,
					EstimatedEndTimeInSec: 0,
				},
				{
					Completed:             true,
					PercentageComplete:    1.0,
					EstimatedEndTimeInSec: 0,
				},
			},
			expectedJobStatus: JobStatus{
				Completed:             true,
				PercentageComplete:    1.0,
				EstimatedEndTimeInSec: 0,
			},
		},
		{
			description: "One complete Job",
			jobStatuses: []JobStatus{
				{
					Completed:             true,
					PercentageComplete:    1.0,
					EstimatedEndTimeInSec: 0,
				},
			},
			expectedJobStatus: JobStatus{
				Completed:             true,
				PercentageComplete:    1.0,
				EstimatedEndTimeInSec: 0,
			},
		},
		{
			description: "First complete and second incomplete",
			jobStatuses: []JobStatus{
				{
					Completed:             true,
					PercentageComplete:    1.0,
					EstimatedEndTimeInSec: 0,
				},
				{
					Completed:             false,
					PercentageComplete:    0.5,
					EstimatedEndTimeInSec: 1555520534,
				},
			},
			expectedJobStatus: JobStatus{
				Completed:             false,
				PercentageComplete:    0.5,
				EstimatedEndTimeInSec: 1555520534,
			},
		},
		{
			description: "First incomplete and second complete",
			jobStatuses: []JobStatus{
				{
					Completed:             false,
					PercentageComplete:    0.5,
					EstimatedEndTimeInSec: 1555520534,
				},
				{
					Completed:             true,
					PercentageComplete:    1.0,
					EstimatedEndTimeInSec: 0,
				},
			},
			expectedJobStatus: JobStatus{
				Completed:             false,
				PercentageComplete:    0.5,
				EstimatedEndTimeInSec: 1555520534,
			},
		},
		{
			description: "Two incomplete Jobs",
			jobStatuses: []JobStatus{
				{
					Completed:             false,
					PercentageComplete:    0.7,
					EstimatedEndTimeInSec: 1555520534,
				},
				{
					Completed:             false,
					PercentageComplete:    0.3,
					EstimatedEndTimeInSec: 1555520534 + 20,
				},
			},
			expectedJobStatus: JobStatus{
				Completed:             false,
				PercentageComplete:    0.3,
				EstimatedEndTimeInSec: 1555520534 + 20,
			},
		},
		{
			description: "Two incomplete Jobs; use the job with the latest estimated complete date",
			jobStatuses: []JobStatus{
				{
					Completed:             false,
					PercentageComplete:    0.7,
					EstimatedEndTimeInSec: 1555520534,
				},
				{
					Completed:             false,
					PercentageComplete:    0.8,
					EstimatedEndTimeInSec: 1555520534 + 20,
				},
			},
			expectedJobStatus: JobStatus{
				Completed:             false,
				PercentageComplete:    0.8,
				EstimatedEndTimeInSec: 1555520534 + 20,
			},
		},
		{
			description: "Two incomplete Jobs; use the job with the latest estimated complete date; opposite order",
			jobStatuses: []JobStatus{
				{
					Completed:             false,
					PercentageComplete:    0.8,
					EstimatedEndTimeInSec: 1555520534 + 20,
				},
				{
					Completed:             false,
					PercentageComplete:    0.7,
					EstimatedEndTimeInSec: 1555520534,
				},
			},
			expectedJobStatus: JobStatus{
				Completed:             false,
				PercentageComplete:    0.8,
				EstimatedEndTimeInSec: 1555520534 + 20,
			},
		},
	}

	for _, test := range cases {
		t.Run(fmt.Sprintf("FindSlowestJobStatus: %s", test.description),
			func(t *testing.T) {
				mergedJobStatus := FindSlowestJobStatus(test.jobStatuses)

				assert.Equal(t, test.expectedJobStatus, mergedJobStatus)
			})
	}
}
