package authz

import (
	"context"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
)

const (
	sleepTimeBetweenStatusChecksMilliSec = 1000
	maxNumberOfConsecutiveFails          = 10
)

type EsClient interface {
	JobCancel(context.Context, string) error
	UpdateProjectTags(context.Context, map[string]*iam_v2.ProjectRules) ([]string, error)
	JobStatus(context.Context, string) (JobStatus, error)
}

const (
	RunningState    = "running"
	NotRunningState = "not_running"
)
