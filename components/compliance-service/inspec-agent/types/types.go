package types

import (
	"time"

	"github.com/chef/automate/components/compliance-service/inspec"
)

const (
	// StatusNew is the initial state of every job
	StatusNew string = "new"
	// StatusScheduled is given when the scheduler accepted the job
	StatusScheduled string = "scheduled"
	// StatusCompleted for all jobs that passed
	StatusCompleted string = "completed"
	// StatusReachable if connect/detect is successful
	StatusReachable string = "reachable"
	// StatusFailed if any component of the job failed
	StatusFailed string = "failed"
	// StatusRunning when the job is being processed by any worker
	StatusRunning string = "running"
	// StatusAborted if any job run on a node has been aborted
	StatusAborted string = "aborted"
)

const (
	// JobTypeDetect for running endpoint detections
	JobTypeDetect string = "detect"
	// JobTypeExec for executing inspec profiles
	JobTypeExec string = "exec"
	// JobTypeSSM for executing inspec ssm jobs (via aws)
	JobTypeSSM string = "ssm"
)

type InspecBaseJob struct {
	JobID    string `json:"job_id,omitempty"`
	JobName  string `json:"job_name,omitempty"`
	JobType  string `json:"job_type,omitempty"`
	NodeID   string `json:"node_uuid,omitempty"`
	NodeName string `json:"node_name,omitempty"`
	NodeEnv  string `json:"node_env,omitempty"`
	Status   string `json:"status,omitempty"`
}

type NodeStatus struct {
	Status    string
	StartTime time.Time
}

// InspecJob description.
// StartTime, EndTime are pointers in order to be omitted when having default values(0001-01-01T00:00:00Z)
// Timeout is the number of seconds until a timeout is issued and the job is aborted.
type InspecJob struct {
	InspecBaseJob
	Profiles          []string            `json:"profiles,omitempty"`
	Timeout           int32               `json:"timeout,omitempty"`
	Retries           int32               `json:"retries"`
	RetriesLeft       int32               `json:"retries_left"`
	StartTime         *time.Time          `json:"start_time,omitempty"`
	EndTime           *time.Time          `json:"end_time,omitempty"`
	TargetConfig      inspec.TargetConfig `json:"target_config,omitempty"`
	NodeStatus        *string             `json:"node_status,omitempty"`
	SourceID          string              `json:"source_id,omitempty"`
	SourceAccountID   string              `json:"account_id,omitempty"`
	SSM               bool                `json:"ssm,omitempty"`
	Reporter          inspec.Reporter     `json:"reporter,omitempty"`
	ProfilesOwner     string              `json:"profiles_owner,omitempty"`
	InternalProfiles  []string            `json:"internal_profiles,omitempty"`
	MachineIdentifier string              `json:"machine_identifier,omitempty"`
}

// WorkerStats describe the state of all inspec job workers to be used for analysis purposes
type WorkerStats struct {
	ID                int    `json:"worker_id"`
	CurrentJob        string `json:"current_job_id"`
	CurrentJobSummary string `json:"current_job_summary"`
	PreviousJob       string `json:"previous_job_id"`
	CompletedJobs     int    `json:"completed_jobs"`
}

type SSMTargetConfig struct {
	Reporter     map[string]inspec.Reporter `json:"reporter,omitempty"`
	SudoPassword string                     `json:"sudo_password,omitempty"`
	Sudo         bool                       `json:"sudo,omitempty"`
	BackendCache bool                       `json:"backend_cache,omitempty"`
	InspecBaseJob
	Compliance *AutomateLoginInfo `json:"compliance,omitempty"`
}

type AutomateLoginInfo struct {
	Server   string `json:"server,omitempty"`
	Token    string `json:"token,omitempty"`
	User     string `json:"user,omitempty"`
	Insecure bool   `json:"insecure,omitempty"`
}
