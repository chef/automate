package relaxting

import (
	"time"

	"github.com/chef/automate/components/compliance-service/reporting"
)

// Used to unmarshal summary documents stored in comp-s-* ElasticSearch indices
type ESInSpecSummary struct {
	NodeID        string    `json:"node_uuid"`
	InSpecVersion string    `json:"version"`
	ReportID      string    `json:"report_uuid"`
	DailyLatest   bool      `json:"daily_latest"`
	NodeName      string    `json:"node_name"`
	Environment   string    `json:"environment"`
	EndTime       time.Time `json:"end_time"`
	Status        string    `json:"status"`
	JobID         string    `json:"job_uuid"`
	Roles         []string  `json:"roles"`
	Recipes       []string  `json:"recipes"`
	Platform      struct {
		Name    string `json:"name"`
		Release string `json:"release"`
		Full    string `json:"full"`
	} `json:"platform"`
	ControlsSums reporting.NodeControlSummary `json:"controls_sums"`
	Profiles     []ESInSpecSummaryProfile     `json:"profiles"`
	Statistics   struct {
		Duration float32 `json:"duration"`
	} `json:"statistics"`
	DocVersion       string   `json:"doc_version"`
	ESTimestamp      string   `json:"@timestamp"`
	PolicyName       string   `json:"policy_name"`
	PolicyGroup      string   `json:"policy_group"`
	OrganizationName string   `json:"organization_name"`
	SourceFQDN       string   `json:"source_fqdn"`
	ChefTags         []string `json:"chef_tags"`
	Projects         []string `json:"projects"`
	IPAddress        string   `json:"ipaddress"`
}

// Used to unmarshal report documents stored in comp-r-* ElasticSearch indices
type ESInSpecReport struct {
	ID            string    `json:"_id,omitempty"`
	InSpecVersion string    `json:"version"`
	NodeID        string    `json:"node_uuid"`
	ReportID      string    `json:"report_uuid"`
	DailyLatest   bool      `json:"daily_latest"`
	NodeName      string    `json:"node_name"`
	Environment   string    `json:"environment"`
	EndTime       time.Time `json:"end_time"`
	Status        string    `json:"status"`
	JobID         string    `json:"job_uuid"`
	Roles         []string  `json:"roles"`
	Recipes       []string  `json:"recipes"`
	Platform      struct {
		Name    string `json:"name"`
		Release string `json:"release"`
		Full    string `json:"full"`
	} `json:"platform"`
	ControlsSums reporting.NodeControlSummary `json:"controls_sums"`
	Profiles     []ESInSpecReportProfile      `json:"profiles"`
	Statistics   struct {
		Duration float32 `json:"duration"`
	} `json:"statistics"`
	DocVersion       string   `json:"doc_version"`
	ESTimestamp      string   `json:"@timestamp"`
	Projects         []string `json:"projects"`
	PolicyName       string   `json:"policy_name"`
	PolicyGroup      string   `json:"policy_group"`
	OrganizationName string   `json:"organization_name"`
	SourceFQDN       string   `json:"source_fqdn"`
	ChefTags         []string `json:"chef_tags"`
	// Elastic won't accept empty string for a field of data type 'ip'. Defining IPAddress
	// as pointer so we can assign null to it when we get a report without an IP
	IPAddress *string `json:"ipaddress"`
	FQDN      string  `json:"fqdn"`
}

type ESInSpecSummaryProfile struct {
	Profile      string                       `json:"profile"`
	Status       string                       `json:"status"`
	Name         string                       `json:"name"`
	Title        string                       `json:"title"`
	Version      string                       `json:"version"`
	Full         string                       `json:"full"`
	SHA256       string                       `json:"sha256"`
	ControlsSums reporting.NodeControlSummary `json:"controls_sums"`
}

type ESInSpecReportControlsResult struct {
	Status      string  `json:"status"`
	CodeDesc    string  `json:"code_desc"`
	RunTime     float32 `json:"run_time"`
	StartTime   string  `json:"start_time,omitempty"`
	Message     string  `json:"message,omitempty"`
	SkipMessage string  `json:"skip_message,omitempty"`
}

type ESInSpecReportControlStringTags struct {
	Key    string   `json:"key"`
	Values []string `json:"values"`
}

type ESInSpecReportControlRefs struct {
	Ref string `json:"ref"`
	Url string `json:"url"`
}

type ESInSpecReportControl struct {
	ID         string                            `json:"id"`
	Impact     float32                           `json:"impact"`
	Title      string                            `json:"title"`
	Status     string                            `json:"status"`
	Results    []*ESInSpecReportControlsResult   `json:"results"`
	StringTags []ESInSpecReportControlStringTags `json:"string_tags"`
	Refs       []ESInSpecReportControlRefs       `json:"refs"`
}

type ESInSpecReportProfile struct {
	Name         string                       `json:"name"`
	Title        string                       `json:"title"`
	Profile      string                       `json:"profile"`
	Full         string                       `json:"full"`
	Version      string                       `json:"version"`
	Namespace    string                       `json:"namespace,omitempty"`
	SHA256       string                       `json:"sha256"`
	Controls     []ESInSpecReportControl      `json:"controls"`
	ControlsSums reporting.NodeControlSummary `json:"controls_sums"`
	Depends      []ESInSpecReportDepends      `json:"depends"`
	Status       string                       `json:"status"`
	SkipMessage  string                       `json:"skip_message"`
}

type ESInSpecReportDepends struct {
	Name        string `json:"name"`
	Status      string `json:"status"`
	SkipMessage string `json:"skip_message"`
}

type ProfileStatSummary struct {
	Name     string `json:"name"`
	ID       string `json:"id"`
	Failures int    `json:"failures"`
}

type Depends struct {
	Name        string `json:"name"`
	Path        string `json:"path"`
	Status      string `json:"status"`
	SkipMessage string `json:"skip_message"`
}

type Suggestion struct {
	Text    string  `json:"text"`
	Id      string  `json:"id,omitempty"`
	Score   float32 `json:"score"`
	Version string  `json:"version,omitempty"`
}

type ControlStatSummary struct {
	Name     string `json:"name"`
	Profile  string `json:"profile"`
	Failures int    `json:"failures"`
}

type PlatformStatSummary struct {
	Name     string `json:"name"`
	Failures int    `json:"failures"`
}

type EnvironmentStatSummary struct {
	Name     string `json:"name"`
	Failures int    `json:"failures"`
}

type StatsTopFailures struct {
	Profiles     []ProfileStatSummary     `json:"profiles,omitempty"`
	Platforms    []PlatformStatSummary    `json:"platforms,omitempty"`
	Controls     []ControlStatSummary     `json:"controls,omitempty"`
	Environments []EnvironmentStatSummary `json:"environments,omitempty"`
}

type PassedFailedSkipped struct {
	Passed  int32 `json:"passed"`
	Failed  int32 `json:"failed"`
	Skipped int32 `json:"skipped"`
}

type ControlStats struct {
	Control string  `json:"control"`
	Title   string  `json:"title"`
	Passed  int     `json:"passed"`
	Failed  int     `json:"failed"`
	Skipped int     `json:"skipped"`
	Impact  float32 `json:"impact"`
}

//ControlMeta is for getting extra info from inspec_profiles needed for report 8
type ControlMeta struct {
	Name   string
	Title  string
	Impact float32
}

type StatsComplianceSummary struct {
	Compliant    int `json:"compliant"`
	Skipped      int `json:"skipped"`
	Noncompliant int `json:"noncompliant"`
	HighRisk     int `json:"high_risk"`
	MediumRisk   int `json:"medium_risk"`
	LowRisk      int `json:"low_risk"`
}

type AggregatedComplianceSummary struct {
	Failures  int `json:"failures"`
	Majors    int `json:"majors"`
	Minors    int `json:"minors"`
	Criticals int `json:"criticals"`
	Passed    int `json:"passed"`
	Skipped   int `json:"skipped"`
}

type ProfileListWithAggregatedComplianceSummary struct {
	Name      string `json:"name"`
	ID        string `json:"id"`
	Failures  int    `json:"failures"`
	Majors    int    `json:"majors"`
	Minors    int    `json:"minors"`
	Criticals int    `json:"criticals"`
	Passed    int    `json:"passed"`
	Skipped   int    `json:"skipped"`
}

type OS struct {
	Name string `json:"name"`
}

type Platform struct {
	Name    string `json:"name"`
	Release string `json:"release"`
}

type Failure struct {
	Total    int `json:"total"`
	Minor    int `json:"minor"`
	Major    int `json:"major"`
	Critical int `json:"critical"`
}

type NodeListWithAggregatedComplianceSummary struct {
	NodeID      string    `json:"node_id"`
	Name        string    `json:"node_name"`
	Environment string    `json:"environment"`
	EndTime     time.Time `json:"end_time"`
	Platform    Platform  `json:"platform"`
	Failed      Failure   `json:"failed"`
}

type ReportId struct {
	ID string `json:"id"`
}

type TimeBucketedReportIds struct {
	Date      string
	ReportIds []string
}

type ESMigrationInfo struct {
	MostRecentValidLatestDate time.Time `json:"most_recent_valid_latest_date,omitempty"`
	CurrentIndicesVersion     string    `json:"current_indices_version,omitempty"`
}
