package relaxting

import (
	"time"
)

// These are legacy indices kept for migration purposes from older A1 and A2 versions released before Feb 2019
// They apply for ElasticSearch timeseries indices named like this:
// `compliance-YYYY-MM-DD` which contained `inspec_summary` and `inspec_report` document types
// `comp-1-s-YYYY-MM-DD` and `comp-1-r-YYYY-MM-DD`, referred by migration code as A2v1
// `comp-2-s-YYYY-MM-DD` and `comp-2-r-YYYY-MM-DD`, referred by migration code as A2v2
// where YYYY-MM-DD would be the date when compliance reports got ingested

//
// Main type for the summary documents used in the timeseries
//
type ESInSpecSummaryA2v2 struct {
	NodeID      string    `json:"node_uuid"`
	ReportID    string    `json:"report_uuid"`
	DailyLatest bool      `json:"daily_latest"`
	NodeName    string    `json:"node_name"`
	Environment string    `json:"environment"`
	EndTime     time.Time `json:"end_time"`
	Status      string    `json:"status"`
	JobID       string    `json:"job_uuid"`
	Roles       []string  `json:"roles"`
	Recipes     []string  `json:"recipes"`
	Platform    struct {
		Name    string `json:"name"`
		Release string `json:"release"`
	} `json:"platform"`
	Controls     NodeControlSummaryA2v2              `json:"controls"`
	ProfilesSums []ESInspecProfileControlSummaryA2v2 `json:"profiles_sums"`
	DocVersion   string                              `json:"doc_version"`
	ESTimestamp  string                              `json:"@timestamp"`
}

//
// Main type for the report documents used in the timeseries
//
type ESInSpecReportA2v2 struct {
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
	} `json:"platform"`
	Controls    NodeControlSummaryA2v2      `json:"controls"`
	ProfilesMin []ESInSpecReportMinA2v2     `json:"profiles_min"`
	Profiles    []ESInSpecReportProfileA2v2 `json:"profiles"`
	Statistics  struct {
		Duration float32 `json:"duration"`
	} `json:"statistics"`
	DocVersion  string `json:"doc_version"`
	ESTimestamp string `json:"@timestamp"`
}

type NodeControlSummaryA2v2 struct {
	Total  int `json:"total"`
	Passed struct {
		Total int `json:"total"`
	} `json:"passed"`
	Skipped struct {
		Total int `json:"total"`
	} `json:"skipped"`
	Failed struct {
		Total    int `json:"total"`
		Minor    int `json:"minor"`
		Major    int `json:"major"`
		Critical int `json:"critical"`
	} `json:"failed"`
}

type ESInSpecReportProfileA2v2 struct {
	Name        string                      `json:"name"`
	Title       string                      `json:"title"`
	Profile     string                      `json:"profile"`
	Version     string                      `json:"version"`
	Namespace   string                      `json:"namespace,omitempty"`
	SHA256      string                      `json:"sha256"`
	Controls    []ESInSpecReportControlA2v2 `json:"controls"`
	Depends     []ESInSpecReportDependsA2v2 `json:"depends"`
	Status      string                      `json:"status"`
	SkipMessage string                      `json:"skip_message"`
}

type ProfileMinA2v2 struct {
	Name    string `json:"name"`
	Title   string `json:"title"`
	ID      string `json:"id"`
	Version string `json:"version"`
	Status  string `json:"status,omitempty"`
}

type ESInspecProfileControlSummaryA2v2 struct {
	Profile  string                 `json:"profile"`
	Status   string                 `json:"status"`
	Controls NodeControlSummaryA2v2 `json:"controls"`
}

type ESInSpecReportControlsResultA2v2 struct {
	Status      string  `json:"status"`
	CodeDesc    string  `json:"code_desc"`
	RunTime     float32 `json:"run_time"`
	StartTime   string  `json:"start_time,omitempty"`
	Message     string  `json:"message,omitempty"`
	SkipMessage string  `json:"skip_message,omitempty"`
}

type ESInSpecReportControlA2v2 struct {
	ID      string                              `json:"id"`
	Impact  float32                             `json:"impact"`
	Title   string                              `json:"title"`
	Status  string                              `json:"status"`
	Results []*ESInSpecReportControlsResultA2v2 `json:"results"`
}

type ESInSpecReportMinA2v2 struct {
	Name        string                      `json:"name"`
	Version     string                      `json:"version"`
	Namespace   string                      `json:"namespace,omitempty"`
	SHA256      string                      `json:"sha256"`
	Controls    []ESInSpecReportControlA2v2 `json:"controls"`
	Depends     []ESInSpecReportDependsA2v2 `json:"depends"`
	Status      string                      `json:"status"`
	SkipMessage string                      `json:"skip_message"`
}

type ESInSpecReportDependsA2v2 struct {
	Name        string `json:"name"`
	Status      string `json:"status"`
	SkipMessage string `json:"skip_message"`
}

type ProfileStatSummaryA2v2 struct {
	Name     string `json:"name"`
	ID       string `json:"id"`
	Failures int    `json:"failures"`
}

type DependsA2v2 struct {
	Name        string `json:"name"`
	Path        string `json:"path"`
	Status      string `json:"status"`
	SkipMessage string `json:"skip_message"`
}

type SuggestionA2v2 struct {
	Text    string  `json:"text"`
	Id      string  `json:"id,omitempty"`
	Score   float32 `json:"score"`
	Version string  `json:"version,omitempty"`
}

type ControlStatSummaryA2v2 struct {
	Name     string `json:"name"`
	Profile  string `json:"profile"`
	Failures int    `json:"failures"`
}

type PlatformStatSummaryA2v2 struct {
	Name     string `json:"name"`
	Failures int    `json:"failures"`
}

type EnvironmentStatSummaryA2v2 struct {
	Name     string `json:"name"`
	Failures int    `json:"failures"`
}

type StatsTopFailuresA2v2 struct {
	Profiles     []ProfileStatSummaryA2v2     `json:"profiles,omitempty"`
	Platforms    []PlatformStatSummaryA2v2    `json:"platforms,omitempty"`
	Controls     []ControlStatSummaryA2v2     `json:"controls,omitempty"`
	Environments []EnvironmentStatSummaryA2v2 `json:"environments,omitempty"`
}

type PassedFailedSkippedA2v2 struct {
	Passed  int32 `json:"passed"`
	Failed  int32 `json:"failed"`
	Skipped int32 `json:"skipped"`
}

type ControlStatsA2v2 struct {
	Control string  `json:"control"`
	Title   string  `json:"title"`
	Passed  int     `json:"passed"`
	Failed  int     `json:"failed"`
	Skipped int     `json:"skipped"`
	Impact  float32 `json:"impact"`
}

//ControlMeta is for getting extra info from inspec_profiles needed for report 8
type ControlMetaA2v2 struct {
	Name   string
	Title  string
	Impact float32
}

type StatsComplianceSummaryA2v2 struct {
	Compliant    int `json:"compliant"`
	Skipped      int `json:"skipped"`
	Noncompliant int `json:"noncompliant"`
	HighRisk     int `json:"high_risk"`
	MediumRisk   int `json:"medium_risk"`
	LowRisk      int `json:"low_risk"`
}

type AggregatedComplianceSummaryA2v2 struct {
	Failures  int `json:"failures"`
	Majors    int `json:"majors"`
	Minors    int `json:"minors"`
	Criticals int `json:"criticals"`
	Passed    int `json:"passed"`
	Skipped   int `json:"skipped"`
}

type ProfileListWithAggregatedComplianceSummaryA2v2 struct {
	Name      string `json:"name"`
	ID        string `json:"id"`
	Failures  int    `json:"failures"`
	Majors    int    `json:"majors"`
	Minors    int    `json:"minors"`
	Criticals int    `json:"criticals"`
	Passed    int    `json:"passed"`
	Skipped   int    `json:"skipped"`
}

type OSA2v2 struct {
	Name string `json:"name"`
}

type PlatformA2v2 struct {
	Name    string `json:"name"`
	Release string `json:"release"`
}

type FailureA2v2 struct {
	Total    int `json:"total"`
	Minor    int `json:"minor"`
	Major    int `json:"major"`
	Critical int `json:"critical"`
}

type NodeListWithAggregatedComplianceSummaryA2v2 struct {
	NodeID      string       `json:"node_id"`
	Name        string       `json:"node_name"`
	Environment string       `json:"environment"`
	EndTime     time.Time    `json:"end_time"`
	Platform    PlatformA2v2 `json:"platform"`
	Failed      FailureA2v2  `json:"failed"`
}
