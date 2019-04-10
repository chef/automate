package inspec

import (
	"encoding/json"

	reportingapi "github.com/chef/automate/components/compliance-service/api/reporting"
	"gopkg.in/yaml.v2"
)

const (
	ResultStatusPassed  string = "passed"
	ResultStatusSkipped string = "skipped"
	ResultStatusFailed  string = "failed"
)

type Result struct {
	Status      string  `json:"status"`
	CodeDesc    string  `json:"code_desc"`
	RunTime     float32 `json:"run_time"`
	StartTime   string  `json:"start_time,omitempty"`
	Message     string  `json:"message,omitempty"`
	SkipMessage string  `json:"skip_message,omitempty"`
}

type SourceLocation struct {
	Ref  string `json:"ref"`
	Line int    `json:"line"`
}

// control impact names
const MINOR = "minor"
const MAJOR = "major"
const CRITICAL = "critical"

type Control struct {
	ID             string                       `json:"id"`
	Code           string                       `json:"code"`
	Desc           string                       `json:"desc"`
	Impact         float32                      `json:"impact"`
	Title          string                       `json:"title"`
	SourceLocation *reportingapi.SourceLocation `json:"source_location"`
	Refs           *json.RawMessage             `json:"refs"`
	Tags           *json.RawMessage             `json:"tags"`
	Results        []*reportingapi.Result       `json:"results,omitempty"`
}

// Status calculates the overall status of all controls based on all results
func (control *Control) Status() string {
	status := ResultStatusPassed

	for _, result := range control.Results {
		if result.Status == ResultStatusFailed {
			status = ResultStatusFailed
			break
		} else if result.Status == ResultStatusSkipped {
			status = ResultStatusSkipped
		}
	}

	return status
}

// ImpactName returns a human readable name for the impact
func (control *Control) ImpactName() string {
	name := CRITICAL
	if control.Impact < 0.4 {
		name = MINOR
	} else if control.Impact < 0.7 {
		name = MAJOR
	}
	return name
}

type Report struct {
	ID            string    `json:"id"`
	InSpecVersion string    `json:"version"`
	Profiles      []Profile `json:"profiles"`
	Platform      Platform  `json:"platform"`
	Statistics    struct {
		Duration float32 `json:"duration"`
	} `json:"statistics"`
}

type Platform struct {
	Name    string `json:"name"`
	Release string `json:"release"`
}

// ToJSON is a simple helper convert Metadata into a json string
func (report *Report) ToJSON() ([]byte, error) {
	return json.Marshal(report)
}

type Attribute struct {
	Name    string `json:"name"`
	Options struct {
		Description string           `json:"description"`
		Default     *json.RawMessage `json:"default"` // type varies and can be string and int
	} `json:"options,omitempty"`
}

// see https://github.com/chef/inspec/blob/master/docs/profiles.md
type Dependency struct {
	Name        string `yaml:"name" json:"name"`
	URL         string `yaml:"url,omitempty" json:"url,omitempty"`
	Path        string `yaml:"path,omitempty" json:"path,omitempty"`
	Git         string `yaml:"git,omitempty" json:"git,omitempty"`
	Branch      string `yaml:"branch,omitempty" json:"branch,omitempty"`
	Tag         string `yaml:"tag,omitempty" json:"tag,omitempty"`
	Commit      string `yaml:"commit,omitempty" json:"commit,omitempty"`
	Version     string `yaml:"version,omitempty" json:"version,omitempty"`
	Supermarket string `yaml:"supermarket,omitempty" json:"supermarket,omitempty"`
	Github      string `yaml:"github,omitempty" json:"github,omitempty"`
	Compliance  string `yaml:"compliance,omitempty" json:"compliance,omitempty"`
	Status      string `yaml:"status,omitempty" json:"status,omitempty"`
	SkipMessage string `yaml:"skip_message,omitempty" json:"skip_message,omitempty"`
}

type Group struct {
	ID       string   `json:"id"`
	Title    *string  `json:"title"` //this needs to be a string pointer as in inspec, this is stored as null instead of ""
	Controls []string `json:"controls"`
}

type Profile struct {
	Name           string                     `json:"name"`
	Title          string                     `json:"title"`
	Version        string                     `json:"version"`
	Summary        string                     `json:"summary"`
	Maintainer     string                     `json:"maintainer"`
	License        string                     `json:"license"`
	Copyright      string                     `json:"copyright"`
	CopyrightEmail string                     `json:"copyright_email"`
	Controls       []Control                  `json:"controls"`
	Supports       []map[string]string        `json:"supports"`
	Attributes     []*Attribute               `json:"attributes"`
	Dependencies   []*reportingapi.Dependency `json:"depends,omitempty"`
	Sha256         string                     `json:"sha256"`
	Groups         []*reportingapi.Group      `json:"groups"`
	Status         string                     `json:"status,omitempty"`
	SkipMessage    string                     `json:"skip_message,omitempty"`
}

// ToJSON is a simple helper convert Metadata into a json string
func (profile *Profile) ToJSON() ([]byte, error) {
	return json.Marshal(profile)
}

// FromJSON turns a profile in JSON format in Profile struct
func (profile *Profile) FromJSON(content []byte) error {
	return json.Unmarshal(content, &profile)
}

type CheckMessage struct {
	File      string `json:"file,omitempty"`
	Line      *int   `json:"line,omitempty"`
	Column    *int   `json:"column,omitempty"`
	ControlId string `json:"control_id,omitempty"`
	Msg       string `json:"msg"`
}

type CheckResult struct {
	Summary struct {
		Valid     bool   `json:"valid"`
		Timestamp string `json:"timestamp"`
		Location  string `json:"location"`
		Controls  int    `json:"controls"`
	} `json:"summary"`
	Errors   []CheckMessage `json:"errors"`
	Warnings []CheckMessage `json:"warnings"`
}

// ToJSON is a simple helper convert Metadata into a json string
func (results *CheckResult) ToJSON() ([]byte, error) {
	return json.Marshal(results)
}

// Support is part of Metadata, holding information about the supported platform
type Support struct {
	OSName        string `yaml:"os-name,omitempty" json:"os-name,omitempty"`
	OSFamily      string `yaml:"os-family,omitempty" json:"os-family,omitempty"`
	Release       string `yaml:"release,omitempty" json:"release,omitempty"`
	InSpecVersion string `yaml:"inspec,omitempty" json:"inspec,omitempty"`
	Platform      string `yaml:"platform,omitempty" json:"platform,omitempty"`
}

type Metadata struct {
	Name            string              `yaml:"name" json:"name"`
	Title           string              `yaml:"title"  json:"title"`
	Maintainer      string              `yaml:"maintainer" json:"maintainer"`
	Copyright       string              `yaml:"copyright" json:"copyright"`
	Copyright_Email string              `yaml:"copyright_email" json:"copyright_email"`
	License         string              `yaml:"license" json:"license"`
	Summary         string              `yaml:"summary" json:"summary"`
	Version         string              `yaml:"version" json:"version"`
	Supports        []map[string]string `yaml:"supports" json:"supports"`
	Dependencies    []Dependency        `yaml:"depends" json:"depends"`
	LatestVersion   string              `json:"latest_version,omitempty"`
	Sha256          string              `json:"sha256"`
}

// Parse takes a yml byte array and parses the content into Metadata
func (yml *Metadata) Parse(data []byte) error {
	return yaml.Unmarshal(data, &yml)
}

// ParseJSON takes a json byte representation and parses the content into Metadata
func (yml *Metadata) ParseJSON(data []byte) error {
	return json.Unmarshal(data, &yml)
}

// OSInfo contains simple information on a target operating system
type OSInfo struct {
	OSArch     string   `json:"arch"`
	OSFamilies []string `json:"families"`
	OSName     string   `json:"name"`
	OSRelease  string   `json:"release"`
}

const CONN_REFUSED = "connection refused"
const AUTH_FAILED = "authentication failed"
const SUDO_PW_REQUIRED = "sudo password required"
const WRONG_SUDO_PW = "wrong sudo password"
const NO_SUDO = "no sudo"
const CONN_TIMEOUT = "connection timed out"
const UNREACHABLE_HOST = "unreachable host"
const RESPONSE_ERROR = "response error"
const UNSUPPORTED_OS = "unsupported OS"
const UNKNOWN_ERROR = "unknown error"
const INVALID_PARAM = "invalid parameter"
const INVALID_OUTPUT = "invalid output"
const NO_CREDS_PROVIDED = "no credentials provided"

// Error describes a failing connection and provides
// a type for each error as well as a simple message to explain
// it in a human-readable form.
type Error struct {
	Type    string `json:"error"`
	Message string `json:"message"`
}

type Secrets struct {
	User              string   `json:"user,omitempty"`
	Password          string   `json:"password,omitempty"`
	KeyFiles          []string `json:"key_files,omitempty"`
	SudoPassword      string   `json:"sudo_password,omitempty"`
	SudoOptions       string   `json:"sudo_options,omitempty"`
	AwsUser           string   `json:"aws_user,omitempty"`
	AwsPassword       string   `json:"aws_password,omitempty"`
	AzureClientID     string   `json:"azure_client_id,omitempty"`
	AzureClientSecret string   `json:"azure_client_secret,omitempty"`
	AzureTenantID     string   `json:"azure_tenant_id,omitempty"`
	GcpCredsJson      string   `json:"gcp_creds_json,omitempty"`
	AwsSessionToken   string   `json:"aws_session_token,omitempty"`
}

type TargetBaseConfig struct {
	Backend        string              `json:"backend,omitempty"`
	Hostname       string              `json:"host,omitempty"`
	Port           int                 `json:"port,omitempty"`
	LoginPath      string              `json:"login_path,omitempty"` // winrm
	Sudo           bool                `json:"sudo,omitempty"`
	Format         string              `json:"format,omitempty"`
	Reporter       map[string]Reporter `json:"reporter,omitempty"`
	Ssl            bool                `json:"ssl,omitempty"`
	SslSelfSigned  bool                `json:"self_signed,omitempty"`
	BackendCache   bool                `json:"backend_cache,omitempty"`
	Region         string              `json:"region,omitempty"`
	SubscriptionId string              `json:"subscription_id,omitempty"`
	AttributesJson *json.RawMessage    `json:"attributes,omitempty"`
}

// TargetConfig is inspec's JSON config options
type TargetConfig struct {
	TargetBaseConfig
	Secrets
	SecretsArr []*Secrets `json:"creds_arr,omitempty"`
}

type Reporter struct {
	Url         string `json:"url,omitempty"`
	Token       string `json:"token,omitempty"`
	NodeID      string `json:"node_uuid,omitempty"`
	NodeName    string `json:"node_name,omitempty"`
	Environment string `json:"environment,omitempty"`
	ReportUUID  string `json:"report_uuid,omitempty"`
	JobUUID     string `json:"job_uuid,omitempty"`
}

const (
	BackendSSH        = "ssh"
	BackendSSM        = "ssm"
	BackendWinRm      = "winrm"
	BackendSSMWindows = "ssm:windows"
	BackendAZ         = "az-run-command"
	BackendAZWindows  = "az-run-command:windows"
	BashScript        = "bash"
	PowershellScript  = "powershell"
)
