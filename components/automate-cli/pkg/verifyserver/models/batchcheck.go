package models

import "github.com/gofiber/fiber"

type BatchCheckRequest struct {
	Checks []string `json:"checks"`
	Config Config   `json:"config"`
}

type Hardware struct {
	AutomateNodeCount        int      `json:"automate_node_count"`
	AutomateNodeIps          []string `json:"automate_node_ips"`
	ChefInfraServerNodeCount int      `json:"chef_infra_server_node_count"`
	ChefInfraServerNodeIps   []string `json:"chef_infra_server_node_ips"`
	PostgresqlNodeCount      int      `json:"postgresql_node_count"`
	PostgresqlNodeIps        []string `json:"postgresql_node_ips"`
	OpenSearchNodeCount      int      `json:"opensearch_node_count"`
	OpenSearchNodeIps        []string `json:"opensearch_node_ips"`
}
type SSHUser struct {
	Username     string `json:"user_name"`
	PrivateKey   string `json:"private_key"`
	SudoPassword string `json:"sudo_password"`
}

type FileSystem struct {
	MountLocation string `json:"mount_location"`
}
type ObjectStorage struct {
	Endpoint   string `json:"endpoint"`
	BucketName string `json:"bucket_name"`
	BasePath   string `json:"base_path"`
	AccessKey  string `json:"access_key"`
	SecretKey  string `json:"secret_key"`
}
type Backup struct {
	FileSystem    FileSystem    `json:"file_system"`
	ObjectStorage ObjectStorage `json:"object_storage"`
}

type Certificate struct {
	Fqdn     string     `json:"fqdn"`
	RootCert string     `json:"root_cert"`
	Nodes    []NodeCert `json:"nodes"`
}

type ExternalOS struct {
	OSDomainName   string `json:"opensearch_domain_name"`
	OSDomainURL    string `json:"opensearch_domain_url"`
	OSUsername     string `json:"opensearch_usename"`
	OSUserPassword string `json:"opensearch_user_password"`
	OSCert         string `json:"opensearch_cert"`
}

type ExternalPG struct {
	PGInstanceURL       string `json:"postgresql_instance_url"`
	PGSuperuserName     string `json:"postgresql_superuser_username"`
	PGSuperuserPassword string `json:"postgresql_superuser_password"`
	PGDbUserName        string `json:"postgresql_dbuser_username"`
	PGDbUserPassword    string `json:"postgresql_dbuser_password"`
	PGRootCert          string `json:"postgresql_root_cert"`
}

type Config struct {
	SSHUser         SSHUser     `json:"ssh_user"`
	Arch            string      `json:"arch"`
	Backup          Backup      `json:"backup"`
	Hardware        Hardware    `json:"hardware"`
	Certificate     Certificate `json:"certificate"`
	ExternalOS      ExternalOS  `json:"external_opensearch"`
	ExternalPG      ExternalPG  `json:"external_postgresql"`
	DeploymentState string      `json:"deployment_state"`
}

type BatchCheckResponse struct {
	Status string             `json:"status"`
	Result []BatchCheckResult `json:"result"`
}
type BatchCheckResult struct {
	NodeType string      `json:"node_type"`
	Ip       string      `json:"ip"`
	Tests    []ApiResult `json:"tests"`
}

type CheckTriggerResponse struct {
	Status    string       `json:"status"`
	Result    ApiResult    `json:"result"`
	Host      string       `json:"host"`
	Error     *fiber.Error `json:"error,omitempty"`
	NodeType  string       `json:"node_type"`
	CheckType string       `json:"check_type"`
}
type ApiResult struct {
	Passed   bool         `json:"passed"`
	Message  string       `json:"msg"`
	Check    string       `json:"check"`
	Checks   []Checks     `json:"checks"`
	Error    *fiber.Error `json:"error,omitempty"`
	NodeType string       `json:"node_type"`
	IP       string       `json:"ip"`
}

type Checks struct {
	Title         string `json:"title"`
	Passed        bool   `json:"passed"`
	SuccessMsg    string `json:"success_msg"`
	ErrorMsg      string `json:"error_msg"`
	ResolutionMsg string `json:"resolution_msg"`
}

type NodeCert struct {
	IP        string `json:"ip"`
	Cert      string `json:"cert"`
	Key       string `json:"key"`
	AdminKey  string `json:"admin_key"`
	AdminCert string `json:"admin_cert"`
}

type HardwareResourceCheckResponse struct {
	Status string                           `json:"status"`
	Result []HardwareResourceCountApiResult `json:"result"`
}

type HardwareResourceCountApiResult struct {
	IP       string   `json:"ip"`
	NodeType string   `json:"node_type"`
	Checks   []Checks `json:"checks"`
}
