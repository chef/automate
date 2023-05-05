package models

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
	OpensearchNodeCount      int      `json:"opensearch_node_count"`
	OpensearchNodeIps        []string `json:"opensearch_node_ips"`
}

type Config struct {
	SshUser struct {
		Username     string `json:"user_name"`
		PrivateKey   string `json:"private_key"`
		SudoPassword string `json:"sudo_password"`
	} `json:"ssh_user"`
	Arch   string `json:"arch"`
	Backup struct {
		FileSystem struct {
			MountLocation string `json:"mount_location"`
		} `json:"file_system"`
		ObjectStorage struct {
			Endpoin    string `json:"endpoint"`
			BucketName string `json:"bucket_name"`
			BasePath   string `json:"base_path"`
			AccessKey  string `json:"access_key"`
			SecretKey  string `json:"secret_key"`
		}
	} `json:"backup"`
	Hardware    Hardware `json:"hardware"`
	Certificate struct {
		Fqdn     string     `json:"fqdn"`
		RootCert string     `json:"root_cert"`
		Nodes    []NodeCert `json:"nodes"`
	} `json:"certificate"`
	ExternalOS struct {
		OSDomainName   string `json:"opensearch_domain_name"`
		OSDomainURL    string `json:"opensearch_domain_url"`
		OSUsername     string `json:"opensearch_usename"`
		OSUserPassword string `json:"opensearch_user_password"`
		OSCert         string `json:"opensearch_cert"`
	} `json:"external_opensearch"`
	ExternalPG struct {
		PGInstanceURL       string `json:"postgresql_instance_url"`
		PGSuperuserName     string `json:"postgresql_superuser_username"`
		PGSuperuserPassword string `json:"postgresql_superuser_password"`
		PGDbUserName        string `json:"postgresql_dbuser_username"`
		PGDbUserPassword    string `json:"postgresql_dbuser_password"`
		PGRootCert          string `json:"postgresql_root_cert"`
	} `json:"external_postgresql"`
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
	Status string    `json:"status"`
	Result ApiResult `json:"result"`
}
type ApiResult struct {
	Passed  bool            `json:"passed"`
	Message string          `json:"msg"`
	Check   string          `json:"check"`
	Checks  []CheckResponse `json:"checks"`
}
type CheckResponse struct {
	Checks struct {
		Title         string `json:"title"`
		Passed        bool   `json:"passed"`
		SuccessMsg    string `json:"success_msg"`
		ErrorMsg      string `json:"error_msg"`
		ResolutionMsg string `json:"resolution_msg"`
	}
}

type NodeCert struct {
	IP        string `json:"ip"`
	Cert      string `json:"cert"`
	Key       string `json:"key"`
	AdminKey  string `json:"admin_key"`
	AdminCert string `json:"admin_cert"`
}
