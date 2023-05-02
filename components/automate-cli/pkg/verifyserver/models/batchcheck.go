package models

type BatchCheckRequest struct {
	Checks []string `json:"checks"`
	Config Config   `json:"config"`
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
	Hardware struct {
		AutomateNodeCount        int      `json:"automate_node_count"`
		AutomateNodeIps          []string `json:"automate_node_ips"`
		ChefInfraServerNodeCount int      `json:"chef_infra_server_node_count"`
		ChefInfraServerNodeIps   []string `json:"chef_infra_server_node_ips"`
		PostgresqlNodeCount      int      `json:"postgresql_node_count"`
		PostgresqlNodeIps        []string `json:"postgresql_node_ips"`
		OpensearchNodeCount      int      `json:"opensearch_node_count"`
		OpensearchNodeIps        []string `json:"opensearch_node_ips"`
	} `json:"hardware"`
}
