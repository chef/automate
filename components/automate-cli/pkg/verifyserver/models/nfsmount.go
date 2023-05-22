package models

type NFSMountRequest struct {
	AutomateNodeIPs        []string `json:"automate_node_ips"`
	ChefInfraServerNodeIPs []string `json:"chef_infra_server_node_ips"`
	PostgresqlNodeIPs      []string `json:"postgresql_node_ips"`
	OpensearchNodeIPs      []string `json:"opensearch_node_ips"`
	MountLocation          string   `json:"mount_location"`
}

type NFSMountResponse struct {
	IP        string   `json:"ip"`
	NodeType  string   `json:"node_type"`
	CheckList []Checks `json:"checks"`
	Error     error    `json:"error"`
}

type NFSMountLocRequest struct {
	MountLocation string `json:"mount_location"`
}

type NFSMountLocResponse struct {
	Address            string `json:"address"`
	MountLocation      string `json:"mount_location"`
	Nfs                string `json:"nfs"`
	StorageCapacity    string `json:"storage_capacity"`
	AvailableFreeSpace string `json:"Available_free_space"`
}
