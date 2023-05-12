package models

type HardwareResourceRequest struct {
	AutomateNodeCount        int      `json:"automate_node_count"`
	AutomateNodeIps          []string `json:"automate_node_ips"`
	ChefInfraServerNodeCount int      `json:"chef_infra_server_node_count"`
	ChefInfraServerNodeIps   []string `json:"chef_infra_server_node_ips"`
	PostgresqlNodeCount      int      `json:"postgresql_node_count"`
	PostgresqlNodeIps        []string `json:"postgresql_node_ips"`
	OpenSearchNodeCount      int      `json:"opensearch_node_count"`
	OpenSearchNodeIps        []string `json:"opensearch_node_ips"`
}

type HardwareResourceResponse struct {
	IP       string   `json:"ip"`
	NodeType string   `json:"node_type"`
	Checks   []Checks `json:"checks"`
}

type CountPerHARequirements struct {
	AutomateCount        int `json:"automate_count"`
	ChefInfraServerCount int `json:"chef_infra_server_count"`
	PostgresqlCount      int `json:"postgresql_count"`
	OpenSearchCount      int `json:"opensearch_count"`
}
