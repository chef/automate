package models

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
