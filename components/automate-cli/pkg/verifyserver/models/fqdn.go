package models

type FqdnRequest struct {
	Fqdn              string   `json:"fqdn"`
	RootCert          string   `json:"root_cert"`
	IsAfterDeployment bool     `json:"is_after_deployment"`
	ApiToken          string   `json:"api_token"`
	Nodes             []string `json:"nodes"`
}

type FqdnResponse struct {
	Passed bool     `json:"passed"`
	Checks []Checks `json:"checks"`
}
