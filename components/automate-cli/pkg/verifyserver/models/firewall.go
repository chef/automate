package models

type FirewallResponse struct {
	Passed bool     `json:"passed"`
	Checks []Checks `json:"checks"`
}
