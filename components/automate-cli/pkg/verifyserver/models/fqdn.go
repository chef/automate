package models

type FqdnResponse struct {
	Passed bool     `json:"passed"`
	Checks []Checks `json:"checks"`
}
