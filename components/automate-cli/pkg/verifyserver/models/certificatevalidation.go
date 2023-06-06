package models

type CertificateCheckResponse struct {
	Passed bool     `json:"passed"`
	Checks []Checks `json:"checks"`
}
