package models

type SoftwareVersionDetails struct {
	Passed bool     `json:"passed"`
	Checks []Checks `json:checks`
}
