package models

type SystemUserResponse struct {
	Passed bool     `json:"passed"`
	Checks []*Checks `json:"checks"`
}
