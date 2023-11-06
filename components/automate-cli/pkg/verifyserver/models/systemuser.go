package models

type SystemUserResponse struct {
	Passed bool         `json:"passed"`
	Checks []*Checks    `json:"checks"`
	Id     SystemUserID `json:"id,omitempty"`
}

type SystemUserID struct {
	UserID  string `json:"user_id,omitempty"`
	GroupID string `json:"group_id,omitempty"`
}
