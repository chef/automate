package backend

import "time"

type InternalChefAction struct {
	Id               string    `json:"id"`
	MessageType      string    `json:"message_type,omitempty"`
	MessageVersion   string    `json:"message_version,omitempty"`
	EntityName       string    `json:"entity_name,omitempty"`
	EntityType       string    `json:"entity_type,omitempty"`
	ParentName       string    `json:"parent_name,omitempty"`
	ParentType       string    `json:"parent_type,omitempty"`
	Task             string    `json:"task,omitempty"`
	OrganizationName string    `json:"organization_name,omitempty"`
	RemoteHostname   string    `json:"remote_hostname,omitempty"`
	RunId            string    `json:"run_id,omitempty"`
	NodeId           string    `json:"node_id,omitempty"`
	RecordedAt       time.Time `json:"recorded_at,omitempty"`
	RemoteRequestId  string    `json:"remote_request_id,omitempty"`
	RequestId        string    `json:"request_id,omitempty"`
	RequestorName    string    `json:"requestor_name,omitempty"`
	RequestorType    string    `json:"requestor_type,omitempty"`
	RevisionId       string    `json:"revision_id, omitempty"`
	ServiceHostname  string    `json:"service_hostname,omitempty"`
	UserAgent        string    `json:"user_agent,omitempty"`
	Data             string    `json:"data,omitempty"`
	Projects         []string  `json:"projects"`
}
