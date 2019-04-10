package backend

import "time"

//Liveness node ping message
type Liveness struct {
	NodeID          string    `json:"entity_uuid"`
	Checkin         time.Time `json:"checkin"`
	LivenessManaged bool      `json:"liveness_managed"`
	Organization    string    `json:"organization_name"`
	NodeName        string    `json:"node_name"`
	SourceFQDN      string    `json:"source_fqdn"`
}
