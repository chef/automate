package pgdb

import (
	"encoding/json"
	"time"
)

type tag struct {
	ID    string `db:"id"`
	Key   string `db:"key"`
	Value string `db:"value"`
}

// node used for DB operations(insert, delete)
type node struct {
	ID              string          `db:"id"`
	Name            string          `db:"name"`
	Platform        string          `db:"platform"`
	PlatformVersion string          `db:"platform_version"`
	Manager         string          `db:"manager"`
	TargetConfig    json.RawMessage `db:"target_config"`
	Tags            json.RawMessage `db:"-"`
	Secrets         []string        `db:"-"`
}

// NodeTag used only to (de)serialize database access
type NodeTag struct {
	NodeID string `db:"node_id"`
	TagID  string `db:"tag_id"`
}

// NodeSecret used only to (de)serialize database access
type NodeSecret struct {
	NodeID   string `db:"node_id"`
	SecretID string `db:"secret_id"`
}

type nodeManager struct {
	ID                  string          `db:"id"`
	Name                string          `db:"name"`
	Type                string          `db:"type"`
	Credential          string          `db:"credentials"`
	InstanceCredentials json.RawMessage `db:"instance_credentials"`
	Status              string          `db:"status_type"`
	StatusMessage       string          `db:"status_message"`
	AccountID           string          `db:"account_id"`
	DateAdded           time.Time       `db:"date_added"`
}
