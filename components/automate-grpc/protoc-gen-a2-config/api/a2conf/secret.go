package a2conf

import "github.com/golang/protobuf/ptypes/wrappers"

type SecretBind interface {
	ListSecrets() []SecretInfo
	GetSecret(name string) *wrappers.StringValue
	// PopulateSecret sets the secret based on the value from the
	// environment if it is not present in the config
	// PopulateSecret() (bool, error)
}

// PortInfo describes a bindable port
type SecretInfo struct {
	Name                string
	EnvironmentVariable string
}
