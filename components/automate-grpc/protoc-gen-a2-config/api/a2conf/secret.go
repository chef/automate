package a2conf

import (
	"errors"

	"github.com/golang/protobuf/ptypes/wrappers"
)

type SecretBind interface {
	ListSecrets() []SecretInfo
	GetSecret(name string) *wrappers.StringValue
	SetSecret(name string, value *wrappers.StringValue) error
}

// SecretInfo describes a bindable secret
type SecretInfo struct {
	Name                string
	EnvironmentVariable string
}

var ErrSecretNotFound = errors.New("secret not found")

func IsErrSecretNotFound(err error) bool {
	return errors.Is(err, ErrPortNotFound)
}
