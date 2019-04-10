package secretsint

// This package is meant to be an interface between the secrets-service and compliance code base

import (
	"context"

	"github.com/chef/automate/api/external/secrets"
)

type SecretsInterface struct {
	secretsClient secrets.SecretsServiceClient
}

func New(secretsClient secrets.SecretsServiceClient) *SecretsInterface {
	return &SecretsInterface{secretsClient}
}

// SecretExist will verify if a secret exists
func (s *SecretsInterface) secretExists(id string) bool {
	ctx := context.Background()
	_, err := s.secretsClient.Read(ctx, &secrets.Id{Id: id})
	if err != nil {
		return false
	}
	return true
}

// CheckSecrets will verify if a list of secrets exist
func (s *SecretsInterface) CheckSecrets(possibleSecrets []string) (allExist bool, existentSecrets []string, nonExistentSecrets []string) {
	for _, secret := range possibleSecrets {
		if s.secretExists(secret) {
			existentSecrets = append(existentSecrets, secret)
			continue
		}
		nonExistentSecrets = append(nonExistentSecrets, secret)
	}

	if len(nonExistentSecrets) == 0 {
		allExist = true
	}
	return
}
