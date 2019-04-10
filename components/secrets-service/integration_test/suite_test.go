package integration_test

import (
	"fmt"
	"os"

	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/components/secrets-service/dao"
)

// Suite helps you manipulate various stages of your tests, it provides
// common functionality like; Initialization and deletion hooks, ingestion of
// Nodes, Runs, and more. If you have some functionality that is repetitive across
// multiple tests, consider putting it here so that we have them available globally
type Suite struct {
}

// Initialize the test suite
func NewSuite(secretsDb *dao.DB) *Suite {
	s := new(Suite)

	err := secretsDb.Ping()
	if err != nil {
		fmt.Printf("Could not connect to postgresql'%s'\n", err)
		os.Exit(1)
	}

	return s
}

// GlobalSetup is the place where you prepare anything that we need before
func (s *Suite) GlobalSetup() {
	deleteAllSecrets()
}

// GlobalTeardown is the place where you tear everything down after we have finished
func (s *Suite) GlobalTeardown() {
	deleteAllSecrets()
}

func deleteAllSecrets() {
	secrets, _, err := secretsDb.GetSecrets("", secrets.Query_ASC, 0, 0, nil)
	if err != nil {
		fmt.Printf("error clearing tables %v\n", err)
	}

	for _, secret := range secrets {
		_, err := secretsDb.DeleteSecret(secret.Id)
		if err != nil {
			fmt.Printf("error deleting secret with ID %v error: %v\n", secret.Id, err)
		}
	}
}

func appendKvs(kvs ...*secrets.Kv) []*secrets.Kv {
	array := make([]*secrets.Kv, len(kvs))

	for i, kv := range kvs {
		array[i] = kv
	}

	return array
}

func appendFilters(filters ...*secrets.Filter) []*secrets.Filter {
	array := make([]*secrets.Filter, len(filters))

	for i, filter := range filters {
		array[i] = filter
	}

	return array
}
