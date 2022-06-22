package integration_test

import (
	"fmt"
	"os"

	"github.com/chef/automate/api/external/common/query"
	"github.com/chef/automate/components/user-settings-service/pkg/storage/postgres"
)

// Suite helps you manipulate various stages of your tests, it provides
// common functionality like; Initialization and deletion hooks, ingestion of
// Nodes, Runs, and more. If you have some functionality that is repetitive across
// multiple tests, consider putting it here so that we have them available globally
type Suite struct {
}

// Initialize the test suite
func NewSuite(db *postgres.DB) *Suite {
	s := new(Suite)

	err := db.Db.Ping()
	if err != nil {
		fmt.Printf("Could not connect to postgresql'%s'\n", err)
		os.Exit(1)
	}

	return s
}

// GlobalSetup is the place where you prepare anything that we need before
func (s *Suite) GlobalSetup() {
	deleteAllUserSettings()
}

// GlobalTeardown is the place where you tear everything down after we have finished
func (s *Suite) GlobalTeardown() {
	deleteAllUserSettings()
}

func deleteAllUserSettings() {
	err := db.TruncateTables()
	if err != nil {
		fmt.Printf("error clearing tables %v\n", err)
	}
}

func appendKvs(kvs ...*query.Kv) []*query.Kv {
	array := make([]*query.Kv, len(kvs))

	for i, kv := range kvs {
		array[i] = kv
	}

	return array
}

func appendFilters(filters ...*query.Filter) []*query.Filter {
	array := make([]*query.Filter, len(filters))

	for i, filter := range filters {
		array[i] = filter
	}

	return array
}
