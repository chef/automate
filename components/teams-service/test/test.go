package test

import (
	"net/url"
	"os"

	"github.com/chef/automate/components/teams-service/storage/postgres/migration"
	"github.com/chef/automate/lib/logger"
)

// MigrationConfigIfPGTestsToBeRun either returns the pg migration config
// if PG_URL is set or we are in CI system, otherwise it returns nil, indicating
// postgres based tests shouldn't be run.
func MigrationConfigIfPGTestsToBeRun(l logger.Logger, migrationPath string) (*migration.Config, error) {
	customPGURL, pgURLPassed := os.LookupEnv("PG_URL")
	ciMode := os.Getenv("CI") == "true"

	// If in ciMode, use the default
	if ciMode {
		pgURL, err := url.Parse("postgresql://postgres@127.0.0.1:5432/teams_test?sslmode=disable")
		return &migration.Config{
			Path:   migrationPath,
			Logger: l,
			PGURL:  pgURL,
		}, err
	}

	// If PG_URL wasn't passed (and we aren't in CI)
	// we shouldn't run the postgres tests, return nil.
	if !pgURLPassed {
		return nil, nil
	}

	pgURL, err := url.Parse(customPGURL)
	if err != nil {
		return nil, err
	}

	return &migration.Config{
		Path:   migrationPath,
		Logger: l,
		PGURL:  pgURL,
	}, nil
}
