//
//  Author:: Salim Afiune <afiune@chef.io>
//  Author:: Christoph Hartmann <chartmann@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.

package main

import (
	"os"

	"github.com/chef/automate/components/ingest-service/cmd/ingest-service/commands"
	"github.com/chef/automate/components/ingest-service/config"
	"github.com/chef/automate/components/ingest-service/storage"
	log "github.com/sirupsen/logrus"
)

func main() {

	configPath := "/hab/svc/ingest-service/config"

	// ✅ Load configuration dynamically
	cfg, err := config.Load(configPath) // Ensure this function is implemented in config package
	if err != nil {
		log.Fatalf("Failed to load configuration: %v", err)
		os.Exit(1)
	}

	// ✅ Connect to DB and run migrations before starting the service
	db, err := storage.ConnectAndMigrate(&cfg.Storage)
	if err != nil {
		log.Fatalf("Failed to initialize database: %v", err)
		os.Exit(1)
	}

	log.Info("✅ Database connection established and migrations applied successfully!")

	// Start the ingest service
	commands.Execute()

	// To avoid unused variable errors (remove if db is used elsewhere)
	_ = db
}
