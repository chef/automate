//
//  Author:: Salim Afiune <afiune@chef.io>
//  Author:: Christoph Hartmann <chartmann@chef.io>
//  Copyright:: Copyright (c) 2019-2025 Progress Software Corporation and/or its subsidiaries or affiliates. All Rights Reserved.

package main

import "github.com/chef/automate/components/ingest-service/cmd/ingest-service/commands"

func main() {
	// Start the ingest service
	commands.Execute()
}
