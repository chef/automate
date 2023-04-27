package main

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver"

func main() {
	verifyserver.StartVerifyServer(verifyserver.DEFAULT_PORT, true)
}
