package main

import verifyserver "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"

func main() {
	verifyserver.StartVerifyServer(verifyserver.DEFAULT_PORT, true)
}
