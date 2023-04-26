package main

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver"

func main() {
	vs := verifyserver.NewVerifyServer(verifyserver.DEFAULT_PORT, true)
	vs.Start()
}
