package main

import "github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"

func main() {
	vs := server.NewVerifyServer(server.DEFAULT_PORT, true)
	vs.Start()
}
