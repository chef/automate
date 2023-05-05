package main

import (
	"fmt"
	"os"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/server"
)

func main() {
	vs, err := server.NewVerifyServer(server.DEFAULT_PORT, true)
	if err != nil {
		fmt.Println(err)
		os.Exit(1)
	}
	vs.Start()
}
