package main

import (
	"context"
	"fmt"
	"io/ioutil"
	"os"
	"time"

	"github.com/ghodss/yaml"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/local_user"
	"github.com/chef/automate/components/local-user-service/config"
	"github.com/chef/automate/lib/grpc/secureconn"
)

func checkHealth(configFile string) {
	configData, err := ioutil.ReadFile(configFile)
	if err != nil {
		fmt.Printf("failed to read config file %s: %s\n", configFile, err.Error())
		os.Exit(1)
	}

	var c config.Config
	if err := yaml.Unmarshal(configData, &c); err != nil { // nolint: vetshadow
		fmt.Printf("error parsing config file %s: %s\n", configFile, err.Error())
		os.Exit(1)
	}

	c.FixupRelativeTLSPaths(configFile)

	serviceCerts, err := c.ReadCerts()
	if err != nil {
		fmt.Printf("failed to read tls keys/certs: %s\n", err.Error())
		os.Exit(1)
	}

	// set timeout to 1s because we'll rerun the health check on failure
	ctx, cancel := context.WithTimeout(context.Background(), 1*time.Second)
	defer cancel()

	connFactory := secureconn.NewFactory(*serviceCerts)

	connection, err := connFactory.DialContext(
		ctx,
		"local-user-service",
		c.GRPC,
		grpc.WithBlock(),
	)
	// Note: this won't be called on os.Exit(), so we're only closing in the happy
	// path
	defer connection.Close() // nolint

	if err != nil {
		fmt.Printf("error connecting to local-user-service at %s\n", c.GRPC)
		os.Exit(2) // CRIT 503
	}
	userClient := local_user.NewUsersMgmtClient(connection)
	email := local_user.Email{Email: "heath-check@habitat.sh"}
	_, err = userClient.GetUser(context.Background(), &email)
	if err != nil && status.Code(err) != codes.NotFound {
		fmt.Printf("error calling local_user.GetUser: %v", err)
		os.Exit(1) // WARN 200
	}

	// OK 200
}

func printUsageExit() {
	fmt.Printf("usage: %s CONFIG_FILE\n  CONFIG_FILE: local-user-service config file\n", os.Args[0])
	os.Exit(3) // UNKNOWN 500
}

func main() {
	if len(os.Args) < 2 {
		printUsageExit()
	}
	configFile := os.Args[1]
	checkHealth(configFile)
}
