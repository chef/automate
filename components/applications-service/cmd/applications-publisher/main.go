package main

import (
	"flag"
	"fmt"
	"log"
	"os"
	"strconv"
	"time"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/components/applications-service/pkg/nats"
	"github.com/chef/automate/lib/tls/certs"
)

var usageStr = `
Usage: applications-publisher

Use this command to publish messages as a habitat supervisor would publish to the A2 NATS Server.

NATS Flags:
	--uniq-client-id     Generate a unique client-id to connect to server
	--infinite-stream    Publish message every second intinitely
	--internal-nats      Connect to the Automate Internal NATS Server
	--disable-tls        Disables TLS when connecting to the server (only applies for event-gateway/external NATS)

NATS Options:
	--auth-token  <token>   Automate auth token (must have ingest permissions)
	--port        <port>    NATS port to connect (default:4222)
	--raw-message <message> Sends a raw message to the NATS Server instead of the Habitat Event message

Options to build a Habitat Event message:
	--sup-id      <id>     The Supervisor ID
	--group       <group>  The group name of a service (part of the service_group)
	--fqdn        <fqdn>   The fqdn of the server where the service is running
	--application <name>   The application name that this service is part of
	--environment <name>   The environment name of the current deployment
	--health      <code>   The health check code of a service
	--status      <code>   The status code of a service

	Package Indentifier
	--origin   <origin>  The origin of a package
	--name     <name>    The name of a package
	--version  <version> The version of a package
	--release  <release> The release of a package

	Health Check Codes:
		0:OK
		1:WARN
		2:CRITICAL
		3:UNKNOWN
	Service Status Codes:
		0:RUNNING
		1:INITIALIZING
		2:DEPLOYING
		3:DOWN
`

func usage() {
	fmt.Printf("%s\n", usageStr)
	os.Exit(0)
}

func exit(err error) {
	fmt.Printf("Error: %s\n", err.Error())
	os.Exit(0)
}

func main() {
	var (
		uniqID       bool
		infiniteLoop bool
		internalNats bool
		disableTLS   bool
		health       int
		status       int
		client       *nats.NatsClient
		authToken    string
		port         string
		rawMessage   string
		t            = time.Now()
		clientID     = "applications-publisher"
		event        = applications.HabService{
			PkgIdent: &applications.PackageIdent{},
		}
	)

	flag.StringVar(&rawMessage, "raw-message", "", "Sends a raw message to the NATS Server instead of the Habitat Event message")
	flag.StringVar(&port, "port", "4222", "NATS port to connect (default:4222)")
	flag.StringVar(&authToken, "auth-token", "", "Automate auth token (must have ingest permissions)")
	flag.StringVar(&event.SupervisorId, "sup-id", "1234567890", "The Supervisor ID")
	flag.StringVar(&event.Group, "group", "default", "The group name of a service (part of the service_group)")
	flag.StringVar(&event.Application, "application", "demo", "The application name that this service is part of")
	flag.StringVar(&event.Environment, "environment", "demo", "The environment name of the current deployment")
	flag.StringVar(&event.Fqdn, "fqdn", "localhost", "The fqdn of the server where the service is running")
	flag.StringVar(&event.PkgIdent.Origin, "origin", "core", "The origin of a package")
	flag.StringVar(&event.PkgIdent.Name, "name", "redis", "The name of a package")
	flag.StringVar(&event.PkgIdent.Version, "version", "0.1.0", "The version of a package")
	flag.StringVar(&event.PkgIdent.Release, "release", t.Format("20060102150405"), "The release of a package")
	flag.IntVar(&health, "health", 0, "The health check code of a service")
	flag.IntVar(&status, "status", 0, "The status code of a service")
	flag.BoolVar(&uniqID, "uniq-client-id", false, "Generate a unique client-id to connect to server")
	flag.BoolVar(&infiniteLoop, "infinite-stream", false, "Publish message every second infinitely")
	flag.BoolVar(&internalNats, "internal-nats", false, "Connect to the Automate Internal NATS Server")
	flag.BoolVar(&disableTLS, "disable-tls", false, "Disable TLS when connecting to NATS")

	log.SetFlags(0)
	flag.Usage = usage
	flag.Parse()

	if uniqID {
		clientID = "applications-publisher-" + strconv.FormatInt(t.UnixNano(), 10)
	}

	if internalNats {
		client = nats.New(
			fmt.Sprintf("nats://0.0.0.0:%s", port),
			"event-service", clientID, "", "habitat",
			certs.TLSConfig{
				CertPath:       "/hab/svc/applications-service/config/service.crt",
				KeyPath:        "/hab/svc/applications-service/config/service.key",
				RootCACertPath: "/hab/svc/applications-service/config/root_ca.crt",
			},
		)
	} else {
		if authToken == "" {
			fmt.Println("A valid auth token is required for connection to the event-gateway NATS service")
			fmt.Println("To publish a message without a token use the option '--internal-nats' to connect using TLS Authentication")
			os.Exit(1)
		}
		client = nats.NewExternalClient(
			fmt.Sprintf("nats://%s@0.0.0.0:%s", authToken, port),
			"event-service", clientID, "", "habitat")
		client.InsecureSkipVerify = true
		client.DisableTLS = disableTLS
	}

	// Convert proto enums
	event.HealthCheck = applications.HealthStatus(health)
	event.Status = applications.ServiceStatus(status)

	// Publish a single raw message
	if len(rawMessage) > 0 {
		err := client.Connect()
		if err != nil {
			exit(err)
		}

		err = client.PublishBytes([]byte(rawMessage))
		if err != nil {
			exit(err)
		}
		os.Exit(0)
	}

	if infiniteLoop {
		err := client.Connect()
		if err != nil {
			exit(err)
		}

		for {
			err = client.PublishHabService(&event)
			if err != nil {
				exit(err)
			}

			time.Sleep(1 * time.Second)

			// Generate a new release
			event.PkgIdent.Release = time.Now().Format("20060102150405")
		}
	} else {

		// Publish a single message
		err := client.ConnectAndPublish(&event)
		if err != nil {
			exit(err)
		}

	}
}
