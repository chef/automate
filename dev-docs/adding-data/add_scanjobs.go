package main

import (
	"context"
	"encoding/base64"
	"fmt"
	"log"
	"os"

	"github.com/chef/automate/api/external/common/query"
	gwnodes "github.com/chef/automate/api/external/nodes"
	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/compliance/jobs"
	"github.com/chef/automate/components/automate-gateway/gateway"
	"github.com/chef/automate/components/compliance-service/examples/helpers"
	"github.com/spf13/viper"
	"google.golang.org/grpc"
)

const (
	gatewayBindAddress = "127.0.0.1:2001"
)

type GatewayTestSuite struct {
	ctx     context.Context
	clients gateway.ClientsFactory
	gwConn  *grpc.ClientConn
}

type acceptanceTarget struct {
	Key   string
	Host  string
	Host2 string
	User  string
}

func getAcceptanceTargetFromEnvironment() *acceptanceTarget {
	return &acceptanceTarget{
		Key:  acceptanceTargetKey(),
		Host: os.Getenv("AUTOMATE_ACCEPTANCE_TARGET_HOST"),
		User: os.Getenv("AUTOMATE_ACCEPTANCE_TARGET_USER"),
	}
}

func acceptanceTargetKey() string {
	defaultKey := "default_key"

	if encoded := os.Getenv("AUTOMATE_ACCEPTANCE_TARGET_KEY"); encoded != "" {
		key, err := base64.StdEncoding.DecodeString(encoded)
		if err != nil {
			return defaultKey
		}

		return string(key)
	}

	return defaultKey
}

func NewGatewaySuite(ctx context.Context) (*GatewayTestSuite, error) {
	connFactory := helpers.SecureConnFactoryHabWithDeploymentServiceCerts()
	viper.SetConfigFile("/hab/svc/automate-gateway/config/config.toml")
	err := viper.ReadInConfig()
	if err != nil {
		return nil, err
	}
	cfg, err := gateway.ConfigFromViper()
	clients, err := gateway.NewClientsFactory(cfg.GrpcClients, connFactory)
	if err != nil {
		return nil, err
	}
	gwConn, err := connFactory.Dial("automate-gateway", gatewayBindAddress)
	if err != nil {
		return nil, err
	}

	return &GatewayTestSuite{
		ctx:     ctx,
		clients: clients,
		gwConn:  gwConn,
	}, nil
}

func main() {
	fmt.Println("setting up clients")

	targets := getAcceptanceTargetFromEnvironment()

	if len(targets.Host) == 0 {
		log.Fatal("\n\nsecrets are missing. pls check the contents of dev/secrets-env.sh and run the get_secrets script if this is empty\n")
	}

	suite, err := NewGatewaySuite(context.Background())
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println("\n\n!!!!!!!!!!!!!!!!!!!!!!\nNOTE: YOU MUST BE ON VPN TO GET WORKING SCAN JOBS\n!!!!!!!!!!!!!!!!!!!!!!\n\n")

	// get the gateway nodes client
	gatewayNodesClient := gwnodes.NewNodesServiceClient(suite.gwConn)

	// setup secrets service for secret creation
	secretsClient, err := suite.clients.SecretClient()

	// setup compliance-service clients for job creation
	jobsClient, err := suite.clients.ComplianceJobsServiceClient()

	fmt.Println("creating two secrets for the nodes")

	ec2SecretID, err := secretsClient.Create(suite.ctx, &secrets.Secret{
		Name: "ecw secret",
		Type: "ssh",
		Data: []*query.Kv{
			{Key: "username", Value: targets.User},
			{Key: "key", Value: targets.Key},
		},
	})
	vagrantSecretID, err := secretsClient.Create(suite.ctx, &secrets.Secret{
		Name: "vagrant secret",
		Type: "ssh",
		Data: []*query.Kv{
			{Key: "username", Value: "vagrant"},
			{Key: "password", Value: "vagrant"},
		},
	})

	fmt.Println("creating three nodes")

	ec2NodeID, err := gatewayNodesClient.Create(suite.ctx, &gwnodes.Node{
		Name: targets.Host,
		Tags: []*query.Kv{
			{Key: "environment", Value: "test-env"},
		},
		TargetConfig: &gwnodes.TargetConfig{
			Backend: "ssh",
			Host:    targets.Host,
			Port:    22,
			Secrets: []string{ec2SecretID.GetId()},
		},
	})
	ec2NodeID2, err := gatewayNodesClient.Create(suite.ctx, &gwnodes.Node{
		Name: "inspec-target-rhel7-acceptance.cd.chef.co",
		Tags: []*query.Kv{
			{Key: "environment", Value: "test-env"},
		},
		TargetConfig: &gwnodes.TargetConfig{
			Backend: "ssh",
			Host:    "inspec-target-rhel7-acceptance.cd.chef.co",
			Port:    22,
			Secrets: []string{ec2SecretID.GetId()},
		},
	})
	vagrantNodeID, err := gatewayNodesClient.Create(suite.ctx, &gwnodes.Node{
		Name: "vagrant node - localhost",
		Tags: []*query.Kv{
			{Key: "environment", Value: "test-env-local"},
		},
		TargetConfig: &gwnodes.TargetConfig{
			Backend: "ssh",
			Host:    "localhost",
			Port:    22,
			Secrets: []string{vagrantSecretID.GetId()},
		},
	})

	fmt.Println("creating three scan jobs")

	jobsClient.Create(suite.ctx, &jobs.Job{
		Name:     "ec2 node job - inspec-target-rhel7-dev.cd.chef.co",
		Type:     "exec",
		Nodes:    []string{ec2NodeID.GetId()},
		Profiles: []string{"https://github.com/dev-sec/ssh-baseline/archive/master.tar.gz"},
	})

	jobsClient.Create(suite.ctx, &jobs.Job{
		Name:     "ec2 node job - inspec-target-rhel7-acceptance.cd.chef.co",
		Type:     "exec",
		Nodes:    []string{ec2NodeID2.GetId()},
		Profiles: []string{"https://github.com/dev-sec/ssh-baseline/archive/master.tar.gz"},
	})

	jobsClient.Create(suite.ctx, &jobs.Job{
		Name:     "vagrant node job",
		Type:     "exec",
		Nodes:    []string{vagrantNodeID.GetId()},
		Profiles: []string{"https://github.com/dev-sec/ssh-baseline/archive/master.tar.gz"},
	})

}
