// +build cfgmgmt

// This test requires that config-mgmt service is setup and running
// and matches the configuration provided in ./testdata/config.toml
//
package test

import (
	"context"
	"fmt"
	"os"
	"testing"

	"github.com/chef/automate/components/data-lifecycle-service/server"
	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"
)

var conf server.Config
var cancelFunc context.CancelFunc
var ctx context.Context
var conn *grpc.ClientConn
var client api.DataLifecycleClient

func TestMain(m *testing.M) {
	conf, err := server.ConfigFromToml("./testdata/config.ingest.toml")
	if err != nil {
		panic(err)
	}
	ctx, cancelFunc = context.WithCancel(context.Background())
	go server.StartServer(ctx, conf)

	conn, err := grpc.Dial(fmt.Sprintf("%s:%d", conf.ListenAddress, conf.Port), grpc.WithInsecure(), grpc.WithBlock())

	if err != nil {
		logrus.Error(err)
		os.Exit(1)
	}

	client = api.NewDataLifecycleClient(conn)

	e := m.Run()
	cancelFunc()
	os.Exit(e)
}

func TestTriggerPurge(t *testing.T) {
	// Trigger a purge. We expect it to succeed
	_, err := client.TriggerPurge(
		context.Background(),
		&api.TriggerPurgeRequest{
			ServiceName: "ingest-service",
		})

	require.NoError(t, err)
}
