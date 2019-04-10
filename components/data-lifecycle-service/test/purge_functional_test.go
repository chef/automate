package test

import (
	"context"
	"fmt"
	"os"
	"testing"

	"github.com/sirupsen/logrus"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"

	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/components/data-lifecycle-service/server"
	"github.com/chef/automate/lib/grpc/secureconn"
)

var conf server.Config
var cancelFunc context.CancelFunc
var ctx context.Context
var conn *grpc.ClientConn
var client data_lifecycle.DataLifecycleClient

func TestMain(m *testing.M) {
	conf, err := server.ConfigFromToml("./testdata/config.functional.toml")
	if err != nil {
		logrus.Error(err)
		os.Exit(1)
	}

	ctx, cancelFunc = context.WithCancel(context.Background())
	go func() {
		errServer := server.StartServer(ctx, conf)
		if errServer != nil {
			logrus.WithError(errServer).Fatal("server failed")
		}
	}()

	serviceCerts, err := conf.ReadCerts()

	if err != nil {
		logrus.WithError(err).Fatal("failed to read certs")
	}

	connFactory := secureconn.NewFactory(*serviceCerts)

	conn, err := connFactory.Dial("data-lifecycle-service", fmt.Sprintf("%s:%d", conf.ListenAddress, conf.Port), grpc.WithBlock())

	if err != nil {
		logrus.Error(err)
		os.Exit(1)
	}

	client = data_lifecycle.NewDataLifecycleClient(conn)

	e := m.Run()
	cancelFunc()
	os.Exit(e)
}

func TestTriggerPurge(t *testing.T) {
	// Trigger a purge. We expect it to succeed
	resp, err := client.TriggerPurge(
		context.Background(),
		&data_lifecycle.TriggerPurgeRequest{
			ServiceName: "data-lifecycle-service",
		})

	require.NoError(t, err)

	assert.Equal(t, 1, len(resp.Responses))
	assert.Equal(t, data_lifecycle.PurgeStatus_SUCCESS, resp.Responses["data-lifecycle-service"].ComponentStatus["history"].Status)
}
