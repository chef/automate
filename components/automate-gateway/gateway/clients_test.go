package gateway

import (
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/grpc/secureconn"
)

func TestSocketPath(t *testing.T) {
	t.Run("with null endpoint socket", func(t *testing.T) {
		c := ClientConfig{NullBackendSock: "/tmp/backend.sock"}
		require.Equal(t, "unix:/tmp/backend.sock", c.socketPath())
	})

	t.Run("without null endpoint socket", func(t *testing.T) {
		c := ClientConfig{}
		require.Equal(t, "", c.socketPath())
	})
}

func TestExpandEndpoints(t *testing.T) {
	t.Run("with null endpoint socket", func(t *testing.T) {
		c := ClientConfig{NullBackendSock: "/tmp/backend.sock"}
		c.expandEndpoints()

		// Expands to all known gRPC services
		require.Equal(t, len(grpcServices), len(c.Endpoints))

		// All targets are the null socket and insecure
		for name, endpoint := range c.Endpoints {
			require.Equal(t, c.socketPath(), endpoint.Target, "%s target not expanded correctly", name)
			require.False(t, endpoint.Secure)
		}
	})

	t.Run("without null endpoint", func(t *testing.T) {
		c := ClientConfig{}
		c.expandEndpoints()

		// Expands to no endpoints
		require.Len(t, c.Endpoints, 0)
	})

	t.Run("with null endpoint and configured endpoint", func(t *testing.T) {
		grpcService := grpcServices[0]

		c := ClientConfig{
			NullBackendSock: "/tmp/backend.sock",
			Endpoints: map[string]ConnectionOptions{
				grpcService: ConnectionOptions{
					Target: "127.0.0.1:1111",
					Secure: true,
				},
			},
		}

		c.expandEndpoints()

		// Expands to all known gRPC services
		require.Equal(t, len(grpcServices), len(c.Endpoints))

		foundGrpcService := false
		for name, endpoint := range c.Endpoints {
			if name == grpcService {
				foundGrpcService = true
				// Make sure it didn't overwrite the target with the null backend
				require.Equal(t, "127.0.0.1:1111", endpoint.Target, "%s target not expanded correctly", name)
				require.True(t, endpoint.Secure)
				continue
			}

			// All unknown are set to the null socket and insecure
			require.Equal(t, c.socketPath(), endpoint.Target, "%s target not expanded correctly", name)
			require.False(t, endpoint.Secure)
		}

		require.True(t, foundGrpcService)
	})
}

func TestDialWithNoEndpoints(t *testing.T) {
	cfg := ClientConfig{}
	connFactory := &secureconn.Factory{}
	conns, err := cfg.DialEndpoints(connFactory)
	require.NoError(t, err)
	require.Equal(t, 0, len(conns))
}

// Assert that we can initialize a null backend and a clients factory and that
// the null backend returns no errors.
func TestDialWithNoEndpointsAndNullBackendSock(t *testing.T) {
	tmpDir, err := ioutil.TempDir("", "null_backend_test")
	require.NoError(t, err)

	sockName := "backend.sock"
	sockPath := filepath.Join(tmpDir, sockName)
	svrCfg := Config{
		GrpcClients: ClientConfig{
			NullBackendSock: sockPath,
		},
	}
	clientCfg := ClientConfig{
		NullBackendSock: sockPath,
	}
	connFactory := &secureconn.Factory{}

	svr := New(svrCfg)

	defer func() {
		svr.stopNullBackendServer()
		os.RemoveAll(tmpDir)
	}()

	err = svr.startNullBackendServer()
	require.NoError(t, err)

	f, err := NewClientsFactory(clientCfg, connFactory)
	require.NoError(t, err)

	// TODO: figure out a way to test the ClientFactory interface programatically
	_, err = f.ApplicationsClient()
	require.NoError(t, err)
	_, err = f.AuthenticationClient()
	require.NoError(t, err)
	_, err = f.AuthorizationClient()
	require.NoError(t, err)
	_, err = f.AuthorizationV2Client()
	require.NoError(t, err)
	_, err = f.CfgMgmtClient()
	require.NoError(t, err)
	_, err = f.ChefIngesterClient()
	require.NoError(t, err)
	_, err = f.ChefIngesterJobSchedulerClient()
	require.NoError(t, err)
	_, err = f.ComplianceIngesterClient()
	require.NoError(t, err)
	_, err = f.ComplianceJobsServiceClient()
	require.NoError(t, err)
	_, err = f.ComplianceProfilesServiceClient()
	require.NoError(t, err)
	_, err = f.ComplianceReportingServiceClient()
	require.NoError(t, err)
	_, err = f.ComplianceStatsServiceClient()
	require.NoError(t, err)
	_, err = f.ComplianceVersionServiceClient()
	require.NoError(t, err)
	_, err = f.DatafeedClient()
	require.NoError(t, err)
	_, err = f.DeploymentServiceClient()
	require.NoError(t, err)
	_, err = f.FeedClient()
	require.NoError(t, err)
	_, err = f.IngestStatusClient()
	require.NoError(t, err)
	_, err = f.LicenseControlClient()
	require.NoError(t, err)
	_, err = f.NodeManagerClient()
	require.NoError(t, err)
	_, err = f.NodesClient()
	require.NoError(t, err)
	_, err = f.NotificationsClient()
	require.NoError(t, err)
	_, err = f.Notifier()
	require.NoError(t, err)
	_, err = f.PoliciesClient()
	require.NoError(t, err)
	_, err = f.ProjectsClient()
	require.NoError(t, err)
	_, err = f.PurgeClient("ingest-service")
	require.NoError(t, err)
	_, err = f.SecretClient()
	require.NoError(t, err)
	_, err = f.TeamsClient()
	require.NoError(t, err)
	_, err = f.TokensMgmtClient()
	require.NoError(t, err)
	_, err = f.UsersMgmtClient()
	require.NoError(t, f.Close())
}
