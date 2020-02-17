package secureconn

import (
	"context"
	"path"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/codes"
	pb "google.golang.org/grpc/examples/helloworld/helloworld"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/chef/automate/lib/tls/certs"
)

type HelloServer struct{}

// SayHello implements helloworld.GreeterServer
func (s *HelloServer) SayHello(ctx context.Context, in *pb.HelloRequest) (*pb.HelloReply, error) {
	return &pb.HelloReply{Message: "Hello"}, nil
}

func TestSecureConnFactory(t *testing.T) {
	rootAConfig := certs.TLSConfig{
		CertPath:       pathFor("root-a-service.crt"),
		KeyPath:        pathFor("root-a-service.key"),
		RootCACertPath: pathFor("Test_Root_A.crt"),
	}

	rootACerts, err := rootAConfig.ReadCerts()
	require.NoError(t, err)

	// The server runs with a service signed by Test_Root_A
	connFactoryRootA := NewFactory(*rootACerts)

	rootBConfig := certs.TLSConfig{
		CertPath:       pathFor("root-b-service.crt"),
		KeyPath:        pathFor("root-b-service.key"),
		RootCACertPath: pathFor("Test_Root_B.crt"),
	}

	rootBCerts, err := rootBConfig.ReadCerts()
	require.NoError(t, err)

	connFactoryRootB := NewFactory(*rootBCerts)

	grpcServer := connFactoryRootA.NewServer()
	pb.RegisterGreeterServer(grpcServer, &HelloServer{})

	testServer := grpctest.NewServer(grpcServer)
	defer testServer.Close()

	t.Run("Test correctly signed client cert and server name", func(t *testing.T) {
		// Test that we can get a valid response if we have a certificate signed
		// by the same CA as the server and use the right server name
		conn, err := connFactoryRootA.Dial("root-a-service", testServer.Listener.Addr().String())
		defer conn.Close() // nolint: megacheck
		require.NoError(t, err)

		client := pb.NewGreeterClient(conn)
		resp, err := client.SayHello(context.Background(), &pb.HelloRequest{})
		require.NoError(t, err)
		assert.Equal(t, "Hello", resp.Message)
	})

	t.Run("Test correctly signed client cert and wrong server name", func(t *testing.T) {
		// Test that we can cannot talk to the server if we have a correctly signed cert but
		// incorrect server name
		conn, err := connFactoryRootA.Dial("root-foo-service", testServer.Listener.Addr().String())
		defer conn.Close() // nolint: megacheck
		require.NoError(t, err)

		client := pb.NewGreeterClient(conn)
		_, err = client.SayHello(context.Background(), &pb.HelloRequest{})
		grpctest.AssertCode(t, codes.Unavailable, err)

		s, ok := status.FromError(err)
		require.True(t, ok)
		assert.Regexp(t, "authentication handshake failed: x509.*valid for root-a-service, not root-foo-service", s.Message())
	})

	t.Run("Test incorrectly signed client cert and correct server name", func(t *testing.T) {
		// Test that we cannot talk to the service if we are not signed by the same Root CA
		conn, err := connFactoryRootB.Dial("root-a-service", testServer.Listener.Addr().String())
		defer conn.Close() // nolint: megacheck
		require.NoError(t, err)

		client := pb.NewGreeterClient(conn)
		_, err = client.SayHello(context.Background(), &pb.HelloRequest{})
		grpctest.AssertCode(t, codes.Unavailable, err)

		s, ok := status.FromError(err)
		require.True(t, ok)
		assert.Regexp(t, "authentication handshake failed: x509.*certificate signed by unknown authority", s.Message())
	})
}

func pathFor(name string) string {
	return path.Join("testdata", name)
}
