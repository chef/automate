package debugclient

import (
	"context"
	"fmt"
	"io"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	"google.golang.org/grpc"

	"github.com/chef/automate/components/automate-cli/pkg/client"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/habapi"
	"github.com/chef/automate/lib/grpc/debug/debug_api"
	"github.com/chef/automate/lib/grpc/secureconn"
)

const defaultConnectionTimeout = 5 * time.Second

type ChunkReceiver interface {
	Recv() (*debug_api.Chunk, error)
}

type ConnectionInfo struct {
	ServiceName       string
	Endpoint          string
	ConnectionTimeout time.Duration
}

type ProfileRequest struct {
	ConnectionInfo
	ProfileName string
	SampleRate  int
	Duration    time.Duration
}

type TraceRequest struct {
	ConnectionInfo
	Duration time.Duration
}

func Profile(ctx context.Context, req ProfileRequest, out io.Writer) error {
	debugClient, cleanup, err := newClient(ctx, req.ConnectionInfo)
	if err != nil {
		return status.Annotate(err, status.DeploymentServiceUnreachableError)
	}
	defer func() {
		_ = cleanup
	}()

	profileReq := &debug_api.ProfileRequest{
		ProfileName: req.ProfileName,
		SampleRate:  int64(req.SampleRate),
	}
	if req.Duration > 0 {
		profileReq.Duration = ptypes.DurationProto(req.Duration)
	}

	resp, err := debugClient.Profile(ctx, profileReq)
	if err != nil {
		return status.Wrap(err, status.DeploymentServiceCallError, "Failed to run profile")
	}

	return streamChunksToWriter(resp, out)
}

func Trace(ctx context.Context, req TraceRequest, out io.Writer) error {
	debugClient, cleanup, err := newClient(ctx, req.ConnectionInfo)
	if err != nil {
		return status.Annotate(err, status.DeploymentServiceUnreachableError)
	}
	defer func() {
		_ = cleanup
	}()

	traceReq := &debug_api.TraceRequest{}
	if req.Duration > 0 {
		traceReq.Duration = ptypes.DurationProto(req.Duration)
	}
	resp, err := debugClient.Trace(ctx, traceReq)
	if err != nil {
		return status.Wrap(err, status.DeploymentServiceCallError, "Failed to run trace")
	}

	return streamChunksToWriter(resp, out)
}

func SetLogLevel(ctx context.Context, connectionInfo ConnectionInfo, level string) error {
	req := debug_api.SetLogLevelRequest{}
	switch level {
	case "debug":
		req.Level = debug_api.SetLogLevelRequest_DEBUG
	case "info":
		req.Level = debug_api.SetLogLevelRequest_INFO
	case "warn", "warning":
		req.Level = debug_api.SetLogLevelRequest_WARN
	case "fatal":
		req.Level = debug_api.SetLogLevelRequest_FATAL
	default:
		return status.New(status.InvalidCommandArgsError, "log level invalid")
	}

	debugClient, cleanup, err := newClient(ctx, connectionInfo)
	if err != nil {
		return status.Annotate(err, status.DeploymentServiceUnreachableError)
	}
	defer func() {
		_ = cleanup
	}()

	_, err = debugClient.SetLogLevel(ctx, &req)
	if err != nil {
		return status.Wrap(err, status.DeploymentServiceCallError, "failed to set log level")
	}
	return nil
}

func GetVersion(ctx context.Context, connectionInfo ConnectionInfo) (*debug_api.VersionResponse, error) {
	req := debug_api.VersionRequest{}
	debugClient, cleanup, err := newClient(ctx, connectionInfo)
	if err != nil {
		return nil, status.Annotate(err, status.DeploymentServiceUnreachableError)
	}
	defer func() {
		_ = cleanup
	}()

	return debugClient.GetVersion(ctx, &req)
}

type cleanupFunc func() error

func newClient(ctx context.Context, connectionInfo ConnectionInfo) (debug_api.DebugClient, cleanupFunc, error) {
	certData, err := client.LoadLocalCerts()
	if err != nil {
		return nil, nil, errors.Wrap(err, "could not read deployment-service certificates (deployment-service may still be starting up)")
	}
	connFactory := secureconn.NewFactory(*certData)
	connectionTimeout := connectionInfo.ConnectionTimeout
	if connectionTimeout == 0 {
		connectionTimeout = defaultConnectionTimeout
	}

	endpoint := connectionInfo.Endpoint
	if endpoint == "" {
		port, found, err := getGrpcPortForService(ctx, connectionInfo.ServiceName)
		if err != nil {
			return nil, nil, err
		}
		if !found {
			return nil, nil, errors.Errorf("Could not find endpoint for '%s'", connectionInfo.ServiceName)
		}
		endpoint = fmt.Sprintf("localhost:%d", port)
	}

	connectCtx, cancel := context.WithTimeout(ctx, connectionTimeout)
	defer cancel()
	connection, err := connFactory.DialContext(
		connectCtx,
		connectionInfo.ServiceName,
		endpoint,
		grpc.WithBlock())
	if err != nil {
		return nil, nil, errors.Wrapf(err, "could not connect to server at address %s", connectionInfo.Endpoint)
	}

	return debug_api.NewDebugClient(connection), connection.Close, nil
}

func getGrpcPortForService(ctx context.Context, serviceName string) (int32, bool, error) {
	c := habapi.New("http://localhost:9631")
	svcs, err := c.ListServices(ctx)
	if err != nil {
		return 0, false, status.Wrap(err, status.HabAPIError, "services query failed")
	}
	port, found := habapi.PortForService(svcs, serviceName)
	return port, found, nil
}

func streamChunksToWriter(recv ChunkReceiver, w io.Writer) error {
	for {
		chunk, err := recv.Recv()
		if err != nil {
			if err == io.EOF {
				return nil
			}
			return errors.Wrap(err, "failed to get chunk")
		}
		if _, err := w.Write(chunk.Chunk); err != nil {
			return errors.Wrap(err, "failed to write chunk")
		}
	}
}
