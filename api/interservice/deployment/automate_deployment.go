package deployment

import (
	"context"
	fmt "fmt"
	"io"
	"strings"

	"github.com/pkg/errors"
	grpc "google.golang.org/grpc"
)

// DeployClientStreamer is a DeploymentClient with some extra methods added
type DeployClientStreamer interface {
	DeploymentClient
	StreamDeployEvents(taskID string, deployID *DeploymentID, e DeployEventHandler) error
	Close() error
}

type deployClientStreamer struct {
	DeploymentClient
	grpcConnection *grpc.ClientConn
}

// NewDeployClientStreamer creates a DeployClientStreamer for the given
// connection
func NewDeployClientStreamer(cc *grpc.ClientConn) DeployClientStreamer {
	dc := NewDeploymentClient(cc)
	return &deployClientStreamer{DeploymentClient: dc, grpcConnection: cc}
}

// A DeployEventHandler accepts the events streamed from the deployment
// service's DeployStatus API.
type DeployEventHandler interface {
	HandleEvent(e *DeployEvent)
}

// StreamDeployEvents connects to the deployment service DeployStatus API and
// processes the events in the stream, writing them to the writer's output
func (c *deployClientStreamer) StreamDeployEvents(taskID string, deployID *DeploymentID, eh DeployEventHandler) error {
	stream, err := c.DeployStatus(context.Background(), &DeployStatusRequest{
		DeploymentId: deployID,
		TaskId:       taskID,
	})
	if err != nil {
		return errors.Wrap(err, "DeployStatus client call failed")
	}
	for {
		event, err := stream.Recv()
		if err == io.EOF {
			return nil
		}
		if err != nil {
			return err
		}

		eh.HandleEvent(event)

		if isCompleteFail(event) {
			return errors.New("deploy failed")
		}
	}
}

func (c *deployClientStreamer) Close() error {
	return c.grpcConnection.Close()
}

func isCompleteFail(event *DeployEvent) bool {
	if de := event.GetDeploy(); de != nil {
		switch de.Status {
		case DeployEvent_COMPLETE_FAIL:
			return true
		}
	}
	return false
}

// Format returns the BackupStatusResponse as pretty string
func (c *BackupStatusResponse) Format() string {
	msg := ""

	switch c.OpType {
	case BackupStatusResponse_CREATE:
		msg = fmt.Sprintf("Creating backup %s", c.GetTaskIds()[0])
	case BackupStatusResponse_DELETE:
		if len(c.GetTaskIds()) == 1 {
			msg = fmt.Sprintf("Deleting backup %s", c.GetTaskIds()[0])
		} else {
			msg = fmt.Sprintf("Deleting backups %s", strings.Join(c.GetTaskIds(), ", "))
		}
	case BackupStatusResponse_RESTORE:
		msg = fmt.Sprintf("Restoring backup %s", c.GetTaskIds()[0])
	case BackupStatusResponse_IDLE:
		msg = fmt.Sprintf("Idle")
	case BackupStatusResponse_VERIFY_INTEGRITY:
		if len(c.GetTaskIds()) == 1 {
			msg = fmt.Sprintf("Verifying backup %s", c.GetTaskIds()[0])
		} else {
			msg = fmt.Sprintf("Verifying backups %s", strings.Join(c.GetTaskIds(), ", "))
		}
	default:
		msg = "Unknown"
	}

	return msg
}
