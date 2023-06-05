package infrastructure

import (
	"context"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/gofrs/uuid"
	"google.golang.org/grpc"
)

type DSClient interface {
	InfrastructureNodeDelete(ctx context.Context, in *api.InfrastructureNodeDeleteRequest, opts ...grpc.CallOption) (*api.InfrastructureNodeDeleteResponse, error)
	Close() error
}

type InfraFlow struct {
	DsClient DSClient
	Writer   *cli.Writer
}

func NewDeleteNode(writer *cli.Writer) (*InfraFlow, error) {
	connection, err := client.Connection(client.DefaultClientTimeout)

	if err != nil {
		return nil, err
	}
	return &InfraFlow{DsClient: connection, Writer: writer}, nil
}

func (ifw *InfraFlow) RunDeleteNode(nodeID string) error {

	defer ifw.DsClient.Close()

	if !isValidUUID(nodeID) {
		return status.New(status.InvalidCommandArgsError, "argument in not a valid node UUID")
	}
	deleteReq := &api.InfrastructureNodeDeleteRequest{NodeId: nodeID}

	_, err := ifw.DsClient.InfrastructureNodeDelete(context.Background(), deleteReq)
	if err != nil {
		return status.Wrap(err, status.DeploymentServiceCallError, "Request to delete node failed")
	}

	ifw.Writer.Println("Node successfully deleted")
	return nil
}

func isValidUUID(id string) bool {
	_, err := uuid.FromString(id)
	return err == nil
}
