package authz

import (
	"context"

	iam_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/lib/cereal/backend"
	grpccereal "github.com/chef/automate/lib/cereal/grpc"
	"google.golang.org/grpc"
)

const (
	maxNumberOfConsecutiveFails = 10
)

type EsClient interface {
	JobCancel(context.Context, string) error
	UpdateProjectTags(context.Context, map[string]*iam_v2.ProjectRules) ([]string, error)
	JobStatus(context.Context, string) (JobStatus, error)
}

// BackendFromConn takes a connection to cereal-service and returns a cereal backend
// with the correct domain for project updates
func ProjectUpdateBackend(conn *grpc.ClientConn) backend.Driver {
	return grpccereal.NewGrpcBackendFromConn("project-update", conn)
}
