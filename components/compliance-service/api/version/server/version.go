package version

import (
	pb "github.com/golang/protobuf/ptypes/empty"
	"golang.org/x/net/context"

	ver_api "github.com/chef/automate/components/compliance-service/api/version"
	"github.com/chef/automate/components/compliance-service/config"
	"github.com/chef/automate/lib/version"
)

// VersionServer
type Server struct{}

// New creates a new instance with config
func New() *Server {
	return &Server{}
}

// Version gets the version for the service from the config and conf information
func (srv *Server) Version(ctx context.Context, empty *pb.Empty) (*ver_api.VersionInfo, error) {
	version := ver_api.VersionInfo{
		Api:     config.SERVICE_NAME, // Legacy key
		Name:    config.SERVICE_NAME,
		Version: version.Version,
		Sha:     version.GitSHA,
		Built:   version.BuildTime,
	}
	return &version, nil
}
