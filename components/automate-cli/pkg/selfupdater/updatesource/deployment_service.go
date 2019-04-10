package updatesource

import (
	"bytes"
	"context"
	"io"
	"runtime"

	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-cli/pkg/status"
)

type deploymentService struct {
	cliUpdaterClient api.DeploymentClient
}

// DeploymentService returns an UpdateSource that knows how to fetch the desired version
// and executable binary from the deployment-service
func DeploymentService(cliUpdaterClient api.DeploymentClient) UpdateSource {
	return &deploymentService{
		cliUpdaterClient: cliUpdaterClient,
	}
}

func (ds *deploymentService) DesiredVersion(ctx context.Context) (string, error) {
	resp, err := ds.cliUpdaterClient.ManifestVersion(ctx, &api.ManifestVersionRequest{})
	if err != nil {
		return "", status.Wrap(err, status.DeploymentServiceCallError, "Failed to determine latest chef-automate CLI version")
	}

	return resp.CliRelease, nil
}

func (ds *deploymentService) FetchLatest(ctx context.Context) ([]byte, string, error) {
	stream, err := ds.cliUpdaterClient.GetCLIExecutable(ctx,
		&api.GetCLIExecutableRequest{
			Platform: runtime.GOOS,
		})

	if err != nil {
		return nil, "", err
	}

	v := ""
	b := bytes.NewBuffer(make([]byte, 0, 20*(1<<20)))

	for {
		resp, err := stream.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			return nil, "", status.Wrap(err, status.DeploymentServiceCallError, "Failed to fetch latest executable")
		}
		b.Write(resp.Data)
		if v == "" {
			v = resp.Version
		}
	}

	return b.Bytes(), v, nil
}
