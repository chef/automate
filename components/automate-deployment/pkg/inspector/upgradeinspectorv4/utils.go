package upgradeinspectorv4

import (
	"context"
	"fmt"
	"io"
	"io/ioutil"
	"net"
	"net/http"

	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/load_balancer"
	"github.com/chef/automate/api/config/shared"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/pkg/errors"
	"google.golang.org/protobuf/types/known/wrapperspb"
)

type UpgradeV4Utils interface {
	IsExternalElasticSearch(timeout int64) bool
	ExecRequest(url, methodType string, requestBody io.Reader) ([]byte, error)
	GetESBasePath(timeout int64) string
	GetBackupS3URL(timeout int64) (string, error)
	PatchS3backupURL(timeout int64) (stdOut, stdErr string, err error)
	GetMaintenanceStatus(timeout int64) (bool, error)
	SetMaintenanceMode(timeout int64, status bool) (stdOut, stdErr string, err error)
	WriteToFile(filepath string, data []byte) error
	GetServicesStatus() (bool, error)
}

type UpgradeV4UtilsImp struct{}

func NewUpgradeV4Utils() UpgradeV4Utils {
	return &UpgradeV4UtilsImp{}
}

func (cu *UpgradeV4UtilsImp) GetBackupS3URL(timeout int64) (string, error) {
	res, err := client.GetAutomateConfig(timeout)
	if err != nil {
		return "", err
	}
	return res.Config.GetGlobal().GetV1().GetBackups().GetS3().GetBucket().GetEndpoint().GetValue(), nil
}

func (cu *UpgradeV4UtilsImp) GetMaintenanceStatus(timeout int64) (bool, error) {
	config, err := client.GetAutomateConfig(timeout)
	if err != nil {
		return false, err
	}
	return config.GetConfig().GetLoadBalancer().GetV1().GetSys().GetService().GetMaintenanceMode().GetValue(), nil
}

func (cu *UpgradeV4UtilsImp) PatchS3backupURL(timeout int64) (stdOut, stdErr string, err error) {
	cfg := deployment.NewUserOverrideConfig()
	cfg.Global = &shared.GlobalConfig{
		V1: &shared.V1{
			Backups: &shared.Backups{
				Location: wrapperspb.String("s3"),
				S3: &shared.Backups_S3{
					Bucket: &shared.Backups_S3_Bucket{
						Endpoint: wrapperspb.String(NEW_S3_URL),
					},
				},
			},
		},
	}
	tw := NewTestWriter()
	err = client.PatchAutomateConfig(10, cfg, tw.CliWriter)
	if err != nil {
		return "", "", err
	}
	return tw.WriteBuffer.String(), tw.ErrorBuffer.String(), nil
}

func (cu *UpgradeV4UtilsImp) SetMaintenanceMode(timeout int64, status bool) (stdOut, stdErr string, err error) {
	enable := wrapperspb.Bool(status)
	cfg := deployment.NewUserOverrideConfig()
	cfg.LoadBalancer = &load_balancer.ConfigRequest{
		V1: &load_balancer.ConfigRequest_V1{
			Sys: &load_balancer.ConfigRequest_V1_System{
				Service: &load_balancer.ConfigRequest_V1_System_Service{
					MaintenanceMode: enable,
				},
			},
		},
	}
	tw := NewTestWriter()
	if err := client.PatchAutomateConfig(10, cfg, tw.CliWriter); err != nil {
		return "", "", err
	}
	return tw.WriteBuffer.String(), tw.ErrorBuffer.String(), nil
}

func (cu *UpgradeV4UtilsImp) IsExternalElasticSearch(timeout int64) bool {
	return majorupgrade_utils.IsExternalElasticSearch(timeout)
}

func (cu *UpgradeV4UtilsImp) GetESBasePath(timeout int64) string {
	var basePath = "http://localhost:10144/"
	cfg := deployment.DefaultAutomateConfig()
	defaultHost := cfg.GetEsgateway().GetV1().GetSys().GetService().GetHost().GetValue()
	defaultPort := cfg.GetEsgateway().GetV1().GetSys().GetService().GetPort().GetValue()

	if defaultHost != "" || defaultPort > 0 {
		basePath = fmt.Sprintf(`http://%s/`, net.JoinHostPort(defaultHost, fmt.Sprintf("%d", defaultPort)))
	}

	res, err := client.GetAutomateConfig(timeout)
	if err == nil {
		host := res.Config.GetEsgateway().GetV1().GetSys().GetService().GetHost().GetValue()
		port := res.Config.GetEsgateway().GetV1().GetSys().GetService().GetPort().GetValue()
		if host != "" || port > 0 {
			url := net.JoinHostPort(host, fmt.Sprintf("%d", port))
			if url != "" {
				basePath = fmt.Sprintf(`http://%s/`, url)
			}
		}
	}
	return basePath
}

func (cu *UpgradeV4UtilsImp) ExecRequest(url, methodType string, requestBody io.Reader) ([]byte, error) {
	method := methodType

	client := &http.Client{}
	req, err := http.NewRequest(method, url, requestBody) // nosemgrep

	if err != nil {
		return nil, err
	}
	req.Header.Add("Content-Type", "application/json")

	res, err := client.Do(req)
	if err != nil {
		fmt.Println(err)
		return nil, err
	}
	defer res.Body.Close()
	body, err := ioutil.ReadAll(res.Body) // nosemgrep
	if err != nil {
		return nil, err
	}

	if res.StatusCode != http.StatusOK {
		return nil, errors.Errorf("Request failed with status %d\n%s\n", res.StatusCode, string(body))
	}
	return body, nil
}

func (cu *UpgradeV4UtilsImp) WriteToFile(filepath string, data []byte) error {
	return ioutil.WriteFile(filepath, data, 775)
}

func (cu *UpgradeV4UtilsImp) GetServicesStatus() (bool, error) {
	connection, err := client.Connection(client.DefaultClientTimeout)
	if err != nil {
		return false, err
	}

	res, err := connection.Status(context.Background(), &api.StatusRequest{})
	if err != nil {
		return false, errors.Wrap(
			err,
			"Request to obtain Chef Automate status information failed",
		)
	}

	return res.ServiceStatus.AllHealthy(), nil
}
