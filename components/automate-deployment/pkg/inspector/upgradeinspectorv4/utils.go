package upgradeinspectorv4

import (
	"bytes"
	"fmt"
	"io"
	"io/ioutil"
	"net"
	"net/http"

	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/config/shared"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/pkg/errors"
	"google.golang.org/protobuf/types/known/wrapperspb"
)

type UpgradeV4Utils interface {
	IsExternalElasticSearch() bool
	ExecRequest(url, methodType string, requestBody io.Reader) ([]byte, error)
	GetESBasePath(timeout int64) string
	GetBackupS3URL() (string, error)
	PatchS3backupURL(*cli.Writer) (stdOut, stdErr string, err error)
}

type UpgradeV4UtilsImp struct{}

func NewUpgradeV4Utils() UpgradeV4Utils {
	return &UpgradeV4UtilsImp{}
}

func (cu *UpgradeV4UtilsImp) GetBackupS3URL() (string, error) {
	res, err := client.GetAutomateConfig(int64(client.DefaultClientTimeout))
	if err != nil {
		return "", err
	}
	return res.Config.GetGlobal().GetV1().GetBackups().GetS3().GetBucket().GetEndpoint().GetValue(), nil
}

type DummyWriter struct {
	WriteBuffer *bytes.Buffer
	ReadBuffer  *bytes.Buffer
	ErrorBuffer *bytes.Buffer
	CliWriter   *cli.Writer
}

func NewWriter() *DummyWriter {
	tw := &DummyWriter{
		WriteBuffer: new(bytes.Buffer),
		ReadBuffer:  new(bytes.Buffer),
		ErrorBuffer: new(bytes.Buffer),
	}
	tw.CliWriter = cli.NewWriter(tw.WriteBuffer, tw.ErrorBuffer, tw.ReadBuffer)
	return tw
}

func (cu *UpgradeV4UtilsImp) PatchS3backupURL(writer *cli.Writer) (stdOut, stdErr string, err error) {
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
	tw := NewWriter()
	err = client.PatchAutomateConfig(10, cfg, tw.CliWriter)
	if err != nil {
		return "", "", err
	}
	return tw.WriteBuffer.String(), tw.ErrorBuffer.String(), nil
}

func (cu *UpgradeV4UtilsImp) IsExternalElasticSearch() bool {
	res, err := client.GetAutomateConfig(int64(client.DefaultClientTimeout))
	if err != nil {
		return false
	}
	return res.Config.GetGlobal().GetV1().GetExternal().GetElasticsearch().GetEnable().GetValue()
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
