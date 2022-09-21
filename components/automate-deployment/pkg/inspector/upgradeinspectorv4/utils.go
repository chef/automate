package upgradeinspectorv4

import (
	"fmt"
	"io"
	"io/ioutil"
	"net"
	"net/http"

	"github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/client"
	"github.com/pkg/errors"
)

type UpgradeV4Utils interface {
	IsExternalElasticSearch() bool
	ExecRequest(url, methodType string, requestBody io.Reader) ([]byte, error)
	GetESBasePath(timeout int64) string
}

type UpgradeV4UtilsImp struct{}

func NewUpgradeV4Utils() UpgradeV4Utils {
	return &UpgradeV4UtilsImp{}
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
