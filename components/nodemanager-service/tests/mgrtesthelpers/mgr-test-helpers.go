package mgrtesthelpers

import (
	"context"
	"fmt"
	"os"
	"path"
	"strings"

	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	compliance_helpers "github.com/chef/automate/components/compliance-service/examples/helpers"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
	cert_helper "github.com/chef/automate/lib/tls/test/helpers"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"
)

const (
	SSMNodeName        = "vj-test-ssm-linux"
	ComplianceEndpoint = "127.0.0.1:10121"
	ManagerEndpoint    = "127.0.0.1:10120"
	SecretsEndpoint    = "127.0.0.1:10131"
)

var AWSRegionsList = []string{
	"ap-northeast-1",
	"ap-northeast-2",
	"ap-south-1",
	"ap-southeast-1",
	"ap-southeast-2",
	"ca-central-1",
	"eu-central-1",
	"eu-north-1",
	"eu-west-1",
	"eu-west-2",
	"eu-west-3",
	"sa-east-1",
	"us-east-1",
	"us-east-2",
	"us-west-1",
	"us-west-2",
}

func Contains(a []string, x string) bool {
	for _, n := range a {
		if strings.HasSuffix(x, n) {
			return true
		}
	}
	return false
}

func CheckForCreds(credsType string) bool {
	environmentVars := []string{"AWS_ACCESS_KEY_ID", "AWS_SECRET_ACCESS_KEY"}
	if credsType == "azure" {
		environmentVars = []string{"AZURE_CLIENT_ID", "AZURE_CLIENT_SECRET", "AZURE_TENANT_ID"}
	}
	for _, envVar := range environmentVars {
		if _, ok := os.LookupEnv(envVar); !ok {
			return false
		}
	}
	return true
}

func DeleteAllManagersByType(ctx context.Context, mgrClient manager.NodeManagerServiceClient, mgrType string) error {
	mgrsList, err := mgrClient.List(ctx, &manager.Query{})
	if err != nil {
		return err
	}
	for _, mgr := range mgrsList.GetManagers() {
		if mgr.Type == mgrType || mgrType == "" {
			mgrClient.DeleteWithNodes(ctx, &manager.Id{Id: mgr.Id}) // nolint:errcheck
		}
	}
	return nil
}

func DeleteAllNodes(ctx context.Context, nodesClient nodes.NodesServiceClient) error {
	nodesList, err := nodesClient.List(ctx, &nodes.Query{})
	if err != nil {
		return err
	}
	for _, node := range nodesList.GetNodes() {
		nodesClient.Delete(ctx, &nodes.Id{Id: node.Id}) // nolint:errcheck
	}
	return nil
}

func AddAWSManager(ctx context.Context, mgrClient manager.NodeManagerServiceClient, mgrType string) (*manager.Ids, error) {
	awsMgr := manager.NodeManager{
		Name: fmt.Sprintf("my test %s mgr", mgrType),
		Type: mgrType,
		CredentialData: []*common.Kv{
			{Key: "AWS_ACCESS_KEY_ID", Value: os.Getenv("AWS_ACCESS_KEY_ID")},
			{Key: "AWS_SECRET_ACCESS_KEY", Value: os.Getenv("AWS_SECRET_ACCESS_KEY")},
			{Key: "AWS_SESSION_TOKEN", Value: os.Getenv("AWS_SESSION_TOKEN")},
		},
	}

	return mgrClient.Create(ctx, &awsMgr)
}

func AddAzureManager(ctx context.Context, mgrClient manager.NodeManagerServiceClient, mgrType string) (*manager.Ids, error) {
	azureMgr := manager.NodeManager{
		Name: fmt.Sprintf("my test %s mgr", mgrType),
		Type: mgrType,
		CredentialData: []*common.Kv{
			{Key: "AZURE_CLIENT_ID", Value: os.Getenv("AZURE_CLIENT_ID")},
			{Key: "AZURE_CLIENT_SECRET", Value: os.Getenv("AZURE_CLIENT_SECRET")},
			{Key: "AZURE_TENANT_ID", Value: os.Getenv("AZURE_TENANT_ID")},
		},
	}

	return mgrClient.Create(ctx, &azureMgr)
}

func GetManagerConn() (*grpc.ClientConn, error) {
	var connFactory *secureconn.Factory
	if os.Getenv("RUN_MODE") == "local" {
		connFactory = SecureConnFactory()
	} else {
		connFactory = SecureConnFactoryHab()
	}

	mgrConn, err := connFactory.Dial("nodemanager-service", ManagerEndpoint)
	if err != nil {
		return nil, err
	}
	return mgrConn, nil
}

func GetSecretsConn() (*grpc.ClientConn, error) {
	var connFactory *secureconn.Factory
	if os.Getenv("RUN_MODE") == "local" {
		connFactory = SecureConnFactory()
	} else {
		connFactory = SecureConnFactoryHab()
	}

	secretsConn, err := connFactory.Dial("secrets-service", SecretsEndpoint)
	if err != nil {
		return nil, err
	}
	return secretsConn, nil
}

func GetComplianceConn() (*grpc.ClientConn, error) {
	var connFactory *secureconn.Factory
	if os.Getenv("RUN_MODE") == "local" {
		connFactory = compliance_helpers.SecureConnFactory()
	} else {
		connFactory = compliance_helpers.SecureConnFactoryHab()
	}

	cmpConn, err := connFactory.Dial("compliance-service", ComplianceEndpoint)
	if err != nil {
		return nil, err
	}
	return cmpConn, nil
}

// LoadCerts loads the dev certs for nodemanager service
func LoadNodeManagerCerts() *certs.ServiceCerts {
	name := "nodemanager-service"
	cfg := certs.TLSConfig{
		CertPath:       cert_helper.DevCertPath(name),
		KeyPath:        cert_helper.DevKeyPath(name),
		RootCACertPath: cert_helper.DevRootCACert(),
	}

	serviceCerts, err := cfg.ReadCerts()
	if err != nil {
		logrus.WithError(err).Fatal("Could not load certs")
	}

	return serviceCerts
}

// uses the certs in a running hab env
func LoadNodeManagerCertsHab() *certs.ServiceCerts {
	dirname := "/hab/svc/nodemanager-service/config"
	logrus.Infof("certs dir is %s", dirname)

	cfg := certs.TLSConfig{
		CertPath:       path.Join(dirname, "service.crt"),
		KeyPath:        path.Join(dirname, "service.key"),
		RootCACertPath: path.Join(dirname, "root_ca.crt"),
	}

	serviceCerts, err := cfg.ReadCerts()
	if err != nil {
		logrus.WithError(err).Fatal("Could not load certs:")
	}

	return serviceCerts
}

func SecureConnFactory() *secureconn.Factory {
	serviceCerts := LoadNodeManagerCerts()
	return secureconn.NewFactory(*serviceCerts)
}

func SecureConnFactoryHab() *secureconn.Factory {
	certs := LoadNodeManagerCertsHab()
	return secureconn.NewFactory(*certs)
}
