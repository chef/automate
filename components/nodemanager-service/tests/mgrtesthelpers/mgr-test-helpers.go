package mgrtesthelpers

import (
	"context"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"os"
	"strings"

	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/compliance-service/examples/helpers"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/lib/grpc/secureconn"
	"google.golang.org/grpc"
)

const (
	SSMNodeName        = "vj-test-ssm-linux"
	ComplianceEndpoint = "127.0.0.1:10121"
	ManagerEndpoint    = "127.0.0.1:10120"
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
	if credsType == "gcp" {
		environmentVars = []string{"GOOGLE_CREDS_JSON"}
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

func AddGCPManager(ctx context.Context, mgrClient manager.NodeManagerServiceClient, mgrType string) (*manager.Ids, error) {
	// decode our encoded key
	fmt.Println("length of credentials: ", len(os.Getenv("GOOGLE_CREDS_JSON")))
	decoded, err := base64.StdEncoding.DecodeString(os.Getenv("GOOGLE_CREDS_JSON"))
	if err != nil {
		return nil, err
	}
	fmt.Println("length of decoded credentials: ", len(decoded))
	stringDecoded := string(decoded)
	first5 := stringDecoded[0:5]
	fmt.Println("first 5 characters: ", first5)
	last25 := stringDecoded[len(stringDecoded)-25:]
	fmt.Println("last 25 characters: ", last25)

	formatted := strings.ReplaceAll(string(decoded), "\\n", "")
	newLineForKeyBeg := strings.ReplaceAll(formatted, "KEY-----", "KEY-----\\n")
	newLineForKeyEnd := strings.ReplaceAll(newLineForKeyBeg, "-----END", "\\n-----END")

	first5 = newLineForKeyEnd[0:5]
	fmt.Println("first 5 characters: ", first5)
	last25 = newLineForKeyEnd[len(newLineForKeyEnd)-25:]
	fmt.Println("last 25 characters: ", last25)

	fmt.Println("LENGTH OF CRED: ", len(newLineForKeyEnd))
	var gcpCred *secrets.GcpCredential
	err = json.Unmarshal([]byte(newLineForKeyEnd), &gcpCred)
	if err != nil {
		fmt.Print("UNMARSHAL ERROR: ", err)
	}
	gcpMgr := manager.NodeManager{
		Name: fmt.Sprintf("my test %s mgr", mgrType),
		Type: mgrType,
		CredentialData: []*common.Kv{
			{Key: "GOOGLE_CREDENTIALS_JSON", Value: newLineForKeyEnd},
		},
	}

	return mgrClient.Create(ctx, &gcpMgr)
}

func GetManagerConn() (*grpc.ClientConn, error) {
	var connFactory *secureconn.Factory
	if os.Getenv("RUN_MODE") == "local" {
		connFactory = helpers.SecureConnFactory()
	} else {
		connFactory = helpers.SecureConnFactoryHab()
	}

	mgrConn, err := connFactory.Dial("nodemanager-service", ManagerEndpoint)
	if err != nil {
		return nil, err
	}
	return mgrConn, nil
}

func GetComplianceConn() (*grpc.ClientConn, error) {
	var connFactory *secureconn.Factory
	if os.Getenv("RUN_MODE") == "local" {
		connFactory = helpers.SecureConnFactory()
	} else {
		connFactory = helpers.SecureConnFactoryHab()
	}

	cmpConn, err := connFactory.Dial("compliance-service", ComplianceEndpoint)
	if err != nil {
		return nil, err
	}
	return cmpConn, nil
}
