package managers

import (
	"context"
	"fmt"

	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/chef/automate/api/interservice/nodemanager/manager"
	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/chef/automate/components/nodemanager-service/managers/awsec2"
	"github.com/chef/automate/components/nodemanager-service/managers/azure"
	"github.com/chef/automate/components/nodemanager-service/managers/gcp"
	"github.com/chef/automate/components/nodemanager-service/mgrtypes"
	"github.com/chef/automate/components/nodemanager-service/pgdb"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
)

func GetAWSCreds(secret *secrets.Secret) awsec2.AwsCreds {
	var accessKeyID, secretKey, role, sessionToken, region string
	for _, item := range secret.Data {
		if item.Key == "AWS_ACCESS_KEY_ID" {
			accessKeyID = item.Value
		}
		if item.Key == "AWS_SECRET_ACCESS_KEY" {
			secretKey = item.Value
		}
		if item.Key == "ARN_ROLE" {
			role = item.Value
		}
		if item.Key == "AWS_SESSION_TOKEN" {
			sessionToken = item.Value
		}
		if item.Key == "AWS_REGION" {
			region = item.Value
		}
	}
	if len(role) == 0 {
		if len(accessKeyID) == 0 || len(secretKey) == 0 {
			logrus.Errorf("GetAwsCreds insufficient creds available; len(accessKeyID) %d len(secretKey) %d", len(accessKeyID), len(secretKey))
		}
	}
	if len(region) == 0 {
		region = awsec2.DefaultRegion
	}
	return awsec2.AwsCreds{
		AccessKeyId:     accessKeyID,
		SecretAccessKey: secretKey,
		ArnRole:         role,
		SessionToken:    sessionToken,
		Region:          region,
	}
}

// GetAzureCreds returns clientID, clientSecret, tenantID, subscriptionID
func GetAzureCreds(secret *secrets.Secret) (string, string, string, string) {
	clientID, clientSecret, tenantID, subscriptionID := "", "", "", ""
	if secret != nil {
		for _, item := range secret.Data {
			if item.Key == "AZURE_CLIENT_ID" {
				clientID = item.Value
			}
			if item.Key == "AZURE_CLIENT_SECRET" {
				clientSecret = item.Value
			}
			if item.Key == "AZURE_TENANT_ID" {
				tenantID = item.Value
			}
			if item.Key == "AZURE_SUBSCRIPTION_ID" {
				subscriptionID = item.Value
			}
		}
	}
	if len(clientID) == 0 || len(clientSecret) == 0 || len(tenantID) == 0 {
		logrus.Infof("GetAzureCreds attempting to use environment credentials")
	}

	return clientID, clientSecret, tenantID, subscriptionID
}

func GetAWSManagerFromCredential(ctx context.Context, credential string, db *pgdb.DB, secretsClient secrets.SecretsServiceClient) (myaws *awsec2.AwsCreds, err error) {
	var awsCreds awsec2.AwsCreds
	if len(credential) > 0 { // for users running in ec2, can add mgr with no credential
		secret, err := secretsClient.Read(ctx, &secrets.Id{Id: credential})
		if err != nil {
			return nil, errors.Wrapf(err, "Could not find secret with id %s", credential)
		}

		awsCreds = GetAWSCreds(secret)
	}
	myaws = awsec2.New(awsCreds)
	return myaws, nil
}

func GetAzureManagerFromCredential(ctx context.Context, credential string, db *pgdb.DB, secretsClient secrets.SecretsServiceClient) (myaws *azure.Creds, err error) {
	var clientID, clientSecret, tenantID, subscriptionID string
	if len(credential) > 0 {
		secret, err := secretsClient.Read(ctx, &secrets.Id{Id: credential})
		if err != nil {
			return nil, errors.Wrapf(err, "Could not find secret with id %s", credential)
		}

		clientID, clientSecret, tenantID, subscriptionID = GetAzureCreds(secret)
	}
	creds, err := azure.New(clientID, clientSecret, tenantID, subscriptionID)
	if err != nil {
		return nil, errors.Wrap(err, "GetAzureManagerFromCredential unable to initialize connection to azure api")
	}
	return &creds, nil
}

func GetGCPManagerFromCredential(ctx context.Context, credential string, db *pgdb.DB, secretsClient secrets.SecretsServiceClient) (mygcp *gcp.GcpCreds, err error) {
	secret, err := secretsClient.Read(ctx, &secrets.Id{Id: credential})
	if err != nil {
		return nil, errors.Wrapf(err, "Could not find secret with id %s", credential)
	}
	mygcp, err = gcp.New(secret)
	if err != nil {
		return nil, errors.Wrap(err, "Failed to create a new GCP instance")
	}
	return mygcp, nil
}

func GetAWSManagerFromID(ctx context.Context, id string, db *pgdb.DB, secretsClient secrets.SecretsServiceClient) (myaws *awsec2.AwsCreds, ssm bool, err error) {
	var awsCreds awsec2.AwsCreds
	awsSecret, err := db.GetCredentialFromNodeManager(ctx, id, secretsClient)
	if err != nil {
		return nil, ssm, errors.Wrapf(err, "Could not find secret with manager id %s", id)
	}
	if awsSecret != nil {
		// if the user is running in ec2, and has added the node manager with no credential reference,
		// then the secret is nil. if it's not nil, we should get the info
		awsCreds = GetAWSCreds(awsSecret)
	}
	if len(awsCreds.AccessKeyId) == 0 && len(awsCreds.SecretAccessKey) == 0 {
		ssm = true
	}

	myaws = awsec2.New(awsCreds)
	return myaws, ssm, nil
}

func GetAzureManagerFromID(ctx context.Context, id string, db *pgdb.DB, secretsClient secrets.SecretsServiceClient) (*azure.Creds, error) {
	azureSecret, err := db.GetCredentialFromNodeManager(ctx, id, secretsClient)
	if err != nil {
		return nil, errors.Wrapf(err, "Could not find secret with manager id %s", id)
	}

	clientID, clientSecret, tenantID, subscriptionID := GetAzureCreds(azureSecret)
	creds, err := azure.New(clientID, clientSecret, tenantID, subscriptionID)
	if err != nil {
		return nil, errors.Wrap(err, "GetAzureManagerFromID unable to initialize connection to azure api")
	}
	return &creds, nil
}

// GetStateInfoByManager takes manager id
// and returns an array of instance id and its state
func GetStateInfoByManager(ctx context.Context, mgrID string, db *pgdb.DB, mgrType string, secretsClient secrets.SecretsServiceClient) ([]pgdb.InstanceState, error) {
	instanceStates := make([]pgdb.InstanceState, 0)
	switch mgrType {
	case "aws-ec2":
		creds, _, err := GetAWSManagerFromID(ctx, mgrID, db, secretsClient)
		if err != nil {
			return instanceStates, errors.Wrapf(err, "GetStateInfoByManager unable to get manager from id %s", mgrID)
		}

		instanceStates, err = creds.QueryStatus(ctx, []string{})
		if err != nil {
			return instanceStates, errors.Wrap(err, "GetStateInfoByManager unable to query for status")
		}
	case "azure-vm":
		creds, err := GetAzureManagerFromID(ctx, mgrID, db, secretsClient)
		if err != nil {
			return instanceStates, errors.Wrapf(err, "GetStateInfoByManager unable to get manager from id %s", mgrID)
		}

		instanceStates, err = creds.QueryVMState(ctx)
		if err != nil {
			return instanceStates, errors.Wrap(err, "GetStateInfoByManager unable to query for status")
		}
	}
	return instanceStates, nil
}

func GetAccountID(ctx context.Context, credential string, db *pgdb.DB, mgrType string, secretsClient secrets.SecretsServiceClient) (string, error) {
	switch mgrType {
	case "aws", "aws-ec2", "aws-api":
		creds, err := GetAWSManagerFromCredential(ctx, credential, db, secretsClient)
		if err != nil {
			return "", errors.Wrapf(err, "Could not get aws manager from credential %s", credential)
		}
		return creds.GetAccountID(ctx)
	case "azure", "azure-vm", "azure-api":
		creds, err := GetAzureManagerFromCredential(ctx, credential, db, secretsClient)
		if err != nil {
			return "", errors.Wrapf(err, "Could not get azure manager from credential %s", credential)
		}
		// if the user has included tenant id credential, we just return that
		if len(creds.TenantID) > 0 {
			return creds.TenantID, nil
		}
		ids, err := creds.GetTenantIds(ctx)
		if err != nil {
			return "", errors.Wrapf(err, "Could not get tenant ids for manager %s", credential)
		}
		// TODO (@vj): should return all tenant ids here; handle accordingly (multi-tenant accounts)
		if len(ids) > 0 {
			return ids[0], nil
		}
	case "gcp", "gcp-api":
		creds, err := GetGCPManagerFromCredential(ctx, credential, db, secretsClient)
		if err != nil {
			return "", errors.Wrapf(err, "Could not get gcp manager from credential %s", credential)
		}
		return creds.GetProjectID(ctx), nil
	}
	return "", fmt.Errorf("unable to fetch tenant ids for account")
}

func CreateManualNodeManager(db *pgdb.DB) (string, error) {
	logrus.Infof("checking for existence of manual nodemanager")
	_, err := db.GetNodeManager(mgrtypes.AutomateManagerID)
	if err == nil {
		return mgrtypes.AutomateManagerID, nil
	}
	logrus.Infof("no manual node manager found; creating one now with id %s", mgrtypes.AutomateManagerID)
	manager := manager.NodeManager{
		Id:   mgrtypes.AutomateManagerID,
		Name: "Automate",
		Type: "automate",
	}
	_, err = db.AddNodeManager(&manager, "")
	if err != nil {
		return "", errors.Wrap(err, "unable to create manual node manager")
	}
	// get all nodes with manager empty string (for legacy) or automate
	var perPage int32 = 100
	var total int64 = 100
	nodeIDs := make([]string, 0)
	filters := []*common.Filter{{Key: "manager_type", Values: []string{"automate", ""}}}
	for cnt := int32(1); int64(len(nodeIDs)) < total; cnt++ {
		logrus.Debugf("getting nodes with page %d for total %d, per_page %d", cnt, total, perPage)
		pageNodes, totalCount, err := db.GetNodes("", 0, cnt, perPage, filters)
		if err != nil {
			logrus.Errorf("CreateManualNodeManager unable to get manual nodes: %s %s", filters, err.Error())
			return "", err
		}
		total = int64(totalCount.Total)
		for _, node := range pageNodes {
			nodeIDs = append(nodeIDs, node.Id)
		}
	}
	// associate manager id with all those nodes
	err = db.AssociateNodeIDsWithManagerID(nodeIDs, mgrtypes.AutomateManagerID)
	if err != nil {
		return "", errors.Wrap(err, "unable to associate manually managed nodes with Automate manager")
	}
	return mgrtypes.AutomateManagerID, nil
}

func SendRemoteExecutionJob(ctx context.Context, job *types.InspecJob, script string, scriptType string) error {
	switch job.TargetConfig.Backend {
	case inspec.BackendSSM, inspec.BackendSSMWindows:
		s := awsec2.NewSSM()
		return s.SendSSMJob(ctx, job, script, scriptType)
	case inspec.BackendAZ, inspec.BackendAZWindows:
		creds, err := azure.New(job.TargetConfig.AzureClientID, job.TargetConfig.AzureClientSecret, job.TargetConfig.AzureTenantID, job.TargetConfig.AzureSubscriptionID)
		if err != nil {
			return errors.Wrap(err, "SendRemoteExecutionJob unable to initialize connection to azure api")
		}
		return creds.SendRunCommandJob(ctx, job, script, scriptType)
	}
	return fmt.Errorf("unsupported backend attempted remote execution job. job backend: %s", job.TargetConfig.Backend)
}
