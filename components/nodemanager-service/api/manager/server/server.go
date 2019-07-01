package manager

import (
	"fmt"
	"strings"

	pb "github.com/golang/protobuf/ptypes/empty"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"golang.org/x/net/context"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/components/compliance-service/api/common"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/chef/automate/components/nodemanager-service/api/manager"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	nodesserver "github.com/chef/automate/components/nodemanager-service/api/nodes/server"
	"github.com/chef/automate/components/nodemanager-service/managers"
	"github.com/chef/automate/components/nodemanager-service/mgrtypes"
	"github.com/chef/automate/components/nodemanager-service/pgdb"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/stringutils"
)

// Server implementation for manager
type Server struct {
	DB            *pgdb.DB
	secretsClient secrets.SecretsServiceClient
}

var empty = pb.Empty{}

// New creates a new server
func New(db *pgdb.DB, connectionFactory *secureconn.Factory, secretsEndpoint string) *Server {
	conf := &Server{DB: db}
	go conf.getSecretsConnection(connectionFactory, secretsEndpoint)
	return conf
}

func (srv *Server) getSecretsConnection(connectionFactory *secureconn.Factory, secretsEndpoint string) {
	if secretsEndpoint == "" {
		logrus.Errorf("secretsEndpoint cannot be empty or Dial will get stuck")
		return
	}

	conn, err := connectionFactory.Dial("secrets-service", secretsEndpoint, grpc.WithBlock())
	if err != nil {
		logrus.Errorf("getSecretsConnection, error grpc dialing to Secrets %s", err.Error())
		return
	}

	secretsClient := secrets.NewSecretsServiceClient(conn)
	if secretsClient == nil {
		logrus.Errorf("getSecretsConnection got nil for NewSecretsServiceClient")
		return
	}
	srv.secretsClient = secretsClient
}

func (srv *Server) getMgrAccountID(ctx context.Context, in *manager.NodeManager, db *pgdb.DB) (string, error) {
	acctID, err := managers.GetAccountID(ctx, in.CredentialId, db, in.Type, srv.secretsClient)
	if err != nil {
		return acctID, err
	}
	return acctID, nil
}

// Create a new node manager
func (srv *Server) Create(ctx context.Context, in *manager.NodeManager) (*manager.Ids, error) {
	logrus.Infof("Create a new node manager: %+v", in.Name)

	// validate the node manager type provided
	err := validateNodeManager(in, []string{"aws-ec2", "aws-api", "azure-api", "aws", "azure", "azure-vm", "gcp", "gcp-api"})
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Name)
	}

	if len(in.CredentialData) > 0 {
		in.CredentialId, err = srv.handleCredentialData(ctx, in)
		if err != nil {
			return nil, utils.FormatErrorMsg(err, "")
		}
	}

	// fetch account id
	acctID, err := srv.getMgrAccountID(ctx, in, srv.DB)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Name)
	}

	// attempt a dry run connection to ensure the account credentials have the correct permissions
	_, err = srv.Connect(ctx, in)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Name)
	}
	// if we set it here, we consider the manager reachable and that's how we save it in the database
	in.Status = types.StatusReachable

	// if the user has sent in a manager of type "aws" or "azure", we create two manager instances
	// in the background, for example: "aws-api" and "aws-ec2". handleManagersToBeAdded takes care of this
	// logic, sending us back an array of managers to be added to the db
	managers := handleManagersToBeAdded(in)
	mgrIds := make([]*manager.Id, 0, len(managers))
	for _, mgr := range managers {
		mgrID, err := srv.addManagerAndNodes(ctx, mgr, acctID)
		if err != nil {
			return nil, utils.FormatErrorMsg(err, mgrID)
		}
		mgrIds = append(mgrIds, &manager.Id{Id: mgrID})
	}
	return &manager.Ids{Ids: mgrIds}, nil
}

func (srv *Server) handleCredentialData(ctx context.Context, in *manager.NodeManager) (string, error) {
	credType := "aws"
	if strings.HasPrefix(in.Type, "azure") {
		credType = "azure"
	}
	if strings.HasPrefix(in.Type, "gcp") {
		credType = "gcp"
	}
	secretsKv := []*secrets.Kv{}
	for _, kv := range in.CredentialData {
		secretsKv = append(secretsKv, &secrets.Kv{Key: kv.Key, Value: kv.Value})
	}
	newSecret := &secrets.Secret{
		Name: "auto-generated-credential",
		Type: credType,
		Data: secretsKv,
	}
	err := newSecret.Validate()
	if err != nil {
		return "", utils.ProcessInvalid(err, "handleCredentialData: unable to validate secret")
	}
	var id string
	if len(in.CredentialId) > 0 {
		newSecret.Id = in.CredentialId
		_, err = srv.secretsClient.Update(ctx, newSecret)
		if err != nil {
			return "", errors.Wrap(err, "handleCredentialData unable to save credential")
		}
		id = in.CredentialId
	} else {
		secretID, err := srv.secretsClient.Create(ctx, newSecret)
		if err != nil {
			return "", errors.Wrap(err, "handleCredentialData unable to save credential")
		}
		id = secretID.Id
	}
	return id, nil
}

func handleManagersToBeAdded(in *manager.NodeManager) []*manager.NodeManager {
	managers := []*manager.NodeManager{}
	if in.Type == "aws" {
		awsEC2Mgr := *in
		awsEC2Mgr.Type = "aws-ec2"
		managers = append(managers, &awsEC2Mgr)
		awsApiMgr := *in
		awsApiMgr.Type = "aws-api"
		managers = append(managers, &awsApiMgr)
	} else if in.Type == "azure" {
		in.Type = "azure-api"
		managers = append(managers, in)
	} else if in.Type == "gcp" {
		in.Type = "gcp-api"
		managers = append(managers, in)
	} else {
		managers = append(managers, in)
	}
	return managers
}

func (srv *Server) addManagerAndNodes(ctx context.Context, in *manager.NodeManager, acctID string) (string, error) {
	// add node manager to db (table: node_managers)
	id, err := srv.DB.AddNodeManager(in, acctID)
	if err != nil {
		return "", errors.Wrap(err, "addManagerAndNodes unable to add manager")
	}

	// add nodes to db (table: nodes)
	err = srv.addNodes(ctx, id, in, acctID)
	if err != nil {
		return "", errors.Wrap(err, "addManagerAndNodes unable to add nodes")
	}
	return id, nil
}

func (srv *Server) addNodes(ctx context.Context, id string, in *manager.NodeManager, acctID string) error {
	// for the aws-ec2 manager, this will be one node per instance in account
	// for the aws-api manager, this will be one node per region in account
	// for the azure-api manager, this will be one node per subscription in account
	// for the gcp-api manager, this will be one node for the project specified in the credentials
	err := srv.addManagerNodesToDB(ctx, id, in.Name, in.Type, acctID, in.InstanceCredentials, in.CredentialId)
	if err != nil {
		// if we receive an error from adding the nodes to the db, something is likely wrong
		// with the creds. we need to remove the node manager.
		deleteErr := srv.DB.DeleteNodeManager(id)
		if deleteErr != nil {
			deleteErr = errors.Wrapf(deleteErr, "failed to add nodes to db for created node manager %s", err.Error())
			deleteErr = utils.FormatErrorMsg(deleteErr, id)
			return deleteErr
		}
		err = utils.FormatErrorMsg(err, in.Name)
		return err
	}
	return nil
}

// Read a node manager via ID
func (srv *Server) Read(ctx context.Context, in *manager.Id) (*manager.NodeManager, error) {
	logrus.Infof("read node manager with : %+v", in.Id)
	mngr, err := srv.DB.GetNodeManager(in.Id)
	if err != nil {
		err = utils.FormatErrorMsg(err, in.Id)
		return nil, err
	}
	return mngr, nil
}

func (srv *Server) GetNodeWithSecrets(ctx context.Context, id *manager.Id) (*nodes.Node, error) {
	return nodesserver.GetNode(ctx, &nodes.Id{Id: id.Id}, srv.DB, srv.secretsClient)
}

// Update one node manager
func (srv *Server) Update(ctx context.Context, in *manager.NodeManager) (*pb.Empty, error) {
	// Don't log the node manager information because it includes access tokens
	logrus.Debugf("Updating node manager %s", in.Name)

	// validate the node manager type provided
	err := validateNodeManager(in, []string{"aws-ec2", "aws-api", "azure-api", "azure-vm", "gcp-api"})
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Id)
	}
	credID, err := srv.handleNodeManagerCredentialUpdate(ctx, in)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Id)
	}
	in.CredentialId = credID

	// get the account id
	acctID, err := srv.getMgrAccountID(ctx, in, srv.DB)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Id)
	}
	in.AccountId = acctID

	// attempt a dry run connection to ensure the account credentials have the correct
	// permissions
	_, err = srv.Connect(ctx, in)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Name)
	}
	in.Status = "reachable"

	// add node manager to db (table: node_managers)
	id, err := srv.DB.UpdateNodeManager(in)
	if err != nil {
		err = utils.FormatErrorMsg(err, id)
		return nil, err
	}

	// add nodes to db (table: nodes)
	err = srv.addNodes(ctx, id, in, acctID)
	if err != nil {
		err = utils.FormatErrorMsg(err, id)
		return nil, err
	}
	return &empty, nil
}

// Delete a node manager
func (srv *Server) Delete(ctx context.Context, in *manager.Id) (*pb.Empty, error) {
	logrus.Infof("Deleting manager id: %+v", in.Id)
	err := srv.DB.DeleteNodeManager(in.Id)
	if err != nil {
		err = utils.FormatErrorMsg(err, in.Id)
		return nil, err
	}
	return &empty, nil
}

// Delete a node manager with its nodes
func (srv *Server) DeleteWithNodes(ctx context.Context, in *manager.Id) (*manager.Ids, error) {
	logrus.Infof("Deleting manager id with nodes: %+v", in.Id)
	ids, err := srv.DB.DeleteNodeManagerWithNodes(in.Id)
	if err != nil {
		err = utils.FormatErrorMsg(err, in.Id)
		return nil, err
	}
	result := make([]*manager.Id, len(ids))
	for i := range ids {
		result[i] = &manager.Id{Id: ids[i]}
	}
	return &manager.Ids{Ids: result}, nil
}

// Delete a node manager and set its nodes' states to stopped
func (srv *Server) DeleteWithNodeStateStopped(ctx context.Context, in *manager.Id) (*pb.Empty, error) {
	logrus.Infof("Deleting manager id with nodes: %+v", in.Id)
	err := srv.DB.DeleteNodeManagerWithNodeStateUpdate(in.Id, "stopped")
	if err != nil {
		err = utils.FormatErrorMsg(err, in.Id)
		return nil, err
	}
	return &empty, nil
}

// Delete a node manager and set its nodes' states to terminated
func (srv *Server) DeleteWithNodeStateTerminated(ctx context.Context, in *manager.Id) (*pb.Empty, error) {
	logrus.Infof("Deleting manager id with nodes: %+v", in.Id)
	err := srv.DB.DeleteNodeManagerWithNodeStateUpdate(in.Id, "terminated")
	if err != nil {
		err = utils.FormatErrorMsg(err, in.Id)
		return nil, err
	}
	return &empty, nil
}

// List node managers
func (srv *Server) List(ctx context.Context, in *manager.Query) (*manager.NodeManagers, error) {
	logrus.Debugf("Getting Nodes with query: %+v", in)
	nodeManagers, totalCount, err := srv.DB.GetNodeManagers(in.Sort, in.Order, in.Page, in.PerPage, in.FilterMap)
	if err != nil {
		err = utils.FormatErrorMsg(err, "")
		return nil, err
	}
	return &manager.NodeManagers{Managers: nodeManagers, Total: int32(totalCount)}, nil
}

// Connect takes a NodeManager object and tests it's connectivity
func (srv *Server) Connect(ctx context.Context, in *manager.NodeManager) (*pb.Empty, error) {
	logrus.Infof("Connect to manager: %s with credential id: %s", in.Name, in.CredentialId)
	if strings.HasPrefix(in.Type, "aws") {
		myaws, err := managers.GetAWSManagerFromCredential(ctx, in.CredentialId, srv.DB, srv.secretsClient)
		if err != nil {
			return nil, utils.FormatErrorMsg(err, "")
		}
		err = myaws.TestConnectivity(ctx)
		if err != nil {
			return nil, utils.FormatErrorMsg(err, "")
		}
		logrus.Info("AWS connection successful.")
	}
	if strings.HasPrefix(in.Type, "azure") {
		myazure, err := managers.GetAzureManagerFromCredential(ctx, in.CredentialId, srv.DB, srv.secretsClient)
		if err != nil {
			return nil, utils.FormatErrorMsg(err, "")
		}
		err = myazure.TestConnectivity(ctx)
		if err != nil {
			return nil, utils.FormatErrorMsg(err, "")
		}
		logrus.Info("Azure connection successful.")
	}
	if strings.HasPrefix(in.Type, "gcp") {
		mygcp, err := managers.GetGCPManagerFromCredential(ctx, in.CredentialId, srv.DB, srv.secretsClient)
		if err != nil {
			return nil, utils.FormatErrorMsg(err, "")
		}
		err = mygcp.TestConnectivity(ctx)
		if err != nil {
			return nil, utils.FormatErrorMsg(err, "")
		}

		logrus.Info("GCP connection successful.")
	}
	return &pb.Empty{}, nil
}

// ConnectManager attempts a connection to a node manager given a manager id
func (srv *Server) ConnectManager(ctx context.Context, in *manager.Id) (*pb.Empty, error) {
	logrus.Infof("Connect to manager: %s", in.Id)
	// if Automate (manual) node manager, return. no need to check connectivity there.
	if in.Id == mgrtypes.AutomateManagerID {
		return &pb.Empty{}, nil
	}
	// get the manager
	mgr, err := srv.DB.GetNodeManager(in.Id)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, "")
	}

	var status string
	// call the connect function on the mgr.
	// if we get an error, we assume unreachable, else assume reachable
	_, err = srv.Connect(ctx, mgr)
	if err != nil {
		// give nodemanager of status of unreachable
		logrus.Errorf("nodemanager %s unable to connect: %v", mgr.Name, err)
		status = "unreachable"
	} else {
		// give nodemanager of status of reachable
		status = "reachable"
	}
	if err := srv.DB.UpdateManagerStatus(in.Id, status); err != nil {
		return nil, utils.FormatErrorMsg(err, "")
	}
	return &pb.Empty{}, nil
}

// SearchFields for the manager, return fields
func (srv *Server) SearchNodeFields(ctx context.Context, in *manager.FieldQuery) (*manager.Fields, error) {
	logrus.Infof("Getting node manager query fields: %+v", in)

	mgr, err := srv.DB.GetNodeManager(in.NodeManagerId)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.NodeManagerId)
	}
	switch mgr.Type {
	case "aws-ec2", "aws-api":
		return srv.searchAwsFields(ctx, in)
	case "azure-vm", "azure-api":
		return srv.searchAzureFields(ctx, in)
	case "gcp-api":
		return srv.searchGenericNodesFields(ctx, in, in.NodeManagerId)
	case "automate":
		return srv.searchGenericNodesFields(ctx, in, mgrtypes.AutomateManagerID)
	default:
		return nil, &utils.InvalidError{Msg: fmt.Sprintf("Unsupported manager type: %s", mgr.Type)}
	}
}

func (srv *Server) searchAzureFields(ctx context.Context, in *manager.FieldQuery) (*manager.Fields, error) {
	myazure, err := managers.GetAzureManagerFromID(ctx, in.NodeManagerId, srv.DB, srv.secretsClient)
	if err != nil {
		err = utils.FormatErrorMsg(err, in.NodeManagerId)
		return nil, err
	}
	if in.Query == nil {
		return nil, &utils.InvalidError{Msg: "Please provide a query."}
	}
	results, err := myazure.QueryField(ctx, in.GetQuery().GetFilterMap(), in.Field)
	if err != nil {
		err = utils.FormatErrorMsg(err, "")
		return nil, err
	}
	return &manager.Fields{Fields: results}, nil
}

func (srv *Server) searchGenericNodesFields(ctx context.Context, in *manager.FieldQuery, managerId string) (*manager.Fields, error) {
	if in.Query == nil {
		return nil, &utils.InvalidError{Msg: "Please provide a query."}
	}
	filters := in.GetQuery().GetFilterMap()
	filters = append(filters, &common.Filter{Key: "manager_id", Values: []string{managerId}})
	results, err := srv.DB.QueryManualNodesFields(ctx, filters, in.Field)
	if err != nil {
		err = utils.FormatErrorMsg(err, "")
		return nil, err
	}
	return &manager.Fields{Fields: results}, nil
}

func (srv *Server) searchAwsFields(ctx context.Context, in *manager.FieldQuery) (*manager.Fields, error) {
	myaws, _, err := managers.GetAWSManagerFromID(ctx, in.NodeManagerId, srv.DB, srv.secretsClient)
	if err != nil {
		err = utils.FormatErrorMsg(err, in.NodeManagerId)
		return nil, err
	}
	if in.Query == nil {
		return nil, &utils.InvalidError{Msg: "Please provide a query."}
	}
	results, err := myaws.QueryField(ctx, in.GetQuery().GetFilterMap(), in.Field)
	if err != nil {
		err = utils.FormatErrorMsg(err, "")
		return nil, err
	}
	return &manager.Fields{Fields: results}, nil
}

// SearchNodes for the manager, returns nodes
func (srv *Server) SearchNodes(ctx context.Context, in *manager.NodeQuery) (*manager.Nodes, error) {
	logrus.Infof("Getting nodes from node manager: %+v", in)

	mgr, err := srv.DB.GetNodeManager(in.NodeManagerId)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.NodeManagerId)
	}
	switch mgr.Type {
	case "aws-ec2":
		return srv.searchAwsNodes(ctx, in)
	case "azure-vm":
		return srv.searchAzureNodes(ctx, in)
	case "aws-api":
		return srv.searchAwsRegions(ctx, in)
	case "azure-api":
		return srv.searchAzureSubscriptions(ctx, in)
	case "gcp-api":
		return srv.searchGenericNodes(ctx, in.GetQuery().GetFilterMap(), in.NodeManagerId)
	case "automate":
		return srv.searchGenericNodes(ctx, in.GetQuery().GetFilterMap(), mgrtypes.AutomateManagerID)
	default:
		return nil, &utils.InvalidError{Msg: fmt.Sprintf("Unsupported manager type: %s", mgr.Type)}
	}
}

func (srv *Server) searchAwsRegions(ctx context.Context, in *manager.NodeQuery) (*manager.Nodes, error) {
	myaws, _, err := managers.GetAWSManagerFromID(ctx, in.NodeManagerId, srv.DB, srv.secretsClient)
	if err != nil {
		err = utils.FormatErrorMsg(err, "")
		return nil, err
	}

	var includeFilters []string
	var excludeFilters []string
	for _, filter := range in.GetQuery().GetFilterMap() {
		if filter.Key != "region" {
			continue
		}

		for _, val := range filter.Values {
			val = strings.TrimSuffix(val, "*")
			if filter.Exclude {
				excludeFilters = append(excludeFilters, val)
			} else {
				includeFilters = append(includeFilters, val)
			}
		}
	}

	if len(includeFilters) > 0 && len(excludeFilters) > 0 {
		err = &utils.InvalidError{Msg: "using include and exclude filters in the same request is unsupported"}
		return nil, utils.FormatErrorMsg(err, "")
	}

	regions, err := myaws.GetRegions(ctx)
	if err != nil {
		err = utils.FormatErrorMsg(err, "")
		return nil, err
	}

	if len(includeFilters) > 0 {
		allRegions := regions
		regions = make([]string, 0)

		for _, filter := range includeFilters {
			matches := stringutils.SliceFilter(allRegions, func(region string) bool {
				return strings.HasPrefix(region, filter)
			})
			regions = append(regions, matches...)
		}
	}

	for _, filter := range excludeFilters {
		regions = stringutils.SliceFilter(regions, func(region string) bool {
			return !strings.HasPrefix(region, filter)
		})
	}

	regions = utils.UniqueStringSlice(regions)
	return &manager.Nodes{Total: int32(len(regions)), Nodes: regions}, nil
}

func (srv *Server) searchAzureSubscriptions(ctx context.Context, in *manager.NodeQuery) (*manager.Nodes, error) {
	myazure, err := managers.GetAzureManagerFromID(ctx, in.NodeManagerId, srv.DB, srv.secretsClient)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.NodeManagerId)
	}
	subs, err := myazure.GetSubscriptions(ctx, in.GetQuery().GetFilterMap())
	if err != nil {
		return nil, utils.FormatErrorMsg(err, "")
	}

	nodes := make([]string, 0, len(subs))
	for _, sub := range subs {
		nodes = append(nodes, sub.Name)
	}
	return &manager.Nodes{Total: int32(len(subs)), Nodes: nodes}, nil
}

func (srv *Server) searchAzureNodes(ctx context.Context, in *manager.NodeQuery) (*manager.Nodes, error) {
	myazure, err := managers.GetAzureManagerFromID(ctx, in.NodeManagerId, srv.DB, srv.secretsClient)
	if err != nil {
		err = utils.FormatErrorMsg(err, in.NodeManagerId)
		return nil, err
	}

	nodesRes, err := myazure.QueryVMs(ctx, in.GetQuery().GetFilterMap())
	if err != nil {
		err = utils.FormatErrorMsg(err, "")
		return nil, err
	}

	nodeIds := manager.Nodes{}
	for _, nodes := range nodesRes {
		for _, item := range nodes {
			nodeIds.Nodes = append(nodeIds.Nodes, fmt.Sprintf("%s:%s", item.Name, item.Id))
		}
	}
	nodeIds.Total = int32(len(nodeIds.Nodes))
	return &nodeIds, nil
}

func (srv *Server) searchGenericNodes(ctx context.Context, filters []*common.Filter, managerId string) (*manager.Nodes, error) {
	var perPage int32 = 100
	var total int32 = 100
	var nodes []*nodes.Node
	filters = append(filters, &common.Filter{Key: "manager_id", Values: []string{managerId}})
	for cnt := int32(1); int32(len(nodes)) < total; cnt++ {
		logrus.Debugf("getting nodes with page %d for total %d, per_page %d", cnt, total, perPage)
		pageNodes, totalCount, err := srv.DB.GetNodes("name", 0, cnt, perPage, filters)
		if err != nil {
			err = utils.FormatErrorMsg(err, "")
			return nil, err
		}
		total = totalCount.Total
		nodes = append(nodes, pageNodes...)
	}

	nodeIds := manager.Nodes{}
	for _, node := range nodes {
		nodeIds.Nodes = append(nodeIds.Nodes, node.Name)
	}
	nodeIds.Total = int32(len(nodeIds.Nodes))
	return &nodeIds, nil
}

func (srv *Server) searchAwsNodes(ctx context.Context, in *manager.NodeQuery) (*manager.Nodes, error) {
	myaws, _, err := managers.GetAWSManagerFromID(ctx, in.NodeManagerId, srv.DB, srv.secretsClient)
	if err != nil {
		err = utils.FormatErrorMsg(err, "")
		return nil, err
	}

	nodesRes, err := myaws.QueryNodes(ctx, in.GetQuery().GetFilterMap(), false)
	if err != nil {
		err = utils.FormatErrorMsg(err, "")
		logrus.Errorf("There was an error listing nodes: %s", err.Error())
		return nil, err
	}

	nodeIds := manager.Nodes{}
	for _, nodes := range nodesRes {
		for _, item := range nodes {
			nodeIds.Nodes = append(nodeIds.Nodes, item.Name)
		}
	}
	nodeIds.Total = int32(len(nodeIds.Nodes))
	return &nodeIds, nil
}

func (srv *Server) addManagerNodesToDB(ctx context.Context, managerId string, managerName string, managerType string, managerAcctId string, instanceCredentials []*manager.CredentialsByTags, credential string) error {
	var nodeIds []string
	var err error

	switch managerType {
	case "aws-ec2":
		nodeIds, err = srv.addNodeForEachEc2Instance(ctx, managerId, managerName, managerAcctId, instanceCredentials)
		if err != nil {
			return errors.Wrapf(err, "addManagerNodesToDb unable to add a node for each ec2 instance for manager %s", managerId)
		}
		srv.getInstanceStatesAndUpdateDB(ctx, managerId, managerAcctId)
	case "aws-api":
		nodeIds, err = srv.addNodeForEachApiRegion(ctx, managerId, managerName, managerAcctId, credential)
		if err != nil {
			return errors.Wrapf(err, "addManagerNodesToDb unable to add a node for each api region for manager %s", managerId)
		}
	case "azure-api":
		nodeIds, err = srv.addNodeForEachSubscription(ctx, managerId, managerName, credential)
		if err != nil {
			return errors.Wrapf(err, "addManagerNodesToDb unable to add a node for each subscription for manager %s", managerId)
		}
	case "azure-vm":
		nodeIds, err = srv.addNodeForEachVM(ctx, managerId, managerName, instanceCredentials, managerAcctId)
		if err != nil {
			return errors.Wrapf(err, "addManagerNodesToDb unable to add a node for each vm for manager %s", managerId)
		}
	case "gcp-api":
		_, err := srv.addNodeForGcpApi(ctx, managerId, managerAcctId, credential)
		if err != nil {
			return errors.Wrapf(err, "addManagerNodesToDb unable to add a node for each api region for manager %s", managerId)
		}
	default:
		return &utils.InvalidError{Msg: "valid manager types are: 'aws-ec2', 'aws-api', 'azure-api', 'azure-vm', 'gcp-api'"}
	}
	return srv.DB.RemoveStaleNodeAssociations(managerId, nodeIds)
}

func (srv *Server) getInstanceStatesAndUpdateDB(ctx context.Context, mgrId string, mgrAcctID string) {
	// GetStateInfoByManager will get manager connection for each manager
	// and call queryStatus for each region for each manager
	instanceStatesResp, err := managers.GetStateInfoByManager(ctx, mgrId, srv.DB, "aws-ec2", srv.secretsClient)
	if err != nil {
		logrus.Errorf("queryAwsEc2InstanceStates unable to get state information for instances %s", err.Error())
		return
	}
	// update each instance's source_state; if source_state != running, update status=unreachable
	for _, inst := range instanceStatesResp {
		_, err := srv.DB.UpdateOrInsertInstanceSourceStateInDb(inst, mgrId, mgrAcctID, "aws-ec2")
		if err != nil {
			logrus.Errorf("queryAwsEc2InstanceStates unable to update db with state %s", err.Error())
			return
		}
	}
}

func (srv *Server) addNodeForEachEc2Instance(ctx context.Context, managerId string, managerName string, managerAcctId string, instanceCredentials []*manager.CredentialsByTags) ([]string, error) {
	logrus.Infof("Getting instances for node manager: %+v", managerId)
	myaws, _, err := managers.GetAWSManagerFromID(ctx, managerId, srv.DB, srv.secretsClient)
	if err != nil {
		return nil, fmt.Errorf("addNodeForEachEc2Instance; error getting connection %s %s", managerName, err.Error())
	}
	filters := make([]*common.Filter, 0)
	awsNodes, err := myaws.QueryNodes(ctx, filters, true)
	if err != nil {
		return nil, fmt.Errorf("addNodeForEachEc2Instance; there was an error listing nodes: %s %s", managerName, err.Error())
	}
	nodesArr := make([]*manager.ManagerNode, 0)
	for _, nodes := range awsNodes {
		nodesArr = append(nodesArr, nodes...)
	}
	nodeIds := srv.DB.AddManagerNodesToDB(nodesArr, managerId, managerAcctId, instanceCredentials, "aws-ec2")
	logrus.Debugf("added %d nodes to the db", len(nodeIds))
	return nodeIds, nil
}

func (srv *Server) addNodeForEachApiRegion(ctx context.Context, managerId string, managerName string, managerAcctId string, credential string) ([]string, error) {
	logrus.Infof("Getting regions for node manager: %+v", managerId)
	myaws, _, err := managers.GetAWSManagerFromID(ctx, managerId, srv.DB, srv.secretsClient)
	if err != nil {
		return nil, fmt.Errorf("addNodeForEachApiRegion; error getting connection %s %s", managerName, err.Error())
	}

	filters := make([]*common.Filter, 0)
	regions, err := myaws.QueryField(ctx, filters, "regions")
	if err != nil {
		return nil, fmt.Errorf("addNodeForEachApiRegion; there was an error listing nodes: %s %s", managerName, err.Error())
	}
	acctAlias, err := myaws.GetAccountAlias(ctx)
	if err != nil {
		return nil, fmt.Errorf("addNodeForEachApiRegion; unable to get account alias: %s %s", managerName, err.Error())
	}
	if len(acctAlias) == 0 {
		// the user may have never set an alias for their account, and that's ok, but we should log it out
		logrus.Warnf("addNodeForEachApiRegion; no account aliases found for manager: %s", managerName)
	}
	nodeIds := srv.DB.AddManagerRegionsToDB(regions, managerId, managerAcctId, credential, acctAlias)
	logrus.Debugf("added %d regions to the db", len(nodeIds))
	return nodeIds, nil
}

func (srv *Server) addNodeForEachSubscription(ctx context.Context, managerId string, managerName string, credential string) ([]string, error) {
	logrus.Infof("Getting subscriptions for node manager: %+v", managerId)
	creds, err := managers.GetAzureManagerFromID(ctx, managerId, srv.DB, srv.secretsClient)
	if err != nil {
		return nil, errors.Wrapf(err, "addNodeForEachSubscription; error getting azure client %s", managerName)
	}
	subs, err := creds.ListSubscriptions(ctx)
	if err != nil {
		return nil, errors.Wrapf(err, "addNodeForEachSubscription; error listing subscriptions %s", managerName)
	}
	nodeIds := srv.DB.AddManagerSubscriptionsToDB(subs, managerId, creds.TenantID, credential)
	logrus.Debugf("added %d subscriptions to the db", len(nodeIds))
	return nodeIds, nil
}

func (srv *Server) addNodeForGcpApi(ctx context.Context, managerId string, managerAccId string, credential string) (*string, error) {
	nodeId, err := srv.DB.AddManagerGcpApi(managerId, managerAccId, credential)
	if err != nil {
		return nil, errors.Wrapf(err, "addNodeForGcpApi error for manager %s", managerId)
	}
	logrus.Debugf("addNodeForGcpApi: added api node %s for manager %s", *nodeId, managerId)
	return nodeId, nil
}

func (srv *Server) addNodeForEachVM(ctx context.Context, managerId string, managerName string, instanceCredentials []*manager.CredentialsByTags, managerAcctId string) ([]string, error) {
	logrus.Infof("Getting vms for node manager: %+v", managerId)
	creds, err := managers.GetAzureManagerFromID(ctx, managerId, srv.DB, srv.secretsClient)
	if err != nil {
		return nil, errors.Wrapf(err, "addNodeForEachVM; error getting azure client %s", managerName)
	}
	vms, err := creds.QueryVMs(ctx, []*common.Filter{})
	if err != nil {
		return nil, errors.Wrapf(err, "addNodeForEachVM; error listing vms %s", managerName)
	}

	nodesArr := make([]*manager.ManagerNode, 0)
	for _, nodes := range vms {
		nodesArr = append(nodesArr, nodes...)
	}
	vmIds := srv.DB.AddManagerNodesToDB(nodesArr, managerId, managerAcctId, instanceCredentials, "azure-vm")
	logrus.Debugf("added %d vms to the db", len(vmIds))
	return vmIds, nil
}

// ProcessNode takes node metadata information and updates or adds the node reference accordingly
func (srv *Server) ProcessNode(ctx context.Context, in *manager.NodeMetadata) (*manager.ProcessNodeResponse, error) {
	logrus.Infof("ProcessNode processing node with uuid: %s and source_id: %s", in.Uuid, in.SourceId)

	err := srv.DB.ProcessIncomingNode(in)
	if err != nil {
		err = utils.FormatErrorMsg(err, in.Uuid)
		return nil, err
	}

	return &manager.ProcessNodeResponse{}, nil
}

// ChangeNodeState takes a node id and desired state and modified the node to reflect selected state. If selected state
// is not running, status will be updated to unreachable.
func (srv *Server) ChangeNodeState(ctx context.Context, in *manager.NodeState) (*manager.ChangeNodeStateResponse, error) {
	logrus.Infof("ChangeNodeState processing node with uuid: %s requested state: %s", in.Id, in.State)

	// first we get the current node state
	state, err := srv.DB.GetNodeState(in.Id)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Id)
	}
	// if node state is terminated, the user can no longer update the node state. they terminated it.
	if state == manager.NodeState_TERMINATED.String() {
		return nil, status.Error(codes.InvalidArgument, fmt.Sprintf("unable to update terminated node %s", in.Id))
	}
	// update the node state
	err = srv.DB.ChangeNodeState(in)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Id)
	}
	return &manager.ChangeNodeStateResponse{}, nil
}

func validateNodeManager(in *manager.NodeManager, validMgrTypes []string) error {
	if len(in.CredentialId) > 0 && len(in.CredentialData) > 0 {
		return &utils.InvalidError{Msg: fmt.Sprintf("only one or no credential is allowed %+s %+v", in.CredentialId, in.CredentialData)}
	}
	if stringutils.SliceContains(validMgrTypes, in.Type) {
		return nil
	}
	return &utils.InvalidError{Msg: fmt.Sprintf("valid types for node manager instance are %+v", validMgrTypes)}
}

func (srv *Server) handleNodeManagerCredentialUpdate(ctx context.Context, in *manager.NodeManager) (string, error) {
	var credID string
	var err error
	if len(in.CredentialData) > 0 {
		if len(in.CredentialId) == 0 {
			// get cred_id already associated with mgr from db
			mgr, err := srv.DB.GetNodeManager(in.Id)
			if err != nil {
				return "", errors.Wrap(err, "handleNodeManagerCredentialUpdate unable to retrieve nodemanager from db")
			}
			in.CredentialId = mgr.CredentialId
		}
		// update cred with credential data info
		credID, err = srv.handleCredentialData(ctx, in)
		if err != nil {
			return "", errors.Wrap(err, "handleNodeManagerCredentialUpdate unable to handle cred data update")
		}
	} else if len(in.CredentialId) > 0 {
		// update the cred id to be assoc with manager
		credID = in.CredentialId
	}
	return credID, nil
}

// SearchManagerNodes returns a list of type ManagerNode give a nodeQuery that includes a manager id
// and whatever filters needed. This is used by the inspec-agent when resolving jobs to a desired set of nodes.
func (srv *Server) SearchManagerNodes(ctx context.Context, in *manager.NodeQuery) (*manager.ManagerNodes, error) {
	// read manager by id to determine what type it is (aws, azure)
	mgr, err := srv.DB.GetNodeManager(in.GetNodeManagerId())
	if err != nil {
		return nil, errors.Wrap(err, "unable to retrieve manager from db")
	}
	// based on type, gather the nodes
	switch mgr.Type {
	case "aws-ec2":
		logrus.Info("getting manager nodes for aws-ec2 mgr")
		myaws, ssm, err := managers.GetAWSManagerFromID(ctx, in.GetNodeManagerId(), srv.DB, srv.secretsClient)
		if err != nil {
			return nil, fmt.Errorf("Failed to get credentials for aws node manager %s: %s", in.GetNodeManagerId(), err)
		}
		nodes, err := myaws.QueryNodes(ctx, in.GetQuery().GetFilterMap(), ssm)
		if err != nil {
			return nil, fmt.Errorf("Failed to query all nodes for job manager %s: %s", in.GetNodeManagerId(), err)
		}
		nodesArr := make([]*manager.ManagerNode, 0)
		// the nodes are returned as map[region_val][]*manager.ManagerNode, so here
		// we run through the map to collect all the nodes into one array
		for _, nodesList := range nodes {
			nodesArr = append(nodesArr, nodesList...)
		}
		return &manager.ManagerNodes{Nodes: nodesArr}, nil
	case "azure-vm":
		logrus.Info("getting manager nodes for azure-vm mgr")
		myazure, err := managers.GetAzureManagerFromID(ctx, in.GetNodeManagerId(), srv.DB, srv.secretsClient)
		if err != nil {
			return nil, fmt.Errorf("Failed to get credentials for azure node manager %s: %s", in.GetNodeManagerId(), err)
		}
		nodes, err := myazure.QueryVMs(ctx, in.GetQuery().GetFilterMap())
		if err != nil {
			return nil, fmt.Errorf("Failed to query all nodes for job manager %s: %s", in.GetNodeManagerId(), err)
		}
		nodesArr := make([]*manager.ManagerNode, 0)
		// the nodes are returned as map[subs_val][]*manager.ManagerNode, so here
		// we run through the map to collect all the nodes into one array
		for _, nodesList := range nodes {
			nodesArr = append(nodesArr, nodesList...)
		}
		return &manager.ManagerNodes{Nodes: nodesArr}, nil
	case "azure-api":
		logrus.Info("getting manager nodes for azure-api mgr")
		myazure, err := managers.GetAzureManagerFromID(ctx, in.GetNodeManagerId(), srv.DB, srv.secretsClient)
		if err != nil {
			return nil, errors.Wrapf(err, "Failed to get credentials for azure node manager: %s", in.GetNodeManagerId())
		}
		subs, err := myazure.GetSubscriptions(ctx, in.GetQuery().GetFilterMap())
		if err != nil {
			return nil, fmt.Errorf("Failed to query all nodes for job manager %s: %s", in.GetNodeManagerId(), err)
		}
		return &manager.ManagerNodes{Nodes: subs}, nil
	}
	return &manager.ManagerNodes{}, nil
}
