package nodes

import (
	"strings"

	"golang.org/x/net/context"

	"fmt"

	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/components/compliance-service/secretsint"
	"github.com/chef/automate/components/compliance-service/utils"
	"github.com/chef/automate/components/nodemanager-service/api/nodes"
	"github.com/chef/automate/components/nodemanager-service/mgrtypes"
	"github.com/chef/automate/components/nodemanager-service/pgdb"
	pb "github.com/golang/protobuf/ptypes/empty"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"

	"github.com/chef/automate/lib/grpc/secureconn"
)

// Server implementation for nodes
type Server struct {
	db            *pgdb.DB
	secretsClient secrets.SecretsServiceClient
}

var empty = pb.Empty{}

// New creates a new server
func New(db *pgdb.DB, connectionFactory *secureconn.Factory, secretsEndpoint string) *Server {
	conf := &Server{db: db}
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

// Create a new node
func (srv *Server) Create(ctx context.Context, in *nodes.Node) (*nodes.Id, error) {
	logrus.Infof("Create a new node: %+v", in)

	secrets := secretsint.New(srv.secretsClient)
	allExist, _, _ := secrets.CheckSecrets(in.GetTargetConfig().GetSecrets())
	if !allExist {
		return nil, utils.FormatErrorMsg(&utils.InvalidError{Msg: "AddNode unable to confirm secrets existence"}, "")
	}
	in.Manager = "automate"
	nID, err := srv.db.AddNode(in)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, "")
	}
	in.Id = nID
	// associate node id with manual manager id
	err = srv.db.AssociateNodeIDsWithManagerID([]string{nID}, mgrtypes.AutomateManagerID)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Id)
	}

	return &nodes.Id{Id: nID}, nil
}

// Create a new node
func (srv *Server) BulkCreate(ctx context.Context, in *nodes.Nodes) (*nodes.Ids, error) {
	logrus.Infof("Create new nodes: %+v", in)

	secrets := secretsint.New(srv.secretsClient)
	for _, node := range in.GetNodes() {
		allExist, _, _ := secrets.CheckSecrets(node.TargetConfig.Secrets)
		if !allExist {
			return nil, utils.FormatErrorMsg(&utils.InvalidError{Msg: "AddNode unable to confirm secrets existence"}, "")
		}
	}

	ids, err := srv.db.BulkAddNodes(in.GetNodes())
	if err != nil {
		ids := strings.Join(ids, ",")
		return nil, utils.FormatErrorMsg(err, ids)
	}

	// associate node id with manual manager id
	err = srv.db.AssociateNodeIDsWithManagerID(ids, mgrtypes.AutomateManagerID)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, "")
	}

	return &nodes.Ids{Ids: ids}, nil
}

// Read a node via ID
func (srv *Server) Read(ctx context.Context, in *nodes.Id) (*nodes.Node, error) {
	logrus.Infof("read node with : %+v", in)
	return GetNode(ctx, in, srv.db, srv.secretsClient)
}

func GetNode(ctx context.Context, in *nodes.Id, db *pgdb.DB, secretsClient secrets.SecretsServiceClient) (*nodes.Node, error) {
	node, err := db.GetNode(ctx, in.Id)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Id)
	}

	secretIds, err := db.GetNodeSecretIds(ctx, in.Id)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Id)
	}

	secretsAgg, err := getSecretsAgg(ctx, secretIds, secretsClient)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Id)
	}

	err = resolveInspecConfigWithSecrets(node.TargetConfig, secretsAgg)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Id)
	}

	return node, nil
}

func resolveInspecConfigWithSecrets(tc *nodes.TargetConfig, secretsMaps []map[string]string) error {
	secretsArr := make([]*nodes.NodeSecrets, 0)

	for _, secretsMap := range secretsMaps {
		secret := nodes.NodeSecrets{}
		if secretsMap["username"] != "" {
			secret.User = secretsMap["username"]
		}
		if tc.Backend == "ssh" && secretsMap["username"] != "" {
			if secretsMap["key"] != "" {
				keyPath, err := nodes.PrepareSSHPrivateKey(secretsMap["key"])
				if err != nil {
					return errors.Wrap(err, "resolveInspecConfigWithSecrets error preparing private key")
				}
				arr := []string{keyPath}
				secret.KeyFiles = arr
			} else if secretsMap["password"] != "" {
				secret.Password = secretsMap["password"]
			}
			secretsArr = append(secretsArr, &secret)
			if tc.Port == 0 {
				tc.Port = 22
			}
		}

		if tc.Backend == "winrm" && secretsMap["username"] != "" {
			if secretsMap["password"] != "" {
				secret.Password = secretsMap["password"]
			}
			secretsArr = append(secretsArr, &secret)
			if tc.Port == 0 {
				tc.Port = 5985
			}
		}

		if tc.Sudo {
			if secretsMap["sudo_password"] != "" {
				tc.SudoPassword = secretsMap["sudo_password"]
			}

			// Update SudoOptions if provided
			tc.SudoOptions += secretsMap["options"]
		}

		if tc.Backend == "aws" {
			tc.AwsUser = secretsMap["AWS_ACCESS_KEY_ID"]
			tc.AwsPassword = secretsMap["AWS_SECRET_ACCESS_KEY"]
		}

		if tc.Backend == "gcp" {
			tc.GcpCredsJson = secretsMap["GOOGLE_CREDENTIALS_JSON"]
		}
	}
	if len(secretsArr) == 1 {
		tc.User = secretsArr[0].User
		tc.KeyFiles = secretsArr[0].KeyFiles
		tc.Password = secretsArr[0].Password
	} else {
		tc.SecretsArr = secretsArr
	}
	return nil
}

// getSecretsAgg is used to collect all secret data for a node (given an array of secret ids)
func getSecretsAgg(ctx context.Context, secretIds []string, secretsClient secrets.SecretsServiceClient) ([]map[string]string, error) {
	arrMaps := make([]map[string]string, 0)

	for _, id := range secretIds {
		arrMap := make(map[string]string, 0)
		s, err := secretsClient.Read(ctx, &secrets.Id{Id: id})
		if err != nil {
			logrus.WithError(err).Errorf("could not read credential %q", id)
			continue
		}

		for _, kv := range s.Data {
			if s.Type == "sudo" && kv.Key == "password" {
				arrMap["sudo_password"] = kv.Value
			} else {
				arrMap[kv.Key] = kv.Value
			}
		}
		arrMaps = append(arrMaps, arrMap)
	}

	return arrMaps, nil
}

// Update one node
func (srv *Server) Update(ctx context.Context, in *nodes.Node) (*pb.Empty, error) {
	logrus.Infof("update node with : %+v", in)

	// validate the node can be updated
	in, err := srv.validateNodeUpdate(ctx, in)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Id)
	}

	secrets := secretsint.New(srv.secretsClient)
	if in.GetTargetConfig() != nil {
		allExist, _, _ := secrets.CheckSecrets(in.GetTargetConfig().GetSecrets())
		if !allExist {
			return nil, utils.FormatErrorMsg(&utils.InvalidError{Msg: "AddNode unable to confirm secrets existence"}, "")
		}
	}

	err = srv.db.UpdateNode(in)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Id)
	}

	return &empty, nil
}

// Delete a node
func (srv *Server) Delete(ctx context.Context, in *nodes.Id) (*pb.Empty, error) {
	logrus.Infof("Node manager is deleting node id: %+v", in)
	_, err := srv.db.DeleteNode(in.Id)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, in.Id)
	}
	return &empty, nil
}

// List nodes based on a query
func (srv *Server) List(ctx context.Context, in *nodes.Query) (*nodes.Nodes, error) {
	logrus.Debugf("Getting Nodes with query: %+v", in)
	dbnodes, totalCount, err := srv.db.GetNodes(in.Sort, in.Order, in.Page, in.PerPage, in.Filters)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, "")
	}
	return &nodes.Nodes{
		Nodes:            dbnodes,
		Total:            totalCount.Total,
		TotalUnreachable: totalCount.Unreachable,
		TotalReachable:   totalCount.Reachable,
		TotalUnknown:     totalCount.Unknown}, nil
}

// Delete nodes based on a query
func (srv *Server) BulkDelete(ctx context.Context, in *nodes.Query) (*nodes.BulkDeleteResponse, error) {
	logrus.Debugf("Deleting Nodes with query: %+v", in)
	names, err := srv.db.DeleteNodesWithQuery(in.Filters)
	if err != nil {
		return nil, utils.FormatErrorMsg(err, "")
	}
	return &nodes.BulkDeleteResponse{Names: names}, nil
}

func (srv *Server) validateNodeUpdate(ctx context.Context, in *nodes.Node) (*nodes.Node, error) {
	fullNode := in
	node, err := GetNode(ctx, &nodes.Id{Id: in.Id}, srv.db, srv.secretsClient)
	if err != nil {
		return fullNode, errors.Wrapf(err, "unable to retrieve node from db %s", in.Id)
	}
	switch node.Manager {
	case "aws-api", "azure-api", "gcp-api":
		return fullNode, &utils.InvalidError{Msg: fmt.Sprintf("invalid option. unable to update %s node", in.Manager)}
	case "aws-ec2", "azure-vm":
		// the name, host, and tags of these nodes is retrieved from the provider, so let's not let the user update it
		fullNode.Name = node.Name
		fullNode.TargetConfig.Host = node.GetTargetConfig().GetHost()
		fullNode.Tags = node.Tags
		if node.GetTargetConfig().GetBackend() == "ssm" && len(in.GetTargetConfig().GetSecrets()) == 0 {
			logrus.Infof("backend for node %s is ssm; no secrets assigned to node", node.Name)
			fullNode.TargetConfig.Backend = node.GetTargetConfig().GetBackend()
		}
		if in.Name != "" && in.Name != node.Name {
			logrus.Warnf("invalid option. unable to update name of %s node", in.Manager)
		} else if in.GetTargetConfig().GetHost() != "" && in.GetTargetConfig().GetHost() != node.GetTargetConfig().GetHost() {
			logrus.Warnf("invalid option. unable to update host of %s node", in.Manager)
		}
	}
	return fullNode, nil
}

// UpdateNodeDetectInfo updates the detect info for a node. used by the inspec-agent runner
func (srv *Server) UpdateNodeDetectInfo(ctx context.Context, in *nodes.NodeDetectJobInfo) (*pb.Empty, error) {
	err := srv.db.UpdateNodeDetectInfo(ctx, in)
	return &pb.Empty{}, err
}

// UpdateNodeDetectInfo updates the connection error for a node. used by the inspec-agent.
func (srv *Server) UpdateNodeConnectionError(ctx context.Context, in *nodes.NodeError) (*pb.Empty, error) {
	err := srv.db.UpdateNodeConnectionErr(ctx, in)
	return &pb.Empty{}, err
}

func (srv *Server) BulkDeleteById(ctx context.Context, in *nodes.Ids) (*nodes.BulkDeleteResponse, error) {
	names, err := srv.db.DeleteNodesById(ctx, in.GetIds())
	if err != nil {
		return nil, err
	}
	return &nodes.BulkDeleteResponse{Names: names}, nil
}
