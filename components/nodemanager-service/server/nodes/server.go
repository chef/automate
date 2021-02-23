package nodes

import (
	"context"
	"fmt"
	"strings"

	pb "github.com/golang/protobuf/ptypes/empty"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"google.golang.org/grpc"

	"github.com/chef/automate/api/external/lib/errorutils"
	"github.com/chef/automate/api/external/secrets"
	"github.com/chef/automate/api/interservice/compliance/common"
	"github.com/chef/automate/api/interservice/nodemanager/nodes"
	authzConstants "github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/compliance-service/secretsint"
	"github.com/chef/automate/components/nodemanager-service/mgrtypes"
	"github.com/chef/automate/components/nodemanager-service/pgdb"
	"github.com/chef/automate/lib/grpc/auth_context"
	"github.com/chef/automate/lib/grpc/secureconn"
	libSecrets "github.com/chef/automate/lib/secrets"
	"github.com/chef/automate/lib/stringutils"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
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
		return nil, errorutils.FormatErrorMsg(&errorutils.InvalidError{Msg: "AddNode unable to confirm secrets existence"}, "")
	}
	in.Manager = "automate"
	nID, err := srv.db.AddNode(in)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}
	in.Id = nID
	// associate node id with manual manager id
	err = srv.db.AssociateNodeIDsWithManagerID([]string{nID}, mgrtypes.AutomateManagerID)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
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
			return nil, errorutils.FormatErrorMsg(&errorutils.InvalidError{Msg: "AddNode unable to confirm secrets existence"}, "")
		}
	}

	ids, err := srv.db.BulkAddNodes(in.GetNodes())
	if err != nil {
		ids := strings.Join(ids, ",")
		return nil, errorutils.FormatErrorMsg(err, ids)
	}

	// associate node id with manual manager id
	err = srv.db.AssociateNodeIDsWithManagerID(ids, mgrtypes.AutomateManagerID)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}

	return &nodes.Ids{Ids: ids}, nil
}

// Read a node via ID
func (srv *Server) Read(ctx context.Context, in *nodes.Id) (*nodes.Node, error) {
	logrus.Debugf("read node with : %+v", in)
	return GetNode(ctx, in, srv.db, srv.secretsClient)
}

func GetNode(ctx context.Context, in *nodes.Id, db *pgdb.DB, secretsClient secrets.SecretsServiceClient) (*nodes.Node, error) {
	projectsAllowed, err := filterByProjects(ctx)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}

	node, err := db.GetNode(ctx, in.Id)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}

	if !isRequestAllowedForProjects(node, projectsAllowed) {
		return nil, status.Error(codes.NotFound, "Not found for id: "+in.Id)
	}

	secretIds, err := db.GetNodeSecretIds(ctx, in.Id)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}

	secretsAgg, err := getSecretsAgg(ctx, secretIds, secretsClient)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}

	err = resolveInspecConfigWithSecrets(node.TargetConfig, secretsAgg)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}

	return node, nil
}

func resolveInspecConfigWithSecrets(tc *nodes.TargetConfig, secretsMaps []map[string]string) error {
	if tc == nil {
		return nil
	}
	secretsArr := make([]*nodes.NodeSecrets, 0)

	for _, secretsMap := range secretsMaps {
		secret := nodes.NodeSecrets{}
		if secretsMap["username"] != "" {
			secret.User = secretsMap["username"]
		}
		if tc.Backend == "ssh" && secretsMap["username"] != "" {
			if secretsMap["key"] != "" {
				keyPath, err := libSecrets.PrepareSSHPrivateKey(secretsMap["key"])
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
	err := srv.validateNodeUpdate(ctx, in)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}

	secrets := secretsint.New(srv.secretsClient)
	if in.GetTargetConfig() != nil {
		allExist, _, _ := secrets.CheckSecrets(in.GetTargetConfig().GetSecrets())
		if !allExist {
			return nil, errorutils.FormatErrorMsg(&errorutils.InvalidError{Msg: "AddNode unable to confirm secrets existence"}, "")
		}
	}

	err = srv.db.UpdateNode(in)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}

	return &empty, nil
}

// Delete a node
func (srv *Server) Delete(ctx context.Context, in *nodes.Id) (*pb.Empty, error) {
	logrus.Infof("deleting node id: %+v", in)
	_, err := srv.db.DeleteNode(in.Id)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, in.Id)
	}
	return &empty, nil
}

// List nodes based on a query
func (srv *Server) List(ctx context.Context, in *nodes.Query) (*nodes.Nodes, error) {
	logrus.Debugf("Getting Nodes with query: %+v", in)
	filters, err := addProjectFilters(ctx, in.Filters)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
	}

	dbnodes, totalCount, err := srv.db.GetNodes(in.Sort, in.Order, in.Page, in.PerPage, filters)
	if err != nil {
		return nil, errorutils.FormatErrorMsg(err, "")
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
		return nil, errorutils.FormatErrorMsg(err, "")
	}
	return &nodes.BulkDeleteResponse{Names: names}, nil
}

func (srv *Server) validateNodeUpdate(ctx context.Context, in *nodes.Node) error {
	node, err := GetNode(ctx, &nodes.Id{Id: in.Id}, srv.db, srv.secretsClient)
	if err != nil {
		return errors.Wrapf(err, "unable to retrieve node from db %s", in.Id)
	}
	switch node.Manager {
	case "aws-api", "azure-api", "gcp-api":
		return &errorutils.InvalidError{Msg: fmt.Sprintf("invalid option. unable to update %s node", in.Manager)}
	case "", "chef":
		if in.Name != "" && in.Name != node.Name {
			return &errorutils.InvalidError{Msg: fmt.Sprintf("invalid option. unable to update name of ingested node")}
		} else if in.Tags != nil {
			if !equal(in.Tags, node.Tags) {
				return &errorutils.InvalidError{Msg: fmt.Sprintf("invalid option. unable to update tags of ingested node")}
			}
		}
	case "aws-ec2", "azure-vm":
		// the name, host, and tags of these nodes is retrieved from the cloud provider, so let's not let the user update it.
		// users may update other target config information, such as "backend" (ssh/winrm) or the credential id for the node.
		if in.Name != "" && in.Name != node.Name {
			return &errorutils.InvalidError{Msg: fmt.Sprintf("invalid option. unable to update name of %s node", in.Manager)}
		} else if in.GetTargetConfig().GetHost() != "" && in.GetTargetConfig().GetHost() != node.GetTargetConfig().GetHost() {
			return &errorutils.InvalidError{Msg: fmt.Sprintf("invalid option. unable to update host of %s node", in.Manager)}
		} else if !equal(in.Tags, node.Tags) {
			return &errorutils.InvalidError{Msg: fmt.Sprintf("invalid option. unable to update tags of %s node", in.Manager)}
		}
	}
	return nil
}

func equal(a, b []*common.Kv) bool {
	if len(a) != len(b) {
		return false
	}
	for i, v := range a {
		if v != b[i] {
			return false
		}
	}
	return true
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

func filterByProjects(ctx context.Context) ([]string, error) {
	projectsFilter, err := auth_context.ProjectsFromIncomingContext(ctx)
	if err != nil {
		return nil, err
	}
	if auth_context.AllProjectsRequested(projectsFilter) {
		return []string{}, nil
	}

	return projectsFilter, nil
}

func addProjectFilters(ctx context.Context, filters []*common.Filter) ([]*common.Filter, error) {
	projectFilters, err := filterByProjects(ctx)
	if err != nil {
		return nil, err
	}

	if len(projectFilters) == 0 {
		return filters, nil
	}

	return append(filters, &common.Filter{
		Key:    "project",
		Values: projectFilters,
	}), nil
}

func isRequestAllowedForProjects(node *nodes.Node, requestAllowedProjects []string) bool {
	// If the node's Manager is not empty then it is a manually added node, which we do not filter.
	if node.Manager != "" {
		return true
	}

	// If there are no requestAllowedProjects then all nodes are allowed
	if len(requestAllowedProjects) == 0 {
		return true
	}

	// If the node has no projects and the requestAllowedProjects contains the unassigned tag
	if len(node.Projects) == 0 &&
		stringutils.SliceContains(requestAllowedProjects, authzConstants.UnassignedProjectID) {
		return true
	}

	// If one of the node's projects is in the requestAllowedProjects list
	for _, nodeProject := range node.Projects {
		if stringutils.SliceContains(requestAllowedProjects, nodeProject) {
			return true
		}
	}

	return false
}
