package v1

import (
	"context"
	"fmt"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	ver_api "github.com/chef/automate/api/external/common/version"
	"github.com/chef/automate/api/interservice/authz"
	api_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/authz-service/engine"
	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/authz-service/storage/postgres/migration"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/version"
)

// Server is an AuthZ server for storing policies that define AuthZ rules
// and then use those policies to decide if subject is authorized
// for an action on a resource.
type Server struct {
	engine    engine.V1Engine
	logger    logger.Logger
	health    *health.Service
	skipAuthz bool
}

// DefaultEffect is `allow` to ensure user's permissions are the union of their policies
const DefaultEffect = "allow"

// NewPostgresServer instantiates a server.Server that connects to a postgres backend
func NewPostgresServer(
	ctx context.Context,
	l logger.Logger,
	e engine.V1Engine,
	_ migration.Config,
	_ datamigration.Config) (*Server, error) {

	return NewWithStore(ctx, l, e, true, false, nil)
}

func NewWithStore(
	ctx context.Context,
	l logger.Logger,
	e engine.V1Engine,
	_, skipAuthz bool,
	policyV2 api_v2.PoliciesServer) (*Server, error) {

	serv := &Server{
		engine:    e,
		logger:    l,
		health:    health.NewService(),
		skipAuthz: skipAuthz,
	}

	return serv, nil
}

// GetVersion returns the version of Authz GRPC API
func (s *Server) GetVersion(
	ctx context.Context,
	req *ver_api.VersionInfoRequest) (*ver_api.VersionInfo, error) {
	return &ver_api.VersionInfo{
		Name:    constants.ServiceName,
		Version: version.Version,
		Sha:     version.GitSHA,
		Built:   version.BuildTime,
	}, nil
}

// IsAuthorized checks if an API request is authorized
func (s *Server) IsAuthorized(
	ctx context.Context,
	req *authz.IsAuthorizedReq) (*authz.IsAuthorizedResp, error) {

	authorized, err := s.engine.IsAuthorized(ctx,
		engine.Subjects(req.Subjects), engine.Action(req.Action), engine.Resource(req.Resource))
	s.logQuery(req, authorized, err)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &authz.IsAuthorizedResp{
		Authorized: authorized,
	}, nil
}

// FilterAuthorizedPairs implements the GRPC method for filtering pairs of
// resources/actions
func (s *Server) FilterAuthorizedPairs(
	ctx context.Context,
	req *authz.FilterAuthorizedPairsReq) (*authz.FilterAuthorizedPairsResp, error) {

	resp, err := s.engine.FilterAuthorizedPairs(ctx,
		engine.Subjects(req.Subjects),
		toEnginePairs(req.Pairs))
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &authz.FilterAuthorizedPairsResp{
		Pairs: toPBPairs(resp),
	}, nil
}

func toPBPairs(pairs []engine.Pair) []*authz.Pair {
	ps := make([]*authz.Pair, len(pairs))
	for i, p := range pairs {
		ps[i] = &authz.Pair{Resource: string(p.Resource), Action: string(p.Action)}
	}
	return ps
}

func toEnginePairs(pairs []*authz.Pair) []engine.Pair {
	ps := make([]engine.Pair, len(pairs))
	for i, p := range pairs {
		ps[i] = engine.Pair{Resource: engine.Resource(p.Resource), Action: engine.Action(p.Action)}
	}
	return ps
}

func unexpectedError(err error) error {
	return status.Error(codes.Internal, err.Error())
}

func (s *Server) logQuery(req *authz.IsAuthorizedReq, authorized bool, err error) {
	result := fmt.Sprintf("%t", authorized)
	if err != nil {
		result = err.Error()
	}
	s.logger.WithFields(logger.KV{
		"result":   result,
		"subject":  req.Subjects,
		"action":   req.Action,
		"resource": req.Resource,
	}).Info("Authorization Query")
}
