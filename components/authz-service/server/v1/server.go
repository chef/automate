package v1

import (
	"context"
	"fmt"
	"time"

	"github.com/golang/protobuf/ptypes"
	"github.com/golang/protobuf/ptypes/timestamp"
	"github.com/pkg/errors"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	ver_api "github.com/chef/automate/api/external/common/version"
	"github.com/chef/automate/api/interservice/authz"
	api_v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/authz-service/constants"
	"github.com/chef/automate/components/authz-service/engine"
	storage_errors "github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/authz-service/storage/postgres/migration"
	storage "github.com/chef/automate/components/authz-service/storage/v1"
	memstore_v1 "github.com/chef/automate/components/authz-service/storage/v1/memstore"
	postgres_v1 "github.com/chef/automate/components/authz-service/storage/v1/postgres"
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
	storage   storage.Storage
	health    *health.Service
	skipAuthz bool
}

// DefaultEffect is `allow` to ensure user's permissions are the union of their policies
const DefaultEffect = "allow"

// NewMemstoreServer returns an instance of server.Server
func NewMemstoreServer(
	ctx context.Context,
	l logger.Logger,
	e engine.V1Engine) (*Server, error) {
	mem, err := memstore_v1.New(l)
	if err != nil {
		return nil, errors.Wrapf(err, "init memstore")
	}

	return NewWithStore(ctx, l, e, mem, true, false, nil)
}

// NewPostgresServer instantiates a server.Server that connects to a postgres backend
// NewPostgresServer instantiates a server.Server that connects to a postgres backend
func NewPostgresServer(
	ctx context.Context,
	l logger.Logger,
	e engine.V1Engine,
	migrationsConfig migration.Config,
	datamigrationsConfig datamigration.Config) (*Server, error) {

	p, err := postgres_v1.New(ctx, l, migrationsConfig, datamigrationsConfig)
	if err != nil {
		return nil, errors.Wrapf(err, "init postgres for IAM v1")
	}

	return NewWithStore(ctx, l, e, p, true, false, nil)
}

// NewWithStore instantiates a new server with a given storage.Storage implementation
func NewWithStore(
	ctx context.Context,
	l logger.Logger,
	e engine.V1Engine,
	s storage.Storage,
	initPolicies, skipAuthz bool,
	policyV2 api_v2.PoliciesServer) (*Server, error) {

	serv := &Server{
		engine:    e,
		logger:    l,
		storage:   s,
		health:    health.NewService(),
		skipAuthz: skipAuthz,
	}

	if initPolicies {
		if err := serv.updateEngineStore(ctx); err != nil {
			return nil, unexpectedError(err)
		}
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

// CreatePolicy allows the user to persist a policy that informs AuthZ rules.
func (s *Server) CreatePolicy(
	ctx context.Context,
	req *authz.CreatePolicyReq) (*authz.CreatePolicyResp, error) {

	var policy *storage.Policy
	var err error
	if policy, err = s.storage.StorePolicy(ctx, req.Action, req.Subjects, req.Resource, DefaultEffect); err != nil {
		return nil, unexpectedError(err)
	}

	if err = s.updateEngineStore(ctx); err != nil {
		return nil, unexpectedError(err)
	}

	pol, err := fromStorage(policy)
	if err != nil {
		return nil, unexpectedError(err)
	}
	return &authz.CreatePolicyResp{
		Policy: pol,
	}, nil
}

// ListPolicies returns a list of all policies from storage.
func (s *Server) ListPolicies(
	ctx context.Context,
	req *authz.ListPoliciesReq) (*authz.ListPoliciesResp, error) {

	var policies []*storage.Policy
	var err error
	if policies, err = s.storage.ListPolicies(ctx); err != nil {
		return nil, unexpectedError(err)
	}

	pols, err := fromStorageToList(policies)
	if err != nil {
		return nil, unexpectedError(err)
	}

	return &authz.ListPoliciesResp{
		Policies: pols,
	}, nil
}

// DeletePolicy removes a policy from storage by id.
func (s *Server) DeletePolicy(
	ctx context.Context,
	req *authz.DeletePolicyReq) (*authz.DeletePolicyResp, error) {

	var policy *storage.Policy
	var err error
	if policy, err = s.storage.DeletePolicy(ctx, req.Id); err != nil {
		switch err {
		case storage_errors.ErrNotFound:
			return nil, status.Errorf(codes.NotFound, "no policy with id %q found to delete", req.Id)
		case storage_errors.ErrCannotDelete:
			return nil, status.Errorf(codes.InvalidArgument,
				"policy with id %q is marked as non-deletable and cannot be removed", req.Id)
		}
		return nil, unexpectedError(err)
	}

	if err = s.updateEngineStore(ctx); err != nil {
		return nil, unexpectedError(err)
	}

	pol, err := fromStorage(policy)
	if err != nil {
		return nil, unexpectedError(err)
	}
	return &authz.DeletePolicyResp{
		Policy: pol,
	}, nil
}

func (s *Server) PurgeSubjectFromPolicies(
	ctx context.Context,
	req *authz.PurgeSubjectFromPoliciesReq) (*authz.PurgeSubjectFromPoliciesResp, error) {
	policies, err := s.storage.PurgeSubjectFromPolicies(ctx, req.Subject)
	if err != nil {
		return nil, unexpectedError(err)
	}

	err = s.updateEngineStore(ctx)
	if err != nil {
		return nil, unexpectedError(err)
	}

	polIDs := make([]string, len(policies))
	for i := range policies {
		polIDs[i] = policies[i].String()
	}

	return &authz.PurgeSubjectFromPoliciesResp{Ids: polIDs}, nil
}

func fromStorage(s *storage.Policy) (*authz.Policy, error) {
	createdAt, err := ptypes.TimestampProto(s.CreatedAt)
	if err != nil {
		return nil, errors.Wrapf(err, "could not convert policy with ID %q from database.", s.ID)
	}

	var updatedAt *timestamp.Timestamp
	unset := time.Time{}
	if s.UpdatedAt != unset {
		updatedAt, err = ptypes.TimestampProto(s.UpdatedAt)
		if err != nil {
			return nil, errors.Wrapf(err, "could not convert policy with ID %q from database.", s.ID)
		}
	}
	p := authz.Policy{
		Action:    s.Action,
		Id:        s.ID.String(),
		Resource:  s.Resource,
		Subjects:  s.Subjects,
		Effect:    s.Effect,
		CreatedAt: createdAt,
		UpdatedAt: updatedAt,
	}
	return &p, nil
}

func fromStorageToList(sl []*storage.Policy) ([]*authz.Policy, error) {
	tl := make([]*authz.Policy, len(sl))

	for i, policy := range sl {
		pol, err := fromStorage(policy)
		if err != nil {
			return nil, err
		}
		tl[i] = pol
	}

	return tl, nil
}

func unexpectedError(err error) error {
	return status.Error(codes.Internal, err.Error())
}

// updates OPA engine store with policy
func (s *Server) updateEngineStore(ctx context.Context) error {
	var policies []*storage.Policy
	var err error
	// TODO: ListPolicies doesn't yet return an err but should/will
	// This method should probably fail out the whole call (delete/create)
	if policies, err = s.storage.ListPolicies(ctx); err != nil {
		return err
	}

	s.logger.Infof("initializing OPA store with %d V1 policies", len(policies))
	policies = append(policies, SystemPolicies()...)

	// OPA requires this format
	input := make(map[string]interface{})
	for _, p := range policies {
		input[p.ID.String()] = map[string]interface{}{
			"subjects": p.Subjects,
			"action":   p.Action,
			"resource": p.Resource,
			"effect":   p.Effect,
		}
	}

	return s.engine.SetPolicies(ctx, input)
}

// SystemPolicies returns a list of system policies that should always exist by default.
// These should not be visible to the enduser and therefore exist outside of the database.
func SystemPolicies() []*storage.Policy {
	return []*storage.Policy{
		// Grant the deployment-service universal access (used by
		// automate-cli for API requests through the gateway).
		{
			Action:   "*",
			Effect:   "allow",
			Resource: "*",
			Subjects: []string{"tls:service:deployment-service:*"},
		},
	}
}

func (s *Server) logPolicies(policies []*storage.Policy) {
	kv := logger.KV{}
	for _, p := range policies {
		kv[p.ID.String()] = logger.KV{
			"subjects": p.Subjects,
			"action":   p.Action,
			"resource": p.Resource,
			"effect":   p.Effect,
		}
	}
	s.logger.WithFields(kv).Info("Policy definition")
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

// Storage exposes the (v1) server's storage backend, and is used for migrating
// v1 policies from there
func (s *Server) Storage() storage.Storage {
	return s.storage
}
