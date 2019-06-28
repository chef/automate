package v2

import (
	"context"
	"fmt"
	"strings"

	"github.com/pkg/errors"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/lib/logger"

	api "github.com/chef/automate/api/interservice/authz/v2"
	constants "github.com/chef/automate/components/authz-service/constants/v2"
	"github.com/chef/automate/components/authz-service/engine"
	storage_errors "github.com/chef/automate/components/authz-service/storage"
	"github.com/chef/automate/components/authz-service/storage/postgres/datamigration"
	"github.com/chef/automate/components/authz-service/storage/postgres/migration"
	storage_v1 "github.com/chef/automate/components/authz-service/storage/v1"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
	"github.com/chef/automate/components/authz-service/storage/v2/memstore"
	"github.com/chef/automate/components/authz-service/storage/v2/postgres"
)

// policyServer is the server state for policies
type policyServer struct {
	log             logger.Logger
	store           storage.Storage
	engine          engine.V2pXWriter
	v1              storage_v1.PoliciesLister
	vChan           chan api.Version
	policyRefresher PolicyRefresher
}

// PolicyServer is the server interface for policies: what we defined via
// protobuf + the update interceptor
type PolicyServer interface {
	api.PoliciesServer

	EngineUpdateInterceptor() grpc.UnaryServerInterceptor
}

// NewMemstorePolicyServer returns an instance of server.Server
func NewMemstorePolicyServer(
	ctx context.Context,
	l logger.Logger,
	e engine.V2pXWriter,
	pl storage_v1.PoliciesLister,
	vChan chan api.Version) (PolicyServer, PolicyRefresher, error) {

	return NewPoliciesServer(ctx, l, memstore.New(), e, pl, vChan)
}

// NewPostgresPolicyServer instantiates a server.Server that connects to a postgres backend
func NewPostgresPolicyServer(
	ctx context.Context,
	l logger.Logger,
	e engine.V2pXWriter,
	migrationsConfig migration.Config,
	dataMigrationsConfig datamigration.Config,
	pl storage_v1.PoliciesLister,
	vChan chan api.Version) (PolicyServer, PolicyRefresher, error) {

	s, err := postgres.New(ctx, l, migrationsConfig, dataMigrationsConfig)
	if err != nil {
		return nil, nil, errors.Wrap(err, "failed to initialize v2 store state")
	}
	return NewPoliciesServer(ctx, l, s, e, pl, vChan)
}

// NewPoliciesServer returns a new IAM v2 Policy server.
func NewPoliciesServer(
	ctx context.Context,
	l logger.Logger,
	s storage.Storage,
	e engine.V2pXWriter,
	pl storage_v1.PoliciesLister,
	vChan chan api.Version) (PolicyServer, PolicyRefresher, error) {

	policyRefresher, err := NewPolicyRefresher(ctx, l, s, e)
	if err != nil {
		return nil, nil, errors.Wrap(err, "start policy refresher")
	}

	srv := &policyServer{
		log:             l,
		store:           s,
		engine:          e,
		v1:              pl,
		vChan:           vChan,
		policyRefresher: policyRefresher,
	}

	// If we *could* transition to failure, it means we had an in-progress state
	// on service startup.
	if s.Failure(ctx) == nil {
		l.Warn("cleaned up in-progress migration status")
	}

	// check migration status
	ms, err := srv.store.MigrationStatus(ctx)
	if err != nil {
		return nil, nil, errors.Wrap(err, "retrieve migration status from storage")
	}
	var v api.Version
	switch ms {
	case storage.SuccessfulBeta1:
		v = api.Version{Major: api.Version_V2, Minor: api.Version_V1}
	case storage.Successful:
		v = api.Version{Major: api.Version_V2, Minor: api.Version_V0}
	default:
		v = api.Version{Major: api.Version_V1, Minor: api.Version_V0}
	}
	srv.setVersionForInterceptorSwitch(v)

	if v.Major == api.Version_V2 {
		if err := srv.store.ApplyV2DataMigrations(ctx); err != nil {
			return nil, nil, errors.Wrap(err, "error migrating v2 data")
		}
	}

	// now that the data is all set, attempt to feed it into OPA:
	if err := srv.updateEngineStore(ctx); err != nil {
		return nil, nil, errors.Wrapf(err, "initialize engine storage (%v)", v)
	}

	return srv, policyRefresher, nil
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * * POLICIES  * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

// CreatePolicy creates an IAM v2 policy.
func (s *policyServer) CreatePolicy(
	ctx context.Context,
	req *api.CreatePolicyReq) (*api.Policy, error) {

	// API requests always create custom policies.

	pol, err := policyFromAPI(
		req.Id,
		req.Name,
		storage.Custom,
		req.Members,
		req.Statements,
		req.Projects)

	if err != nil {
		if errors.Cause(err) == storage_errors.ErrGenerateUUID {
			return nil, status.Errorf(codes.Internal,
				"error generating UUID for policy database entry %q: %s", req.Id, err.Error())
		}
		return nil, status.Errorf(codes.InvalidArgument,
			"error parsing policy %q: %s", req.Id, err.Error())
	}

	returnPol, err := s.store.CreatePolicy(ctx, &pol)
	switch err {
	case nil: // continue
	case storage_errors.ErrConflict:
		return nil, status.Errorf(codes.AlreadyExists,
			"policy with id %q already exists", req.Id)
	default:
		return nil, status.Errorf(codes.Internal,
			"creating policy %q: %s", req.Id, err.Error())
	}

	return policyFromInternal(returnPol)
}

// PurgeSubjectFromPolicies removes a given subject from all policies.
func (s *policyServer) PurgeSubjectFromPolicies(ctx context.Context,
	req *api.PurgeSubjectFromPoliciesReq) (*api.PurgeSubjectFromPoliciesResp, error) {
	affected, err := s.store.PurgeSubjectFromPolicies(ctx, req.Subject)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "failed to purge subject %q: %s", req.Subject, err.Error())
	}
	return &api.PurgeSubjectFromPoliciesResp{Ids: affected}, nil
}

// ListPolicies fetches a list of all IAM v2 policies.
func (s *policyServer) ListPolicies(ctx context.Context,
	_ *api.ListPoliciesReq) (*api.ListPoliciesResp, error) {

	pols, err := s.store.ListPolicies(ctx)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "error listing policies: %s", err.Error())
	}
	resp := api.ListPoliciesResp{}
	for _, polInternal := range pols {
		pol, err := policyFromInternal(polInternal)
		if err != nil {
			return nil, status.Errorf(codes.Internal, "error converting policy %q: %s", polInternal.Name, err.Error())
		}
		resp.Policies = append(resp.Policies, pol)
	}
	return &resp, nil
}

// GetPolicy fetches an IAM v2 policy.
func (s *policyServer) GetPolicy(
	ctx context.Context,
	req *api.GetPolicyReq) (*api.Policy, error) {

	polInternal, err := s.store.GetPolicy(ctx, req.Id)
	switch err {
	case nil: // continue
	case storage_errors.ErrNotFound:
		return nil, status.Errorf(codes.NotFound, "no policy with ID %q found", req.Id)
	default:
		return nil, status.Errorf(codes.Internal, "error retrieving policy with ID %q: %s", req.Id, err.Error())
	}

	return policyFromInternal(polInternal)
}

// DeletePolicy removes an IAM v2 policy from the data store.
func (s *policyServer) DeletePolicy(
	ctx context.Context,
	req *api.DeletePolicyReq) (*api.DeletePolicyResp, error) {

	err := s.store.DeletePolicy(ctx, req.Id)
	switch err {
	case nil:
		return &api.DeletePolicyResp{}, nil
	case storage_errors.ErrNotFound:
		return nil, status.Errorf(codes.NotFound, "no policy with ID %q found", req.Id)
	default: // some other error
		return nil, status.Errorf(codes.Internal, "error deleting policy with ID %q: %s", req.Id, err.Error())
	}
}

// UpdatePolicy modifies properties of an IAM v2 policy.
// All properties must be supplied, whether changed or not.
func (s *policyServer) UpdatePolicy(
	ctx context.Context,
	req *api.UpdatePolicyReq) (*api.Policy, error) {

	statements := make([]storage.Statement, len(req.Statements))
	for i, statement := range req.Statements {
		statementInt, err := statementFromAPI(statement)
		if err != nil {
			return nil, status.Errorf(codes.InvalidArgument, "parse statement: %s", err.Error())
		}
		statements[i] = statementInt
	}

	members, err := membersFromAPI(req.Members)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "parse member: %s", err.Error())
	}

	// Assume custom policy for API requests.
	storagePolicy, err := storage.NewPolicy(req.Id,
		req.Name, storage.Custom, members, statements, req.Projects)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "parse policy with ID %q: %s", req.Id, err.Error())
	}

	polInternal, err := s.store.UpdatePolicy(ctx, &storagePolicy)
	if err != nil {
		switch err {
		case storage_errors.ErrConflict:
			return nil, status.Errorf(codes.AlreadyExists, "policy with name %q already exists", req.Name)
		case storage_errors.ErrNotFound:
			return nil, status.Errorf(codes.NotFound, "no policy with ID %q found", req.Id)
		}
		return nil, err
	}

	return policyFromInternal(polInternal)
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * *  MEMBERS  * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

// ListPolicyMembers fetches an IAM v2 policy's membership list.
func (s *policyServer) ListPolicyMembers(
	ctx context.Context,
	req *api.ListPolicyMembersReq) (*api.ListPolicyMembersResp, error) {

	membersInternal, err := s.store.ListPolicyMembers(ctx, req.Id)
	switch err {
	case nil: // continue
	case storage_errors.ErrNotFound:
		return nil, status.Errorf(codes.NotFound, "no policy with ID %q found", req.Id)
	default:
		return nil, status.Errorf(codes.Internal, "error retrieving policy with ID %q: %s", req.Id, err.Error())
	}

	return &api.ListPolicyMembersResp{
		Members: storage.MemberSliceToStringSlice(membersInternal),
	}, nil
}

// AddPolicyMembers takes in a list of members and adds them to the specified policy.
func (s *policyServer) AddPolicyMembers(
	ctx context.Context,
	req *api.AddPolicyMembersReq) (*api.AddPolicyMembersResp, error) {

	members, err := membersFromAPI(req.Members)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "parse member: %s", err.Error())
	}

	resp, err := s.store.AddPolicyMembers(ctx, req.Id, members)
	switch err {
	case nil:
		return &api.AddPolicyMembersResp{
			Members: storage.MemberSliceToStringSlice(resp),
		}, nil
	case storage_errors.ErrNotFound:
		return nil, status.Errorf(codes.NotFound, "no policy with ID %q found", req.Id)
	default: // some other error
		return nil, status.Errorf(codes.Internal,
			"error adding members to policy with ID %q: %s", req.Id, err.Error())
	}
}

// ReplacePolicyMembers takes in a new list of policy members and completely replaces
// all policy members for given policy with new list.
func (s *policyServer) ReplacePolicyMembers(
	ctx context.Context,
	req *api.ReplacePolicyMembersReq) (*api.ReplacePolicyMembersResp, error) {

	members, err := membersFromAPI(req.Members)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "parse member: %s", err.Error())
	}

	resp, err := s.store.ReplacePolicyMembers(ctx, req.Id, members)
	switch err {
	case nil:
		return &api.ReplacePolicyMembersResp{
			Members: storage.MemberSliceToStringSlice(resp),
		}, nil
	case storage_errors.ErrNotFound:
		return nil, status.Errorf(codes.NotFound, "no policy with ID %q found", req.Id)
	default: // some other error
		return nil, status.Errorf(codes.Internal,
			"error replacing members on policy with ID %q: %s", req.Id, err.Error())
	}
}

// RemovePolicyMembers takes in a list of members and removes any
// that are currently members of the policy.
func (s *policyServer) RemovePolicyMembers(ctx context.Context,
	req *api.RemovePolicyMembersReq) (*api.RemovePolicyMembersResp, error) {

	members, err := membersFromAPI(req.Members)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "parse member: %s", err.Error())
	}

	// TODO replace this check with a policy once we've got RemovePolicyMember
	if req.Id == constants.AdminPolicyID {
		for _, member := range members {
			if member.Name == "team:local:admins" {
				return nil, status.Error(codes.PermissionDenied, `cannot remove local team: 
				admins from Chef-managed policy: Admin`)
			}
		}
	}

	resp, err := s.store.RemovePolicyMembers(ctx, req.Id, members)
	switch err {
	case nil: // continue
	case storage_errors.ErrNotFound:
		return nil, status.Errorf(codes.NotFound, "no policy with ID %q found", req.Id)
	default: // some other error
		return nil, status.Errorf(codes.Internal,
			"error removing members on policy with ID %q: %s", req.Id, err.Error())
	}

	return &api.RemovePolicyMembersResp{
		Members: storage.MemberSliceToStringSlice(resp),
	}, nil

}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * *   ROLES   * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

// CreateRole creates an IAM v2 role.
func (s *policyServer) CreateRole(
	ctx context.Context,
	req *api.CreateRoleReq) (*api.Role, error) {
	storageRole, err := storage.NewRole(req.Id, req.Name, storage.Custom, req.Actions, req.Projects)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "error parsing role %q: %s", req.Id, err.Error())
	}

	returnRole, err := s.store.CreateRole(ctx, storageRole)
	switch err {
	case nil: // continue
	case storage_errors.ErrConflict:
		return nil, status.Errorf(codes.AlreadyExists, "role with id %q already exists", req.Id)
	case storage_errors.ErrForeignKey:
		return nil, status.Errorf(codes.NotFound, "could not create role with projects %s as "+
			"some projects were not found", req.Projects)
	default:
		return nil, status.Errorf(codes.Internal, "creating role %q: %s", req.Id, err.Error())
	}

	return roleFromInternal(returnRole)
}

// ListRoles fetches a list of all IAM v2 roles.
func (s *policyServer) ListRoles(ctx context.Context,
	_ *api.ListRolesReq) (*api.ListRolesResp, error) {

	internalRoles, err := s.store.ListRoles(ctx)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "error listing roles: %s", err.Error())
	}

	roles := make([]*api.Role, len(internalRoles))

	for index, internalRole := range internalRoles {
		role, err := roleFromInternal(internalRole)
		if err != nil {
			return nil, status.Errorf(codes.Internal, "error converting role %q: %s", internalRole.Name, err.Error())
		}
		roles[index] = role
	}

	resp := api.ListRolesResp{
		Roles: roles,
	}

	return &resp, nil
}

// GetRole fetches an IAM v2 role.
func (s *policyServer) GetRole(
	ctx context.Context,
	req *api.GetRoleReq) (*api.Role, error) {

	roleInternal, err := s.store.GetRole(ctx, req.Id)
	switch err {
	case nil:
		return roleFromInternal(roleInternal)
	case storage_errors.ErrNotFound:
		return nil, status.Errorf(codes.NotFound, "no role with ID %q found", req.Id)
	default:
		return nil, status.Errorf(codes.Internal, "error retrieving role with ID %q: %s", req.Id, err.Error())
	}
}

// DeleteRole removes an IAM v2 role from the data store.
func (s *policyServer) DeleteRole(
	ctx context.Context,
	req *api.DeleteRoleReq) (*api.DeleteRoleResp, error) {

	err := s.store.DeleteRole(ctx, req.Id)
	switch err {
	case nil:
		return &api.DeleteRoleResp{}, nil
	case storage_errors.ErrNotFound:
		return nil, status.Errorf(codes.NotFound, "no role with ID %q found", req.Id)
	default: // some other error
		return nil, status.Errorf(codes.Internal, "error deleting role with ID %q: %s", req.Id, err.Error())
	}
}

// UpdateRole modifies properties of an IAM v2 role.
// All properties must be supplied, whether changed or not.
func (s *policyServer) UpdateRole(
	ctx context.Context,
	req *api.UpdateRoleReq) (*api.Role, error) {

	storageRole, err := storage.NewUpdateRole(req.Id, req.Name, req.Actions, req.Projects)
	if err != nil {
		return nil, status.Errorf(codes.InvalidArgument, "parse policy with ID %q: %s", req.Id, err.Error())
	}

	roleInternal, err := s.store.UpdateRole(ctx, storageRole)
	if err != nil {
		switch err {
		case storage_errors.ErrConflict:
			return nil, status.Errorf(codes.AlreadyExists, "role with name %q already exists", req.Name)
		case storage_errors.ErrNotFound:
			return nil, status.Errorf(codes.NotFound, "no role with ID %q found", req.Id)
		}
		return nil, err
	}

	return roleFromInternal(roleInternal)
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * *   MIGRATION   * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

// MigrateToV2 sets the V2 store to its factory defaults and then migrates
// any existing V1 policies.
func (s *policyServer) MigrateToV2(ctx context.Context,
	req *api.MigrateToV2Req) (*api.MigrateToV2Resp, error) {
	ms, err := s.store.MigrationStatus(ctx)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "retrieve migration status: %s", err.Error())
	}

	// the 2.1 flag is not related to migration or major version;
	// it acts as a feature flag around project authz, so no need to migrate
	// if we're already on some version of v2
	upgraded, err := s.handleMinorUpgrade(ctx, ms, req.Flag)
	if err != nil {
		return nil, err
	}
	if upgraded {
		return &api.MigrateToV2Resp{}, nil
	}

	if err := s.okToMigrate(ctx, ms); err != nil {
		return nil, err
	}

	if err := s.store.InProgress(ctx); err != nil {
		return nil, status.Errorf(codes.Internal, "record migration status: %s", err.Error())
	}

	defaultPolicies, err := storage.DefaultPolicies()
	if err != nil {
		return nil, status.Errorf(codes.Internal, "retrieve default policies: %s", err.Error())
	}

	for _, pol := range defaultPolicies {
		if _, err := s.store.CreatePolicy(ctx, &pol); err != nil {
			return nil, status.Errorf(codes.Internal, "reset to default policies: %s", err.Error())
		}
	}

	for _, role := range storage.DefaultRoles() {
		if _, err := s.store.CreateRole(ctx, &role); err != nil {
			return nil, status.Errorf(codes.Internal, "reset to default roles: %s", err.Error())
		}
	}

	// Added for testing only; these are handled by data migrations.
	for _, project := range storage.DefaultProjects() {
		if _, err := s.store.CreateProject(ctx, &project); err != nil {
			return nil, status.Errorf(codes.Internal, "reset to default project: %s", err.Error())
		}
	}

	recordFailure := func() {
		// This should be unlikely, and it doesn't affect our returned error, which,
		// in any case, is the more interesting error -- so, we merely log it.
		if err := s.store.Failure(ctx); err != nil {
			s.log.Errorf("failed to record migration failure status: %s", err)
		}
	}

	var reports []string
	if !req.SkipV1Policies {
		errs, err := s.migrateV1Policies(ctx)
		if err != nil {
			recordFailure()
			return nil, status.Errorf(codes.Internal, "migrate v1 policies: %s", err.Error())
		}
		for _, e := range errs {
			reports = append(reports, e.Error())
		}
	} else {
		// Note 2019/05/22 (sr): policies without subjects are silently ignored -- this
		// is to be in line with the migration case, that does the same. However, this
		// could be worth revisiting?
		pols, err := s.v1.ListPoliciesWithSubjects(ctx)
		if err != nil {
			recordFailure()
			return nil, status.Errorf(codes.Internal, "list v1 policies: %s", err.Error())
		}
		reports = append(reports, fmt.Sprintf("%d v1 policies", len(pols)))

	}

	err = s.store.ApplyV2DataMigrations(ctx)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "apply v2 data migrations: %s", err.Error())
	}

	// we've made it!
	var v api.Version
	switch req.Flag {
	case api.Flag_VERSION_2_1:
		err = s.store.SuccessBeta1(ctx)
		v = api.Version{Major: api.Version_V2, Minor: api.Version_V1}
	default:
		err = s.store.Success(ctx)
		v = api.Version{Major: api.Version_V2, Minor: api.Version_V0}
	}
	if err != nil {
		recordFailure()
		return nil, status.Errorf(codes.Internal, "record migration status: %s", err.Error())
	}

	s.setVersionForInterceptorSwitch(v)
	return &api.MigrateToV2Resp{Reports: reports}, nil
}

func (s *policyServer) handleMinorUpgrade(ctx context.Context, ms storage.MigrationStatus, f api.Flag) (upgraded bool, err error) {
	var version api.Version
	upgraded = true
	if f == api.Flag_VERSION_2_1 && ms == storage.Successful {
		err = s.store.SuccessBeta1(ctx)
		version = api.Version{Major: api.Version_V2, Minor: api.Version_V1}
	} else if f == api.Flag_VERSION_2_0 && ms == storage.SuccessfulBeta1 {
		err = s.store.Success(ctx)
		version = api.Version{Major: api.Version_V2, Minor: api.Version_V0}
	} else {
		upgraded = false
	}

	if err != nil {
		return false, status.Errorf(codes.Internal, "record migration status: %s", err.Error())
	}

	if upgraded {
		s.setVersionForInterceptorSwitch(version)
	}
	return upgraded, nil
}

// ResetToV1 will mark the migration status as "pristine", which means a
// following MigrateToV2 call will be accepted.
func (s *policyServer) ResetToV1(ctx context.Context,
	req *api.ResetToV1Req) (*api.ResetToV1Resp, error) {

	ms, err := s.store.MigrationStatus(ctx)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "retrieve migration status: %s", err.Error())
	}
	switch ms {
	case storage.Pristine: // skip
	case storage.InProgress:
		return nil, status.Error(codes.FailedPrecondition, "migration in progress")
	case storage.Successful, storage.SuccessfulBeta1, storage.Failed:
		err := s.store.Pristine(ctx)
		if err != nil {
			return nil, status.Errorf(codes.Internal, "record migration status: %s", err.Error())
		}
	}
	if err := s.store.Reset(ctx); err != nil {
		return nil, status.Errorf(codes.Internal, "reset database state: %s", err.Error())
	}
	s.setVersionForInterceptorSwitch(api.Version{Major: api.Version_V1, Minor: api.Version_V0})
	return &api.ResetToV1Resp{}, nil
}

// GetPolicyVersion returns the status of the data store.
func (s *policyServer) GetPolicyVersion(ctx context.Context,
	req *api.GetPolicyVersionReq) (*api.GetPolicyVersionResp, error) {
	ms, err := s.store.MigrationStatus(ctx)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "retrieve migration status: %s", err.Error())
	}
	return &api.GetPolicyVersionResp{
		Version: versionFromInternal(ms),
	}, nil
}

// EngineUpdateInterceptor is a middleware for updating the V2 engine when a
// certain set of methods has been executed successfully.
func (s *policyServer) EngineUpdateInterceptor() grpc.UnaryServerInterceptor {
	return func(ctx context.Context,
		req interface{},
		info *grpc.UnaryServerInfo,
		handler grpc.UnaryHandler) (interface{}, error) {
		resp, err := handler(ctx, req)
		if err != nil {
			return nil, err
		}

		// ignore anything not related to the OPA store.
		if !strings.HasPrefix(info.FullMethod, "/chef.automate.domain.authz.v2.Policies/") &&
			!strings.HasPrefix(info.FullMethod, "/chef.automate.domain.authz.v2.Projects/") {
			return resp, nil
		}

		switch info.FullMethod {
		// Important! Any new endpoint that requires refreshing the OPA cache must be added here.
		case "/chef.automate.domain.authz.v2.Policies/ReplacePolicyMembers",
			"/chef.automate.domain.authz.v2.Policies/CreatePolicy",
			"/chef.automate.domain.authz.v2.Policies/DeletePolicy",
			"/chef.automate.domain.authz.v2.Policies/UpdatePolicy",
			"/chef.automate.domain.authz.v2.Policies/MigrateToV2",
			"/chef.automate.domain.authz.v2.Policies/ResetToV1",
			"/chef.automate.domain.authz.v2.Policies/CreateRole",
			"/chef.automate.domain.authz.v2.Policies/DeleteRole",
			"/chef.automate.domain.authz.v2.Policies/UpdateRole",
			"/chef.automate.domain.authz.v2.Policies/RemovePolicyMembers",
			"/chef.automate.domain.authz.v2.Policies/AddPolicyMembers",
			"/chef.automate.domain.authz.v2.Policies/PurgeSubjectFromPolicies",
			"/chef.automate.domain.authz.v2.Projects/CreateRule",
			"/chef.automate.domain.authz.v2.Projects/UpdateRule",
			"/chef.automate.domain.authz.v2.Projects/DeleteRule":
			if err := s.updateEngineStore(ctx); err != nil {
				return nil, status.Errorf(codes.Internal, "error updating engine store: %s", err.Error())
			}
		default:
			// do nothing
		}

		return resp, nil
	}
}

func (s *policyServer) updateEngineStore(ctx context.Context) error {
	return s.policyRefresher.Refresh(ctx)
}

/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * *  CONVERTERS   * * * * * * * * * * * * * * * * * * */
/* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * */

func policyFromAPI(ID, name string, typeVal storage.Type,
	membersToAttach []string, statementsToAttach []*api.Statement, inProjects []string) (storage.Policy, error) {

	statements := make([]storage.Statement, len(statementsToAttach))
	for i, statement := range statementsToAttach {
		statementInt, err := statementFromAPI(statement)
		if err != nil {
			return storage.Policy{}, errors.Wrap(err, "parse statement")
		}
		statements[i] = statementInt
	}

	members := make([]storage.Member, len(membersToAttach))
	for i, member := range membersToAttach {
		memberInt, err := storage.NewMember(member)
		if err != nil {
			return storage.Policy{}, errors.Wrap(err, "parse statement")
		}
		members[i] = memberInt
	}

	return storage.NewPolicy(ID, name, typeVal, members, statements, inProjects)
}

func policyFromInternal(pol *storage.Policy) (*api.Policy, error) {
	resp := &api.Policy{
		Id:         pol.ID,
		Name:       pol.Name,
		Type:       typeFromInternal(pol.Type),
		Members:    storage.MemberSliceToStringSlice(pol.Members),
		Statements: statementsFromInternal(pol.Statements),
		Projects:   pol.Projects,
	}

	return resp, nil
}

func typeFromInternal(t storage.Type) api.Type {
	var ret api.Type
	switch t {
	case storage.Custom:
		ret = api.Type_CUSTOM
	case storage.ChefManaged:
		ret = api.Type_CHEF_MANAGED
	}

	return ret
}

func statementsFromInternal(internal []storage.Statement) []*api.Statement {
	resp := make([]*api.Statement, len(internal))
	for i, statement := range internal {
		projects := make([]string, len(statement.Projects))
		for i, project := range statement.Projects {
			if project == constants.AllProjectsID {
				projects[i] = constants.AllProjectsExternalID
			} else {
				projects[i] = project
			}
		}

		resp[i] = &api.Statement{
			Effect:    effectFromInternal(statement.Effect),
			Role:      statement.Role,
			Projects:  projects,
			Actions:   statement.Actions,
			Resources: statement.Resources,
		}
	}

	return resp
}

func effectFromInternal(internal storage.Effect) api.Statement_Effect {
	switch internal {
	case storage.Allow:
		return api.Statement_ALLOW
	case storage.Deny:
		return api.Statement_DENY
	default:
		panic("effect must always be one of allow or deny")
	}
}

func roleFromInternal(role *storage.Role) (*api.Role, error) {
	resp := &api.Role{
		Id:       role.ID,
		Name:     role.Name,
		Type:     typeFromInternal(role.Type),
		Actions:  role.Actions,
		Projects: role.Projects,
	}

	return resp, nil
}

func versionFromInternal(ms storage.MigrationStatus) *api.Version {
	switch ms {
	case storage.Successful:
		return &api.Version{
			Major: api.Version_V2,
			Minor: api.Version_V0,
		}
	case storage.SuccessfulBeta1:
		return &api.Version{
			Major: api.Version_V2,
			Minor: api.Version_V1,
		}
	default:
		return &api.Version{
			Major: api.Version_V1,
			Minor: api.Version_V0,
		}
	}
}

func membersFromAPI(apiMembers []string) ([]storage.Member, error) {
	members := make([]storage.Member, len(apiMembers))
	for i, member := range apiMembers {
		memberInternal, err := storage.NewMember(member)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member")
		}
		members[i] = memberInternal
	}

	return members, nil
}

func statementFromAPI(statement *api.Statement) (storage.Statement, error) {
	effect, err := effectFromAPI(statement.Effect)
	if err != nil {
		return storage.Statement{}, err
	}

	// set resources to wildcard if not provided
	if len(statement.Resources) == 0 {
		statement.Resources = []string{"*"}
	}

	// map external representation of "all projects" to actual ID for that meta-project
	projects := make([]string, len(statement.Projects))
	for i, project := range statement.Projects {
		if project == constants.AllProjectsExternalID {
			projects[i] = constants.AllProjectsID
		} else {
			projects[i] = project
		}
	}

	return storage.NewStatement(effect, statement.Role, projects, statement.Resources, statement.Actions)
}

func effectFromAPI(eff api.Statement_Effect) (storage.Effect, error) {
	switch eff {
	case api.Statement_ALLOW:
		return storage.Allow, nil
	case api.Statement_DENY:
		return storage.Deny, nil
	default:
		return storage.Allow, errors.New("effect must always be one of allow or deny")
	}
}

func (s *policyServer) logPolicies(policies []*storage.Policy) {
	kv := logger.KV{}
	for _, p := range policies {
		kv[p.ID] = logger.KV{
			"name":       p.Name,
			"subjects":   p.Members,
			"statements": p.Statements,
		}
	}
	s.log.WithFields(kv).Info("Policy definition")
}

// setVersionForInterceptorSwitch informs the interceptor piece of this server
// to deny v1 requests if set to v2/v2.1 and vice-versa.
func (s *policyServer) setVersionForInterceptorSwitch(v api.Version) {
	if s.vChan != nil {
		s.vChan <- v
	}
}
