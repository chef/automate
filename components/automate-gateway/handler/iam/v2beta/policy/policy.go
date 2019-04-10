package policy

import (
	"context"
	"fmt"

	"github.com/pkg/errors"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	authz "github.com/chef/automate/api/interservice/authz/v2"

	pb_common "github.com/chef/automate/components/automate-gateway/api/iam/v2beta/common"
	pb_req "github.com/chef/automate/components/automate-gateway/api/iam/v2beta/request"
	pb_resp "github.com/chef/automate/components/automate-gateway/api/iam/v2beta/response"
)

// Server is the server interface
type Server struct {
	policies authz.PoliciesClient
	projects authz.ProjectsClient
}

// NewServer creates a server with its client.
func NewServer(policies authz.PoliciesClient, projects authz.ProjectsClient) *Server {
	return &Server{
		policies: policies,
		projects: projects,
	}
}

// CreatePolicy creates a new policy.
func (p *Server) CreatePolicy(
	ctx context.Context, in *pb_req.CreatePolicyReq) (*pb_resp.CreatePolicyResp, error) {
	statements, err := convertAPIStatementSliceToDomain(in.Statements)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument,
			errors.Wrap(err, "could not parse statements").Error())
	}

	resp, err := p.policies.CreatePolicy(ctx, &authz.CreatePolicyReq{
		Id:         in.Id,
		Name:       in.Name,
		Members:    in.Members,
		Statements: statements,
	})
	if err != nil {
		return nil, err
	}

	policy, err := convertDomainPolicyToAPIPolicy(resp)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &pb_resp.CreatePolicyResp{
		Policy: policy,
	}, nil
}

// DeletePolicy deletes a specific policy.
func (p *Server) DeletePolicy(
	ctx context.Context, in *pb_req.DeletePolicyReq) (*pb_resp.DeletePolicyResp, error) {
	_, err := p.policies.DeletePolicy(ctx, &authz.DeletePolicyReq{
		Id: in.Id,
	})
	if err != nil {
		return nil, err
	}
	return &pb_resp.DeletePolicyResp{}, nil
}

// ListPolicies returns the list of all policies.
func (p *Server) ListPolicies(
	ctx context.Context, _ *pb_req.ListPoliciesReq) (*pb_resp.ListPoliciesResp, error) {
	resp, err := p.policies.ListPolicies(ctx, &authz.ListPoliciesReq{})
	if err != nil {
		return nil, err
	}

	var policies []*pb_common.Policy
	for _, authzPol := range resp.Policies {
		policy, err := convertDomainPolicyToAPIPolicy(authzPol)
		if err != nil {
			return nil, status.Error(codes.Internal, err.Error())
		}
		policies = append(policies, policy)
	}

	return &pb_resp.ListPoliciesResp{
		Policies: policies,
	}, nil
}

// GetPolicy returns a specific policy.
func (p *Server) GetPolicy(
	ctx context.Context, in *pb_req.GetPolicyReq) (*pb_resp.GetPolicyResp, error) {
	resp, err := p.policies.GetPolicy(ctx, &authz.GetPolicyReq{
		Id: in.Id,
	})
	if err != nil {
		return nil, err
	}

	policy, err := convertDomainPolicyToAPIPolicy(resp)
	if err != nil {
		return nil, status.Error(codes.Internal, "could not parse response from backend")
	}

	return &pb_resp.GetPolicyResp{
		Policy: policy,
	}, nil
}

// UpdatePolicy returns the updated policy.
func (p *Server) UpdatePolicy(
	ctx context.Context, in *pb_req.UpdatePolicyReq) (*pb_resp.UpdatePolicyResp, error) {
	statements, err := convertAPIStatementSliceToDomain(in.Statements)
	if err != nil {
		return nil, status.Error(codes.InvalidArgument,
			errors.Wrap(err, "could not parse statements").Error())
	}

	resp, err := p.policies.UpdatePolicy(ctx, &authz.UpdatePolicyReq{
		Id:         in.Id,
		Name:       in.Name,
		Members:    in.Members,
		Statements: statements,
	})
	if err != nil {
		return nil, err
	}

	policy, err := convertDomainPolicyToAPIPolicy(resp)
	if err != nil {
		return nil, status.Error(codes.Internal, "could not parse response from backend")
	}

	return &pb_resp.UpdatePolicyResp{
		Policy: policy,
	}, nil
}

// ListPolicyMembers returns the list of members for a specific policy.
func (p *Server) ListPolicyMembers(
	ctx context.Context, in *pb_req.ListPolicyMembersReq) (*pb_resp.ListPolicyMembersResp, error) {
	resp, err := p.policies.ListPolicyMembers(ctx, &authz.ListPolicyMembersReq{
		Id: in.Id,
	})
	if err != nil {
		return nil, err
	}

	return &pb_resp.ListPolicyMembersResp{
		Members: resp.Members,
	}, nil
}

// ReplacePolicyMembers takes in a new list of policy members and completely replaces
// all policy members for given policy with new list.
func (p *Server) ReplacePolicyMembers(
	ctx context.Context, in *pb_req.ReplacePolicyMembersReq) (*pb_resp.ReplacePolicyMembersResp, error) {
	resp, err := p.policies.ReplacePolicyMembers(ctx, &authz.ReplacePolicyMembersReq{
		Id:      in.Id,
		Members: in.Members,
	})
	if err != nil {
		return nil, err
	}

	return &pb_resp.ReplacePolicyMembersResp{
		Members: resp.Members,
	}, nil
}

// AddPolicyMembers add members to the policy then returns the list of members
func (p *Server) AddPolicyMembers(
	ctx context.Context, in *pb_req.AddPolicyMembersReq) (*pb_resp.AddPolicyMembersResp, error) {
	resp, err := p.policies.AddPolicyMembers(ctx, &authz.AddPolicyMembersReq{
		Id:      in.Id,
		Members: in.Members,
	})
	if err != nil {
		return nil, err
	}

	return &pb_resp.AddPolicyMembersResp{
		Members: resp.Members,
	}, nil
}

// RemovePolicyMembers takes in a new list of policy members and completely replaces
// all policy members for given policy with new list.
func (p *Server) RemovePolicyMembers(
	ctx context.Context, in *pb_req.RemovePolicyMembersReq) (*pb_resp.RemovePolicyMembersResp, error) {
	resp, err := p.policies.RemovePolicyMembers(ctx, &authz.RemovePolicyMembersReq{
		Id:      in.Id,
		Members: in.Members,
	})
	if err != nil {
		return nil, err
	}

	return &pb_resp.RemovePolicyMembersResp{
		Members: resp.Members,
	}, nil
}

// UpgradeToV2 resets the IAM v2 database to its factory default state, and
// migrates existing V1 policies
func (p *Server) UpgradeToV2(
	ctx context.Context, in *pb_req.UpgradeToV2Req) (*pb_resp.UpgradeToV2Resp, error) {
	upgradeReq := &authz.MigrateToV2Req{
		Flag: authz.Flag_VERSION_2_0,
	}
	if in.Flag == pb_common.Flag_VERSION_2_1 {
		upgradeReq.Flag = authz.Flag_VERSION_2_1
	}

	resp, err := p.policies.MigrateToV2(ctx, upgradeReq)
	if err != nil {
		return nil, err
	}

	return &pb_resp.UpgradeToV2Resp{Reports: resp.GetReports()}, nil
}

func (p *Server) GetPolicyVersion(
	ctx context.Context, in *pb_req.GetPolicyVersionReq) (*pb_resp.GetPolicyVersionResp, error) {
	resp, err := p.policies.GetPolicyVersion(ctx, &authz.GetPolicyVersionReq{})
	if err != nil {
		return nil, err
	}

	major, err := versionFromInternalVersion(resp.Version.Major)
	minor, err := versionFromInternalVersion(resp.Version.Minor)

	return &pb_resp.GetPolicyVersionResp{
		Version: &pb_common.Version{
			Major: major,
			Minor: minor,
		},
	}, nil
}

func (p *Server) ResetToV1(
	ctx context.Context, in *pb_req.ResetToV1Req) (*pb_resp.ResetToV1Resp, error) {
	_, err := p.policies.ResetToV1(ctx, &authz.ResetToV1Req{})
	if err != nil {
		return nil, err
	}

	return &pb_resp.ResetToV1Resp{}, nil
}

// CreateRole creates a new role.
func (p *Server) CreateRole(
	ctx context.Context, in *pb_req.CreateRoleReq) (*pb_resp.CreateRoleResp, error) {

	storedRole, err := p.policies.CreateRole(ctx, &authz.CreateRoleReq{
		Id:       in.Id,
		Name:     in.Name,
		Actions:  in.Actions,
		Projects: in.Projects,
	})
	if err != nil {
		return nil, err
	}

	apiRole, err := internalRoleToAPIRole(storedRole)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}

	return &pb_resp.CreateRoleResp{
		Role: apiRole,
	}, nil
}

// GetRole returns a specific role.
func (p *Server) GetRole(
	ctx context.Context, in *pb_req.GetRoleReq) (*pb_resp.GetRoleResp, error) {
	storedRole, err := p.policies.GetRole(ctx, &authz.GetRoleReq{
		Id: in.Id,
	})
	if err != nil {
		return nil, err
	}

	apiRole, err := internalRoleToAPIRole(storedRole)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}
	return &pb_resp.GetRoleResp{
		Role: apiRole,
	}, nil
}

// DeleteRole deletes a specific role.
func (p *Server) DeleteRole(
	ctx context.Context, in *pb_req.DeleteRoleReq) (*pb_resp.DeleteRoleResp, error) {
	_, err := p.policies.DeleteRole(ctx, &authz.DeleteRoleReq{
		Id: in.Id,
	})
	if err != nil {
		return nil, err
	}
	return &pb_resp.DeleteRoleResp{}, nil
}

// UpdateRole returns the updated role.
func (p *Server) UpdateRole(
	ctx context.Context, in *pb_req.UpdateRoleReq) (*pb_resp.UpdateRoleResp, error) {
	resp, err := p.policies.UpdateRole(ctx, &authz.UpdateRoleReq{
		Id:       in.Id,
		Name:     in.Name,
		Actions:  in.Actions,
		Projects: in.Projects,
	})
	if err != nil {
		return nil, err
	}

	apiRole, err := internalRoleToAPIRole(resp)
	if err != nil {
		return nil, status.Error(codes.Internal, err.Error())
	}
	return &pb_resp.UpdateRoleResp{
		Role: apiRole,
	}, nil
}

// ListRoles returns the list of all roles.
func (p *Server) ListRoles(
	ctx context.Context, _ *pb_req.ListRolesReq) (*pb_resp.ListRolesResp, error) {
	resp, err := p.policies.ListRoles(ctx, &authz.ListRolesReq{})
	if err != nil {
		return nil, err
	}

	roles := make([]*pb_common.Role, len(resp.Roles))
	for index, role := range resp.Roles {
		apiRole, err := internalRoleToAPIRole(role)
		if err != nil {
			return nil, status.Error(codes.Internal, err.Error())
		}

		roles[index] = apiRole
	}

	return &pb_resp.ListRolesResp{
		Roles: roles,
	}, nil
}

// CreateProject creates a new project.
func (p *Server) CreateProject(
	ctx context.Context, in *pb_req.CreateProjectReq) (*pb_resp.CreateProjectResp, error) {
	resp, err := p.projects.CreateProject(ctx, &authz.CreateProjectReq{
		Id:   in.Id,
		Name: in.Name,
	})
	if err != nil {
		return nil, err
	}
	proj, err := domainProjectToAPIProject(resp.Project)
	if err != nil {
		return nil, err
	}
	return &pb_resp.CreateProjectResp{Project: proj}, nil
}

// UpdateProject creates a new project.
func (p *Server) UpdateProject(
	ctx context.Context, in *pb_req.UpdateProjectReq) (*pb_resp.UpdateProjectResp, error) {
	resp, err := p.projects.UpdateProject(ctx, &authz.UpdateProjectReq{
		Id:   in.Id,
		Name: in.Name,
	})
	if err != nil {
		return nil, err
	}
	proj, err := domainProjectToAPIProject(resp.Project)
	if err != nil {
		return nil, err
	}
	return &pb_resp.UpdateProjectResp{Project: proj}, nil
}

// GetProject returns a specific project.
func (p *Server) GetProject(
	ctx context.Context, in *pb_req.GetProjectReq) (*pb_resp.GetProjectResp, error) {
	resp, err := p.projects.GetProject(ctx, &authz.GetProjectReq{
		Id: in.Id,
	})
	if err != nil {
		return nil, err
	}
	proj, err := domainProjectToAPIProject(resp.Project)
	if err != nil {
		return nil, err
	}
	return &pb_resp.GetProjectResp{Project: proj}, nil
}

// ListProjects returns the list of all projects.
func (p *Server) ListProjects(
	ctx context.Context, _ *pb_req.ListProjectsReq) (*pb_resp.ListProjectsResp, error) {
	resp, err := p.projects.ListProjects(ctx, &authz.ListProjectsReq{})
	if err != nil {
		return nil, err
	}
	projects := make([]*pb_common.Project, len(resp.Projects))
	for i, proj := range resp.Projects {
		apiProj, err := domainProjectToAPIProject(proj)
		if err != nil {
			return nil, err
		}
		projects[i] = apiProj
	}
	return &pb_resp.ListProjectsResp{Projects: projects}, nil
}

// DeleteProject deletes a project.
func (p *Server) DeleteProject(
	ctx context.Context, in *pb_req.DeleteProjectReq) (*pb_resp.DeleteProjectResp, error) {
	_, err := p.projects.DeleteProject(ctx, &authz.DeleteProjectReq{
		Id: in.Id,
	})
	if err != nil {
		return nil, err
	}
	return &pb_resp.DeleteProjectResp{}, nil
}

func convertDomainPolicyToAPIPolicy(policy *authz.Policy) (*pb_common.Policy, error) {
	statements, err := convertDomainStatementSliceToAPI(policy.Statements)
	if err != nil {
		return nil, errors.Wrap(err, "could not parse response from backend")
	}

	t, err := typeFromInternalType(policy.Type)
	if err != nil {
		return nil, status.Errorf(codes.Internal,
			"unexpected policy type for policy %q: %q", policy.Id, policy.Type)
	}

	return &pb_common.Policy{
		Id:         policy.Id,
		Name:       policy.Name,
		Type:       t,
		Members:    policy.Members,
		Statements: statements,
	}, nil
}

// These methods are mostly needed because we can't cast between Effect enums.
func convertAPIStatementSliceToDomain(external []*pb_common.Statement) ([]*authz.Statement, error) {
	var internal []*authz.Statement
	for _, statement := range external {
		// lest former v1 users attempt to create policy with resources (which are ignored)
		if len(statement.Resources) > 0 {
			return nil, errors.New("cannot define resources on v2 policy")
		}
		effectValue, ok := authz.Statement_Effect_value[statement.Effect.String()]
		if !ok {
			return nil, errors.New("could not parse statement effect")
		}

		// Note: this is where we ignore the request's statements' resources
		internal = append(internal, &authz.Statement{
			Effect:  authz.Statement_Effect(effectValue),
			Actions: statement.Actions,
			Role:    statement.Role,
		})
	}
	return internal, nil
}

func convertDomainStatementSliceToAPI(internal []*authz.Statement) ([]*pb_common.Statement, error) {
	var external []*pb_common.Statement
	for _, statement := range internal {
		effectValue, ok := pb_common.Statement_Effect_value[statement.Effect.String()]
		if !ok {
			return nil, errors.New("could not convert effect response from backend")
		}
		effect := pb_common.Statement_Effect(effectValue)
		external = append(external, &pb_common.Statement{
			Effect:    effect,
			Actions:   statement.Actions,
			Role:      statement.Role,
			Resources: statement.Resources,
		})
	}
	return external, nil
}

func internalRoleToAPIRole(role *authz.Role) (*pb_common.Role, error) {
	t, err := typeFromInternalType(role.Type)
	if err != nil {
		return nil, errors.Wrapf(err, "role %q", role.Id)
	}
	return &pb_common.Role{
		Id:       role.Id,
		Name:     role.Name,
		Actions:  role.Actions,
		Type:     t,
		Projects: role.Projects,
	}, nil
}

func versionFromInternalVersion(v authz.Version_VersionNumber) (pb_common.Version_VersionNumber, error) {
	switch v {
	case authz.Version_V0:
		return pb_common.Version_V0, nil
	case authz.Version_V1:
		return pb_common.Version_V1, nil
	case authz.Version_V2:
		return pb_common.Version_V2, nil
	default:
		return 0, fmt.Errorf("unexpected version %q", v)
	}
}

func typeFromInternalType(s authz.Type) (pb_common.Type, error) {
	switch s {
	case authz.Type_CHEF_MANAGED:
		return pb_common.Type_CHEF_MANAGED, nil
	case authz.Type_CUSTOM:
		return pb_common.Type_CUSTOM, nil
	default:
		return 0, fmt.Errorf("unexpected type %q", s)
	}
}

func domainProjectToAPIProject(p *authz.Project) (*pb_common.Project, error) {
	t, err := typeFromInternalType(p.Type)
	if err != nil {
		return nil, errors.Wrapf(err, "project %q", p.Id)
	}
	return &pb_common.Project{
		Id:       p.Id,
		Name:     p.Name,
		Type:     t,
		Projects: p.Projects,
	}, nil
}
