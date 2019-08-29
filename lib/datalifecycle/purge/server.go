package purge

import (
	"context"

	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"github.com/teambition/rrule-go"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/data_lifecycle"
	es "github.com/chef/automate/api/interservice/es_sidecar"
	"github.com/chef/automate/lib/cereal"
	"github.com/chef/automate/lib/stringutils"
)

var _ (data_lifecycle.PurgeServer) = (*Server)(nil)

// Server is the purge server implementation
type Server struct {
	jobName         string
	scheduleName    string
	defaultPolicies *Policies
	esSidecarClient es.EsSidecarClient
	jobManager      *cereal.Manager
}

// NewServer - create a new purge gRPC server
func NewServer(
	man *cereal.Manager,
	scheduleName string,
	jobName string,
	defaultPolicies *Policies,
	opts ...ServerOpt) (*Server, error) {
	s := &Server{
		jobManager:      man,
		scheduleName:    scheduleName,
		jobName:         jobName,
		defaultPolicies: defaultPolicies,
	}

	for _, o := range opts {
		o(s)
	}

	hasDefaultPolicies := false
	if len(s.defaultPolicies.Es) > 0 {
		hasDefaultPolicies = true

		if s.esSidecarClient == nil {
			return s, errors.New("server must have an esSidecarClient when configued with es policies")
		}
	}

	if len(s.defaultPolicies.Pg) > 0 {
		hasDefaultPolicies = true
	}

	if !hasDefaultPolicies {
		return s, errors.New("server must have default policies configured")
	}

	return s, nil
}

type ServerOpt func(*Server)

func WithServerEsSidecarClient(esClient es.EsSidecarClient) ServerOpt {
	return func(s *Server) {
		s.esSidecarClient = esClient
	}
}

// Run executes the purge workflow
func (server *Server) Run(
	ctx context.Context,
	_ *data_lifecycle.RunRequest) (*data_lifecycle.RunResponse, error) {

	var (
		res      = &data_lifecycle.RunResponse{}
		err      error
		sched    *cereal.Schedule
		policies Policies
	)

	log.Info("Purging")

	sched, err = server.jobManager.GetWorkflowScheduleByName(
		ctx, server.scheduleName, server.jobName,
	)
	if err != nil {
		return res, status.Error(codes.Internal, err.Error())
	}

	if err = sched.GetParameters(&policies); err != nil {
		return res, status.Error(codes.Internal, err.Error())
	}

	for _, policy := range policies.Pg {
		// NOTE: this is a noop until pg policies are finished
		if err = policy.Purge(ctx); err != nil {
			return res, status.Error(codes.Internal, err.Error())
		}
	}

	for _, policy := range policies.Es {
		if err = policy.Purge(ctx, server.esSidecarClient); err != nil {
			return res, status.Error(codes.Internal, err.Error())
		}
	}

	return res, nil
}

// Show shows the current scheduled purge job
func (server *Server) Show(ctx context.Context,
	req *data_lifecycle.ShowRequest) (*data_lifecycle.ShowResponse, error) {

	var (
		res          = &data_lifecycle.ShowResponse{}
		err          error
		sched        *cereal.Schedule
		policies     Policies
		dsEsPolicies = []*data_lifecycle.EsPolicy{}
		dsPgPolicies = []*data_lifecycle.PgPolicy{}
	)

	sched, err = server.jobManager.GetWorkflowScheduleByName(
		ctx, server.scheduleName, server.jobName,
	)
	if err != nil {
		return res, status.Error(codes.Internal, err.Error())
	}

	if err = sched.GetParameters(&policies); err != nil {
		return res, status.Error(codes.Internal, err.Error())
	}

	for _, policy := range policies.Es {
		dsEsPolicies = append(dsEsPolicies, &data_lifecycle.EsPolicy{
			Name:             policy.Name,
			Index:            policy.IndexName,
			OlderThanDays:    policy.OlderThanDays,
			CustomPurgeField: policy.CustomPurgeField,
			Disabled:         policy.Disabled,
		})
	}

	for _, policy := range policies.Pg {
		dsPgPolicies = append(dsPgPolicies, &data_lifecycle.PgPolicy{
			Name:     policy.Name,
			Disabled: policy.Disabled,
		})
	}

	res.InstanceName = server.jobName
	res.WorkflowName = server.scheduleName
	res.Enabled = sched.Enabled
	res.EsPolicies = dsEsPolicies
	res.PgPolicies = dsPgPolicies
	res.Recurrence = sched.Recurrence
	nextDue, err := ptypes.TimestampProto(sched.NextDueAt)
	if err != nil {
		return res, err
	}
	res.NextDueAt = nextDue
	lastEnqueue, err := ptypes.TimestampProto(sched.LastEnqueuedAt)
	if err != nil {
		return res, err
	}
	res.LastEnqueuedAt = lastEnqueue

	if sched.LastStart != nil {
		lastStart, err := ptypes.TimestampProto(*sched.LastStart)
		if err != nil {
			return res, err
		}
		res.LastStart = lastStart
	}

	if sched.LastEnd != nil {
		lastEnd, err := ptypes.TimestampProto(*sched.LastEnd)
		if err != nil {
			return res, err
		}
		res.LastEnd = lastEnd
	}

	return res, nil
}

// Configure creates or updates the current policies. Every policy that matches
// the update policy name will be updated or created if it doesn't exist. If
// a policy update does not match an allowed policy an error will be returned.
func (server *Server) Configure(ctx context.Context,
	req *data_lifecycle.ConfigureRequest) (*data_lifecycle.ConfigureResponse, error) {

	var (
		res        = &data_lifecycle.ConfigureResponse{}
		updateOpts []cereal.WorkflowScheduleUpdateOpts
		sched      *cereal.Schedule
		recurrence *rrule.RRule
		updated    = false
		err        error
		policies   = Policies{
			Es: map[string]EsPolicy{},
			Pg: map[string]PgPolicy{},
		}
	)

	log.WithFields(log.Fields{
		"enabled":       req.GetEnabled(),
		"policy_update": req.GetPolicyUpdate(),
	}).Info("Configuring purge job")

	// Make sure we're not trying to configure policies that don't exist
	err = server.validatePolicyUpdatePolicyNames(req.GetPolicyUpdate())
	if err != nil {
		return res, status.Error(codes.InvalidArgument, err.Error())
	}

	// Get the existing scheduled job and policies
	sched, err = server.jobManager.GetWorkflowScheduleByName(
		ctx, server.scheduleName, server.jobName,
	)
	if err != nil {
		return res, status.Error(codes.Internal, err.Error())
	}

	if err = sched.GetParameters(&policies); err != nil {
		return res, status.Error(codes.Internal, err.Error())
	}

	// Update enabled
	updateOpts = append(updateOpts, cereal.UpdateEnabled(req.GetEnabled()))

	// Generate desired policies based on current or default policies and the
	// desired policy update.
	var up bool
	policies, up, err = server.updateEsPolicies(req.GetPolicyUpdate().GetEs(), policies)
	if up == true {
		updated = true
	}

	if err != nil {
		return res, status.Error(codes.InvalidArgument, err.Error())
	}

	policies, up, err = server.updatePgPolicies(req.GetPolicyUpdate().GetPg(), policies)
	if up == true {
		updated = true
	}

	if err != nil {
		return res, status.Error(codes.InvalidArgument, err.Error())
	}

	if updated {
		updateOpts = append(updateOpts, cereal.UpdateParameters(policies))
	}

	if !sched.Enabled && req.GetEnabled() {
		updated = true
	}

	// Update recurrence
	if req.GetRecurrence() != "" {
		if sched.Recurrence != req.GetRecurrence() {
			recurrence, err = rrule.StrToRRule(req.GetRecurrence())
			if err != nil {
				return res, status.Error(codes.InvalidArgument, "invalid recurrence rule")
			}
			updateOpts = append(updateOpts, cereal.UpdateRecurrence(recurrence))
		}
	}

	// Update the workflow
	err = server.jobManager.UpdateWorkflowScheduleByName(
		ctx, server.scheduleName, server.jobName, updateOpts...,
	)
	if err != nil {
		return res, status.Error(codes.Internal, err.Error())
	}

	// Kick off the workflow if it was updated and it is enabled
	if updated && req.GetEnabled() {
		err = server.jobManager.EnqueueWorkflow(
			ctx, server.jobName, server.scheduleName, policies,
		)
		if err == cereal.ErrWorkflowInstanceExists {
			return res, nil // it's already running
		}

		if err != nil {
			return res, status.Error(codes.Internal, err.Error())
		}
	}

	return res, nil
}

// updateEsPolicies takes in desired policy update and the current policies set
// and return updated policy set, a boolean that indicates that the set was updated, and
// an error if the policy update refers to an unknown policy.
func (server *Server) updateEsPolicies(esUpdates []*data_lifecycle.EsPolicyUpdate, currentPolicies Policies) (Policies, bool, error) {
	var (
		updated     = false
		newPolicies = Policies{
			Es: map[string]EsPolicy{},
		}
	)

	for _, update := range esUpdates {
		found := false
		// If our current policies match a desired policy update we'll update it
		for _, p := range currentPolicies.Es {
			if p.Name == update.PolicyName {
				found = true

				if p.OlderThanDays != update.OlderThanDays {
					updated = true
					p.OlderThanDays = update.OlderThanDays
				}

				if p.Disabled != update.Disabled {
					updated = true
					p.Disabled = update.Disabled
				}

				newPolicies.Es[p.Name] = p
				break
			}
		}

		if !found {
			// Check to see if the desired policy update matches an allowed
			// policy that we've been configured with.
			for _, p := range server.defaultPolicies.Es {
				if p.Name == update.PolicyName {
					found = true
					updated = true

					p.OlderThanDays = update.OlderThanDays
					p.Disabled = update.Disabled
					newPolicies.Es[p.Name] = p

					break
				}
			}
		}

		// Should be impossible to get here since we should have validated
		if !found {
			return currentPolicies, updated, errors.Errorf("unknown policy: %s", update.PolicyName)
		}
	}

	return newPolicies, updated, nil
}

// updatePgPolicies takes in a update and currently set policies and returns an
// updated policy set, a boolean that indicates that the set was updated, and
// an error if the policy update refers to an unknown policy.
func (server Server) updatePgPolicies(pgUpdate []*data_lifecycle.PgPolicyUpdate, currentPolicies Policies) (Policies, bool, error) {
	// NOTE: Right now PG policy support is a shim to be filled in later.
	return currentPolicies, false, nil
}

func (server *Server) validatePolicyUpdatePolicyNames(pu *data_lifecycle.PolicyUpdate) error {
	allowedEsNames := make([]string, len(server.defaultPolicies.Es))
	allowedPgNames := make([]string, len(server.defaultPolicies.Pg))

	for _, n := range server.defaultPolicies.Pg {
		allowedPgNames = append(allowedPgNames, n.Name)
	}

	for _, n := range server.defaultPolicies.Es {
		allowedEsNames = append(allowedEsNames, n.Name)
	}

	for _, update := range pu.GetEs() {
		if !stringutils.SliceContains(allowedEsNames, update.PolicyName) {
			return errors.Errorf("'%s' is not a valid ES policy name", update.PolicyName)
		}
	}

	for _, update := range pu.GetPg() {
		if !stringutils.SliceContains(allowedPgNames, update.PolicyName) {
			return errors.Errorf("'%s' is not a valid PG policy name", update.PolicyName)
		}
	}

	return nil
}
