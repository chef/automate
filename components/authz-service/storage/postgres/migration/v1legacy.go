package v2

import (
	"context"
	"fmt"
	"strings"

	"github.com/pkg/errors"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	constants_v1 "github.com/chef/automate/components/authz-service/constants/v1"
	constants_v2 "github.com/chef/automate/components/authz-service/constants/v2"
	storage_errors "github.com/chef/automate/components/authz-service/storage"
	storage_v1 "github.com/chef/automate/components/authz-service/storage/v1"
	storage "github.com/chef/automate/components/authz-service/storage/v2"
	uuid "github.com/chef/automate/lib/uuid4"
)

// Note: it would be silly to translate the migration status into errors twice,
// so this method is an exception to the "only grpc handlers return status
// instances" rule.
func (s *policyServer) okToMigrate(ctx context.Context, ms storage.MigrationStatus) error {
	switch ms {
	case storage.Successful, storage.SuccessfulBeta1:
		return status.Error(codes.AlreadyExists, "already migrated")
	case storage.InProgress:
		return status.Error(codes.FailedPrecondition, "migration already in progress")
	case storage.Failed:
		s.log.Info("migration failed before; attempting again")
	}
	return nil
}

// migrateV1Policies has two error returns: the second one is the ordinary,
// garden-variety, "something went wrong, I've given up" signal; the first one
// serves as an aggregate of errors that happened attempting to convert and
// store individual (custom) policies.
func (s *policyServer) migrateV1Policies(ctx context.Context) ([]error, error) {
	pols, err := s.v1.ListPoliciesWithSubjects(ctx)
	if err != nil {
		return nil, status.Errorf(codes.Internal, "list v1 policies: %s", err.Error())
	}

	var errs []error
	for _, pol := range pols {
		adminTokenPolicy, err := checkForAdminTokenPolicy(pol)
		if err != nil {
			errs = append(errs, errors.Wrapf(err, "verify subjects %q for admin policy %q", pol.Subjects, pol.ID.String()))
			continue
		}
		if adminTokenPolicy != nil {
			if err := s.addTokenToAdminPolicy(ctx, adminTokenPolicy.Subjects[0]); err != nil {
				errs = append(errs, errors.Wrapf(err, "adding members %q for admin policy %q", pol.Subjects, pol.ID.String()))
			}
			continue // don't migrate admin policies with single token
		}
		storagePol, err := migrateV1Policy(pol)
		if err != nil {
			// collect error
			errs = append(errs, errors.Wrapf(err, "convert v1 policy %q", pol.ID.String()))
			continue // nothing to create
		}
		if storagePol == nil {
			continue // nothing to create
		}
		_, err = s.store.CreatePolicy(ctx, storagePol, true)
		switch err {
		case nil, storage_errors.ErrConflict: // ignore, continue
		default:
			errs = append(errs, errors.Wrapf(err, "store converted v1 policy %q", pol.ID.String()))
		}
	}
	return errs, nil
}

func migrateV1Policy(pol *storage_v1.Policy) (*storage.Policy, error) {
	wellKnown, err := isWellKnown(pol.ID)
	if err != nil {
		return nil, errors.Wrapf(err, "lookup v1 default policy %q", pol.ID.String())
	}

	if wellKnown {
		return legacyPolicyFromV1(pol)
	}
	return customPolicyFromV1(pol)
}

func (s *policyServer) addTokenToAdminPolicy(ctx context.Context, tok string) error {
	m, err := storage.NewMember(tok)
	if err != nil {
		return errors.Wrap(err, "format v2 member for admin team")
	}
	mems, err := s.store.AddPolicyMembers(ctx, constants_v2.AdminPolicyID, []storage.Member{m})
	if err != nil {
		return errors.Wrapf(err, "could not add members %q to admin policy", mems)
	}
	return nil
}

func checkForAdminTokenPolicy(pol *storage_v1.Policy) (*storage_v1.Policy, error) {
	if pol.Action == "*" && pol.Resource == "*" && len(pol.Subjects) == 1 && strings.HasPrefix(pol.Subjects[0], "token:") {
		return pol, nil
	}
	return nil, nil
}

func isWellKnown(id uuid.UUID) (bool, error) {
	defaultV1, err := storage_v1.DefaultPolicies()
	if err != nil {
		return false, err
	}
	_, found := defaultV1[id.String()]
	return found, nil
}

var v1PoliciesToSkip = map[string]struct{}{
	constants_v1.AdminPolicyID:                          {},
	constants_v1.ServiceInfoWildcardPolicyID:            {},
	constants_v1.AuthIntrospectionWildcardPolicyID:      {},
	constants_v1.LicenseStatusPolicyID:                  {},
	constants_v1.ReadOwnUserProfilePolicyID:             {},
	constants_v1.LocalUserSelfPolicyID:                  {},
	constants_v1.PolicyVersionPolicyID:                  {},
	constants_v1.OcErchefIngestStatusPolicyID:           {},
	constants_v1.OcErchefIngestEventsPolicyID:           {},
	constants_v1.CSNginxComplianceProfilesPolicyID:      {},
	constants_v1.CSNginxComplianceDataCollectorPolicyID: {},
	constants_v1.ApplicationsServiceGroupsPolicyID:      {},
}

var v1CfgmgmtPolicies = map[string]struct{}{
	constants_v1.CfgmgmtNodesContainerPolicyID: {},
	constants_v1.CfgmgmtNodesWildcardPolicyID:  {},
	constants_v1.CfgmgmtStatsWildcardPolicyID:  {},
}

var v1EventFeedPolicies = map[string]struct{}{
	constants_v1.EventsContainerPolicyID: {},
	constants_v1.EventsWildcardPolicyID:  {},
}

var v1NodesPolicies = map[string]struct{}{
	constants_v1.NodesContainerPolicyID: {},
	constants_v1.NodesWildcardPolicyID:  {},
}

var v1NodeManagersPolicies = map[string]struct{}{
	constants_v1.NodeManagersContainerPolicyID: {},
	constants_v1.NodeManagersWildcardPolicyID:  {},
}

var v1SecretsPolicies = map[string]struct{}{
	constants_v1.SecretsContainerPolicyID: {},
	constants_v1.SecretsWildcardPolicyID:  {},
}

var v1ComplianceTokenPolicies = map[string]struct{}{
	constants_v1.ComplianceTokenReadProfilesPolicyID:   {},
	constants_v1.ComplianceTokenSearchProfilesPolicyID: {},
	constants_v1.ComplianceTokenUploadProfilesPolicyID: {},
}

// nolint: gocyclo
func legacyPolicyFromV1(pol *storage_v1.Policy) (*storage.Policy, error) {
	if _, found := v1PoliciesToSkip[pol.ID.String()]; found {
		return nil, nil
	}
	noProjects := []string{}

	// there's three cfgmgmt policies (which had been deletable) that are now
	// mapped into one:
	//  CfgmgmtNodesContainerPolicyID
	//  CfgmgmtNodesWildcardPolicyID
	//  CfgmgmtStatsWildcardPolicyID
	if _, found := v1CfgmgmtPolicies[pol.ID.String()]; found {
		cfgmgmtStatement, err := storage.NewStatement(storage.Allow, "", []string{},
			[]string{"*"}, []string{"infra:*"})
		if err != nil {
			return nil, errors.Wrap(err, "format v2 statement (cfgmgmt)")
		}
		member, err := storage.NewMember("user:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (cfgmgmt)")
		}
		cfgmgmtPolicy, err := storage.NewPolicy(constants_v2.CfgmgmtPolicyID,
			"[Legacy] Infrastructure Automation Access",
			storage.Custom, []storage.Member{member},
			[]storage.Statement{cfgmgmtStatement}, noProjects)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (cfgmgmt)")
		}
		return &cfgmgmtPolicy, nil
	}

	if pol.ID.String() == constants_v1.ComplianceWildcardPolicyID {
		complianceStatement, err := storage.NewStatement(storage.Allow, "", []string{},
			[]string{"*"}, []string{"compliance:*"})
		if err != nil {
			return nil, errors.Wrap(err, "format v2 statement (compliance)")
		}
		member, err := storage.NewMember("user:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (compliance)")
		}
		compliancePolicy, err := storage.NewPolicy(constants_v2.CompliancePolicyID,
			"[Legacy] Compliance Access",
			storage.Custom, []storage.Member{member}, []storage.Statement{complianceStatement}, noProjects)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (cfgmgmt)")
		}
		return &compliancePolicy, nil
	}

	if _, found := v1EventFeedPolicies[pol.ID.String()]; found {
		eventsStatement, err := storage.NewStatement(storage.Allow, "", []string{},
			[]string{"*"}, []string{"event:*"})
		if err != nil {
			return nil, errors.Wrap(err, "format v2 statement (events)")
		}
		member, err := storage.NewMember("user:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (events)")
		}
		eventsPolicy, err := storage.NewPolicy(constants_v2.EventsPolicyID,
			"[Legacy] Events Access",
			storage.Custom, []storage.Member{member}, []storage.Statement{eventsStatement}, noProjects)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (events)")
		}
		return &eventsPolicy, nil
	}

	if pol.ID.String() == constants_v1.IngestWildcardPolicyID {
		ingestStatement, err := storage.NewStatement(storage.Allow, "", []string{},
			[]string{"*"}, []string{"infra:ingest:*"})
		if err != nil {
			return nil, errors.Wrap(err, "format v2 statement (ingest)")
		}
		member, err := storage.NewMember("token:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (ingest)")
		}

		ingestPolicy, err := storage.NewPolicy(constants_v2.LegacyIngestPolicyID,
			"[Legacy] Ingest Access",
			storage.Custom, []storage.Member{member}, []storage.Statement{ingestStatement}, noProjects)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (ingest)")
		}
		return &ingestPolicy, nil
	}

	if _, found := v1NodesPolicies[pol.ID.String()]; found {
		nodesStatement, err := storage.NewStatement(storage.Allow, "", []string{},
			[]string{"*"}, []string{"infra:nodes:*"})
		if err != nil {
			return nil, errors.Wrap(err, "format v2 statement (nodes)")
		}
		member, err := storage.NewMember("user:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (nodes)")
		}
		nodesPolicy, err := storage.NewPolicy(
			constants_v2.NodesPolicyID,
			"[Legacy] Nodes Access",
			storage.Custom,
			[]storage.Member{member},
			[]storage.Statement{nodesStatement},
			noProjects,
		)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (nodes)")
		}
		return &nodesPolicy, nil
	}

	if _, found := v1NodeManagersPolicies[pol.ID.String()]; found {
		nodeManagersStatement, err := storage.NewStatement(storage.Allow, "", []string{},
			[]string{"*"}, []string{"infra:nodeManagers:*"})
		if err != nil {
			return nil, errors.Wrap(err, "format v2 statement (nodemanagers)")
		}
		member, err := storage.NewMember("user:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (nodemanagers)")
		}
		nodeManagersPolicy, err := storage.NewPolicy(
			constants_v2.NodeManagersPolicyID,
			"[Legacy] Node Managers Access",
			storage.Custom,
			[]storage.Member{member},
			[]storage.Statement{nodeManagersStatement},
			noProjects,
		)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (nodemanagers)")
		}
		return &nodeManagersPolicy, nil
	}

	if _, found := v1SecretsPolicies[pol.ID.String()]; found {
		secretsStatement, err := storage.NewStatement(storage.Allow, "", []string{},
			[]string{"*"}, []string{"secrets:*"})
		if err != nil {
			return nil, errors.Wrap(err, "format v2 statement (secrets)")
		}
		member, err := storage.NewMember("user:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (secrets)")
		}
		secretsPolicy, err := storage.NewPolicy(
			constants_v2.SecretsPolicyID,
			"[Legacy] Secrets Access",
			storage.Custom,
			[]storage.Member{member},
			[]storage.Statement{secretsStatement},
			noProjects,
		)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (secrets)")
		}
		return &secretsPolicy, nil
	}

	if pol.ID.String() == constants_v1.TelemetryConfigPolicyID {
		telemetryStatement, err := storage.NewStatement(storage.Allow, "", []string{},
			[]string{"*"}, []string{"system:telemetryConfig:*"})
		if err != nil {
			return nil, errors.Wrap(err, "format v2 statement (telemetry)")
		}
		member, err := storage.NewMember("user:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (telemetry)")
		}
		telemetryPolicy, err := storage.NewPolicy(
			constants_v2.TelemetryPolicyID,
			"[Legacy] Telemetry Access",
			storage.Custom,
			[]storage.Member{member},
			[]storage.Statement{telemetryStatement},
			noProjects,
		)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (telemetry)")
		}
		return &telemetryPolicy, nil
	}

	if _, found := v1ComplianceTokenPolicies[pol.ID.String()]; found {
		complianceTokenStatement, err := storage.NewStatement(storage.Allow, "", []string{},
			[]string{"*"}, []string{"compliance:profiles:*"})
		if err != nil {
			return nil, errors.Wrap(err, "format v2 statement (compliance token)")
		}
		member, err := storage.NewMember("token:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (compliance token)")
		}
		complianceTokenPolicy, err := storage.NewPolicy(
			constants_v2.ComplianceTokenPolicyID,
			"[Legacy] Compliance Profile Access",
			storage.Custom,
			[]storage.Member{member},
			[]storage.Statement{complianceTokenStatement},
			noProjects,
		)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (compliance token)")
		}
		return &complianceTokenPolicy, nil
	}

	return nil, errors.New("unknown \"well-known\" policy")
}

func customPolicyFromV1(pol *storage_v1.Policy) (*storage.Policy, error) {
	name := fmt.Sprintf("%s (custom)", pol.ID.String())

	// TODO: If we encounter an unknown action can we just be less permissive with a warning?
	// AKA just use []string{"*"} instead of failing the migration?
	action, err := convertV1Action(pol.Action, pol.Resource)
	if err != nil {
		return nil, errors.Wrap(err, "could not derive v2 action")
	}

	resource, err := convertV1Resource(pol.Resource)
	if err != nil {
		return nil, errors.Wrap(err, "could not derive v2 resource")
	}

	// Note: v1 only had (custom) allow policies
	statement, err := storage.NewStatement(storage.Allow, "", []string{}, []string{resource}, action)
	if err != nil {
		return nil, errors.Wrap(err, "format v2 statement")
	}

	members := make([]storage.Member, len(pol.Subjects))
	for i, subject := range pol.Subjects {
		memberInt, err := storage.NewMember(subject)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member")
		}
		members[i] = memberInt
	}

	policy, err := storage.NewPolicy(
		pol.ID.String(),
		name,
		storage.Custom,
		members,
		[]storage.Statement{statement},
		[]string{})
	if err != nil {
		return nil, errors.Wrap(err, "format v2 policy")
	}

	return &policy, nil
}

// Basically implements "Current Resource" -> "New Resource Names (RFR)" column of
// https://docs.google.com/spreadsheets/d/1ccaYDJdMnHBfFgmNC1n2_S1YOnMJ-OgkYd8ySbb-mS0/edit#gid=363200100
func convertV1Resource(resource string) (string, error) {
	terms := strings.Split(resource, ":")
	if len(terms) == 0 {
		return "", errors.New("there was no resource passed")
	}

	if len(terms) == 1 && terms[0] == "*" {
		return "*", nil
	}

	switch terms[0] {
	case "service_info":
		switch terms[1] {
		case "version":
			return "system:service:version", nil
		case "health":
			return "system:health", nil
		}
		return "system:*", nil
	case "auth":
		terms = changeTerm(terms, "auth", "iam")
		terms = changeTerm(terms, "api_tokens", "tokens")
		return combineTermsIntoResource(terms...), nil
	case "users":
		// "users" -> "iam:usersSelf"
		terms[0] = "usersSelf"
		return combineTermsIntoResource(prefixTerms("iam", terms)...), nil
	case "auth_introspection":
		// Special case
		if terms[1] == "*" {
			return "iam:introspect", nil
		}
		terms = changeTerm(terms, "auth_introspection", "iam")
		terms = changeTerm(terms, "introspect_all", "introspect")
		terms = changeTerm(terms, "introspect_some", "introspect")
		return combineTermsIntoResource(terms...), nil
	case "cfgmgmt":
		return convertV1Cfgmgmt(terms)
	case "compliance":
		// Special case
		if resource == "compliance:profiles:market" {
			return "compliance:marketProfiles", nil
		}
		return combineTermsIntoResource(deleteTerm(terms, "storage")...), nil
	case "events":
		return convertV1Events(terms)
	case "ingest":
		// Special case: "ingest:status" -> "infra:ingest:status" (no wildcards to worry about)
		if terms[1] == "status" {
			return "infra:ingest:status", nil
		}

		// Special case: "ingest:unified_events" -> "infra:unifiedEvents" (no wildcards to worry about)
		if terms[1] == "unified_events" {
			return "infra:unifiedEvents", nil
		}

		terms = changeTerm(terms, "ingest", "infra")
		terms = changeTerm(terms, "unified_events", "unifiedEvents")
		return combineTermsIntoResource(terms...), nil
	case "license":
		if len(terms) == 1 {
			return "system:license", nil
		}
		// if len(terms) == 2 aka license:* or license:status
		return "system:status", nil
	case "nodemanagers":
		// "nodemanagers" -> "infra:nodeManagers"
		return combineTermsIntoResource(prefixTerms("infra", changeTerm(terms, "nodemanagers", "nodeManagers"))...),
			nil
	case "nodes":
		// "nodes" -> "infra:nodes"
		return combineTermsIntoResource(prefixTerms("infra", terms)...), nil
	case "secrets":
		// "secrets" -> "secrets:secrets"
		return combineTermsIntoResource(prefixTerms("secrets", terms)...), nil
	case "telemetry":
		// either telemetry:config or telemetry:* maps to system:config
		return "system:config", nil
	case "notifications":
		return resource, nil // unchanged
	case "service_groups":
		return "applications:serviceGroups", nil
	default:
		return "", fmt.Errorf("did not recognize base v1 resource term: %s", terms[0])
	}
}

func convertV1Cfgmgmt(terms []string) (string, error) {
	if terms[1] == "stats" {
		return "infra:nodes", nil
	}
	// cfgmgmt:nodes:{node_id}:runs -> infra:nodes:{node_id}
	// cfgmgmt:nodes:{node_id}:runs:{run_id}" -> infra:nodes:{node_id}
	if len(terms) >= 4 && terms[3] == "runs" {
		return combineTermsIntoResource("infra", "nodes", terms[2]), nil
	}
	// cfgmgmt:nodes:{node_id}:attribute -> infra:nodes:{node_id}
	if len(terms) >= 4 && terms[3] == "attribute" {
		return combineTermsIntoResource("infra", "nodes", terms[2]), nil
	}
	// cfgmgmt:nodes:{node_id}:* -> infra:nodes:{node_id}
	if len(terms) >= 3 && terms[2] == "node_id" {
		return combineTermsIntoResource("infra", "nodes", terms[2]), nil
	}
	terms = changeTerm(terms, "cfgmgmt", "infra")
	terms = changeTerm(terms, "marked-nodes", "markedNodes")
	return combineTermsIntoResource(terms...), nil
}

func convertV1Events(terms []string) (string, error) {
	// "events:*" -> "event:events"
	if len(terms) == 1 {
		return "event:events", nil
	}

	switch terms[1] {
	case "types", "tasks", "strings":
		return "event:events", nil
	default:
		return combineTermsIntoResource(changeTerm(terms, "event", "events")...), nil
	}
}

func changeTerm(terms []string, original, updated string) []string {
	for i, term := range terms {
		if term == original {
			terms[i] = updated
		}
	}
	return terms
}

func prefixTerms(prefix string, terms []string) []string {
	return append([]string{prefix}, terms...)
}

func deleteTerm(terms []string, original string) []string {
	for i, term := range terms {
		if term == original {
			terms = append(terms[:i], terms[i+1:]...)
		}
	}
	return terms
}

func combineTermsIntoResource(terms ...string) string {
	return strings.Join(terms, ":")
}

func convertV1Action(action string, resource string) ([]string, error) {
	terms := strings.Split(resource, ":")
	// introspect is a special case,
	// since there was only the "read" action for every different
	// API endpoint, we must assume them all.
	if terms[0] == "auth_introspection" {
		if action == "*" {
			return []string{"*"}, nil
		}
		return []string{"*:getAll", "*:getSome", "*:get"}, nil
	}

	// resource ingest:* is a special case to handle since we collapsed
	// that top-level ingest term into infra:ingest.
	// Only need to handle the * case specially though.
	if terms[0] == "ingest" && terms[1] == "*" {
		switch action {
		case "*":
			return []string{"infra:ingest:*"}, nil
		case "create":
			return []string{"infra:ingest:create", "infra:ingestUnifiedEvents:create"}, nil
		case "delete":
			return []string{"infra:ingest:delete"}, nil
		case "status":
			return []string{"infra:ingestStatus:get"}, nil
		}

		return []string{"*:getAll", "*:getSome", "*:get"}, nil
	}

	if terms[0] == "events" && action == "count" {
		return []string{"*:list"}, nil
	}

	// The rest of all all actions for other resources can
	// be generally mapped.
	switch action {
	case "read":
		return []string{"*:get", "*:list"}, nil
	case "create":
		return []string{"*:create"}, nil
	case "update":
		return []string{"*:update"}, nil
	case "delete":
		return []string{"*:delete"}, nil
	case "search":
		return []string{"*:list"}, nil
	case "rerun":
		return []string{"*:rerun"}, nil
	case "count":
		return []string{"*:get"}, nil
	case "start":
		return []string{"*:start"}, nil
	case "stop":
		return []string{"*:stop"}, nil // TODO: gdoc says stop -> ["*:start"] but pretty sure that is wrong xD
	case "configure":
		return []string{"*:update"}, nil
	case "mark-missing":
		return []string{"*:markMissing"}, nil
	case "apply":
		return []string{"*:apply"}, nil
	case "request":
		return []string{"*:request"}, nil
	case "list":
		return []string{"*:list"}, nil
	case "validate":
		return []string{"*:validate"}, nil
	case "export":
		return []string{"*:export"}, nil
	case "upload":
		return []string{"*:create"}, nil
	case "*":
		return []string{"*"}, nil
	default:
		// TODO: should we just warn the logs in this case and not crash the whole migration on the off
		// chance that the user created a policy where they mistyped an action name?
		return nil, fmt.Errorf("could not parse V1 action: %s", action)
	}
}
