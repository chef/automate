package legacy

import (
	"context"
	"database/sql"
	"fmt"
	"strings"

	storage_errors "github.com/chef/automate/components/authz-service/storage"
	constants_v1 "github.com/chef/automate/components/authz-service/storage/postgres/migration/legacy/constants/v1"
	constants_v2 "github.com/chef/automate/components/authz-service/storage/postgres/migration/legacy/constants/v2"
	"github.com/chef/automate/lib/logger"
	uuid "github.com/chef/automate/lib/uuid4"
	"github.com/pkg/errors"
)

// MigrateToV2 inserts needed IAM v2 resources into the db and
// migrates any valid v1 policies
func MigrateToV2(ctx context.Context, db *sql.DB) error {
	l, err := logger.NewLogger("text", "info")
	if err != nil {
		return errors.Wrap(err, "could not initialize logger")
	}
	for _, role := range defaultRoles() {
		if err := createRole(ctx, db, &role); err != nil {
			return errors.Wrapf(err,
				"could not create default role with ID: %s", role.ID)
		}
	}

	for _, pol := range v2DefaultPolicies() {
		if _, err := createV2Policy(ctx, db, &pol); err != nil {
			return errors.Wrapf(err,
				"could not create default policy with ID: %s", pol.ID)
		}
	}

	errs, err := migrateV1Policies(ctx, db)
	if err != nil {
		return errors.Wrapf(err, "migrate v1 policies")
	}

	reports := []string{}
	for _, e := range errs {
		reports = append(reports, e.Error())
	}
	if len(reports) != 0 {
		l.Infof("invalid v1 policies could not be migrated: %v", reports)
	}

	return nil
}

/*
COPY PASTA DATABASE CODE

The below is code we've copied from our database functionality because we need
versions of the database functions needed for the migrations that do not change.
This is because this migration is run at a single point in time as part of the schema
upgrades. So this code need to be compatible with a specific schema version that never changes.
*/

// migrateV1Policies has two error returns: the second one is the ordinary,
// garden-variety, "something went wrong, I've given up" signal; the first one
// serves as an aggregate of errors that happened attempting to convert and
// store individual (custom) policies.
func migrateV1Policies(ctx context.Context, db *sql.DB) ([]error, error) {
	pols, err := listPoliciesWithSubjects(ctx, db)
	if err != nil {
		return nil, errors.Wrap(err, "list v1 policies")
	}

	var errs []error
	for _, pol := range pols {
		adminTokenPolicy, err := checkForAdminTokenPolicy(pol)
		if err != nil {
			errs = append(errs, errors.Wrapf(err, "verify subjects %q for admin policy %q", pol.Subjects, pol.ID.String()))
			continue
		}
		if adminTokenPolicy != nil {
			if err := addTokenToAdminPolicy(ctx, adminTokenPolicy.Subjects[0], db); err != nil {
				errs = append(errs, errors.Wrapf(err, "adding members %q for admin policy %q", pol.Subjects, pol.ID.String()))
			}
			continue // don't migrate admin policies with single token
		}
		v2StoragePol, err := versionizeToV2(pol)
		if err != nil {
			// collect error
			errs = append(errs, errors.Wrapf(err, "convert v1 policy %q", pol.ID.String()))
			continue // nothing to create
		}
		if v2StoragePol == nil {
			continue // nothing to create
		}
		_, err = createV2Policy(ctx, db, v2StoragePol)
		switch err {
		case nil, storage_errors.ErrConflict: // ignore, continue
		default:
			errs = append(errs, errors.Wrapf(err, "store converted v1 policy %q", pol.ID.String()))
		}
	}
	return errs, nil
}

func versionizeToV2(pol *v1Policy) (*v2Policy, error) {
	wellKnown, err := isWellKnown(pol.ID)
	if err != nil {
		return nil, errors.Wrapf(err, "lookup v1 default policy %q", pol.ID.String())
	}

	if wellKnown {
		return legacyPolicyFromV1(pol)
	}
	return customPolicyFromV1(pol)
}

func addTokenToAdminPolicy(ctx context.Context, tok string, db *sql.DB) error {
	m, err := newV2Member(tok)
	if err != nil {
		return errors.Wrap(err, "format v2 member for admin team")
	}
	mems, err := addPolicyMembers(ctx, db, constants_v2.AdminPolicyID, []v2Member{m})
	if err != nil {
		return errors.Wrapf(err, "could not add members %q to admin policy", mems)
	}
	return nil
}

func checkForAdminTokenPolicy(pol *v1Policy) (*v1Policy, error) {
	if pol.Action == "*" && pol.Resource == "*" && len(pol.Subjects) == 1 && strings.HasPrefix(pol.Subjects[0], "token:") {
		return pol, nil
	}
	return nil, nil
}

func isWellKnown(id uuid.UUID) (bool, error) {
	defaultV1, err := v1DefaultPolicies()
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
func legacyPolicyFromV1(pol *v1Policy) (*v2Policy, error) {
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
		cfgmgmtStatement := newV2Statement(Allow, "", []string{},
			[]string{"*"}, []string{"infra:*"})
		cfgmgmtStatementDeny := newV2Statement(Deny, "", []string{},
			[]string{"*"}, []string{"infra:ingest:*"})
		member, err := newV2Member("user:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (cfgmgmt)")
		}
		cfgmgmtPolicy, err := newV2Policy(constants_v2.CfgmgmtPolicyID,
			"[Legacy] Infrastructure Automation Access",
			Custom, []v2Member{member},
			[]v2Statement{cfgmgmtStatement, cfgmgmtStatementDeny}, noProjects)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (cfgmgmt)")
		}
		return &cfgmgmtPolicy, nil
	}

	if pol.ID.String() == constants_v1.ComplianceWildcardPolicyID {
		complianceStatement := newV2Statement(Allow, "", []string{},
			[]string{"*"}, []string{"compliance:*"})
		member, err := newV2Member("user:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (compliance)")
		}
		compliancePolicy, err := newV2Policy(constants_v2.CompliancePolicyID,
			"[Legacy] Compliance Access",
			Custom, []v2Member{member}, []v2Statement{complianceStatement}, noProjects)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (cfgmgmt)")
		}
		return &compliancePolicy, nil
	}

	if _, found := v1EventFeedPolicies[pol.ID.String()]; found {
		eventsStatement := newV2Statement(Allow, "", []string{},
			[]string{"*"}, []string{"event:*"})
		member, err := newV2Member("user:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (events)")
		}
		eventsPolicy, err := newV2Policy(constants_v2.EventsPolicyID,
			"[Legacy] Events Access",
			Custom, []v2Member{member}, []v2Statement{eventsStatement}, noProjects)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (events)")
		}
		return &eventsPolicy, nil
	}

	if pol.ID.String() == constants_v1.IngestWildcardPolicyID {
		ingestStatement := newV2Statement(Allow, "", []string{},
			[]string{"*"}, []string{"infra:ingest:*"})
		member, err := newV2Member("token:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (ingest)")
		}

		ingestPolicy, err := newV2Policy(constants_v2.LegacyIngestPolicyID,
			"[Legacy] Ingest Access",
			Custom, []v2Member{member}, []v2Statement{ingestStatement}, noProjects)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (ingest)")
		}
		return &ingestPolicy, nil
	}

	if _, found := v1NodesPolicies[pol.ID.String()]; found {
		nodesStatement := newV2Statement(Allow, "", []string{},
			[]string{"*"}, []string{"infra:nodes:*"})
		member, err := newV2Member("user:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (nodes)")
		}
		nodesPolicy, err := newV2Policy(
			constants_v2.NodesPolicyID,
			"[Legacy] Nodes Access",
			Custom,
			[]v2Member{member},
			[]v2Statement{nodesStatement},
			noProjects,
		)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (nodes)")
		}
		return &nodesPolicy, nil
	}

	if _, found := v1NodeManagersPolicies[pol.ID.String()]; found {
		nodeManagersStatement := newV2Statement(Allow, "", []string{},
			[]string{"*"}, []string{"infra:nodeManagers:*"})
		member, err := newV2Member("user:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (nodemanagers)")
		}
		nodeManagersPolicy, err := newV2Policy(
			constants_v2.NodeManagersPolicyID,
			"[Legacy] Node Managers Access",
			Custom,
			[]v2Member{member},
			[]v2Statement{nodeManagersStatement},
			noProjects,
		)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (nodemanagers)")
		}
		return &nodeManagersPolicy, nil
	}

	if _, found := v1SecretsPolicies[pol.ID.String()]; found {
		secretsStatement := newV2Statement(Allow, "", []string{},
			[]string{"*"}, []string{"secrets:*"})
		member, err := newV2Member("user:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (secrets)")
		}
		secretsPolicy, err := newV2Policy(
			constants_v2.SecretsPolicyID,
			"[Legacy] Secrets Access",
			Custom,
			[]v2Member{member},
			[]v2Statement{secretsStatement},
			noProjects,
		)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (secrets)")
		}
		return &secretsPolicy, nil
	}

	if pol.ID.String() == constants_v1.TelemetryConfigPolicyID {
		telemetryStatement := newV2Statement(Allow, "", []string{},
			[]string{"*"}, []string{"system:telemetryConfig:*"})
		member, err := newV2Member("user:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (telemetry)")
		}
		telemetryPolicy, err := newV2Policy(
			constants_v2.TelemetryPolicyID,
			"[Legacy] Telemetry Access",
			Custom,
			[]v2Member{member},
			[]v2Statement{telemetryStatement},
			noProjects,
		)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (telemetry)")
		}
		return &telemetryPolicy, nil
	}

	if _, found := v1ComplianceTokenPolicies[pol.ID.String()]; found {
		complianceTokenStatement := newV2Statement(Allow, "", []string{},
			[]string{"*"}, []string{"compliance:profiles:*"})
		member, err := newV2Member("token:*")
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member (compliance token)")
		}
		complianceTokenPolicy, err := newV2Policy(
			constants_v2.ComplianceTokenPolicyID,
			"[Legacy] Compliance Profile Access",
			Custom,
			[]v2Member{member},
			[]v2Statement{complianceTokenStatement},
			noProjects,
		)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 policy (compliance token)")
		}
		return &complianceTokenPolicy, nil
	}

	return nil, errors.New("unknown \"well-known\" policy")
}

func customPolicyFromV1(pol *v1Policy) (*v2Policy, error) {
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
	statement := newV2Statement(Allow, "", []string{}, []string{resource}, action)

	members := make([]v2Member, len(pol.Subjects))
	for i, subject := range pol.Subjects {
		memberInt, err := newV2Member(subject)
		if err != nil {
			return nil, errors.Wrap(err, "format v2 member")
		}
		members[i] = memberInt
	}

	policy, err := newV2Policy(
		pol.ID.String(),
		name,
		Custom,
		members,
		[]v2Statement{statement},
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
