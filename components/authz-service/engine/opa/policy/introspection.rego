package authz.introspection

import data.authz as authz
import data.common
import data.policies
import data.roles

const_system_type = "system"

pair_matches_resource[[pol_id, statement_id, pair]] {
	policies[pol_id].statements[statement_id].resources[_] = statement_resource
	input.pairs[_] = pair
	common.resource_matches(pair.resource, statement_resource)
}

pair_matches_action[[pol_id, statement_id, pair]] {
	policies[pol_id].statements[statement_id].actions[_] = statement_action
	input.pairs[_] = pair
	authz.action_matches(pair.action, statement_action)
}

pair_matches_action[[pol_id, statement_id, pair]] {
	policies[pol_id].statements[statement_id].role = role_id
	roles[role_id].actions[_] = role_action
	input.pairs[_] = pair
	authz.action_matches(pair.action, role_action)
}

# This causes the has_member set to be generated and memoized once instead of
# for each policy. Moreover it prevents the backtracking from generating duplicate
# answers that have to be evaluated by the resource and action matchers.
has_member = authz.has_member

match_pair[[effect, pair, pol_id, statement_id]] {
	effect := policies[pol_id].statements[statement_id].effect
	has_member[pol_id]
	pair_matches_resource[[pol_id, statement_id, pair]]
	pair_matches_action[[pol_id, statement_id, pair]]
}

# Note: to return the subset of the authorized pairs of the provided input,
# our rules must "return" the 'pair' data.
allowed_pair[pair] {
	match_pair[["allow", pair, _, _]]
}

denied_pair[pair] {
	match_pair[["deny", pair, _, _]]
}

authorized_pair = allowed_pair - denied_pair

allowed_project[project] {
	project := policies[pol_id].statements[statement_id].projects[_]
	"allow" == policies[pol_id].statements[statement_id].effect
	authz.has_member[pol_id]
	not policies[pol_id].type == const_system_type
}

authorized_project[project] {
	allowed_project[project]
}
