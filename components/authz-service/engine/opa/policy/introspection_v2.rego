package authz_v2.introspection

import data.authz_v2 as authz
import data.common
import data.policies
import data.roles

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

match_pair[[effect, pair, pol_id, statement_id]] {
	effect := policies[pol_id].statements[statement_id].effect
	authz.has_member[pol_id]
	pair_matches_resource[[pol_id, statement_id, pair]]
	pair_matches_action[[pol_id, statement_id, pair]]
}

# Note: to return the subset of the authorized pairs of the provided input,
# our rules must "return" the 'pair' data.
allowed_pair[pair] = match_pair[["allow", pair, _, _]]

denied_pair[pair] = match_pair[["deny", pair, _, _]]

authorized_pair[pair] {
	allowed_pair[pair]
	not denied_pair[pair]
}

allowed_project[project] {
	project := policies[pol_id].statements[statement_id].projects[_]
	match_pair[["allow", pair, pol_id, statement_id]]
}

authorized_project[project] {
	allowed_project[project]
}
