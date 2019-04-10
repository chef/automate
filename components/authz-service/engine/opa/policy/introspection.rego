package authz.introspection

import data.authz
import data.common
import data.policies

pair_matches_resource[[pol_id, pair]] {
	policies[pol_id] = pol
	input.pairs[_] = pair
	common.resource_matches(pair.resource, pol.resource)
}

pair_matches_action[[pol_id, pair]] {
	input.pairs[_] = pair
	authz.action_matches(pair.action, policies[pol_id].action)
}

# Note: to return the subset of the authorized pairs of the provided input,
# our rules must "return" the 'pair' data.
allowed_pair[pair] {
	policies[pol_id].effect = "allow"
	authz.has_subject[pol_id]
	pair_matches_resource[[pol_id, pair]]
	pair_matches_action[[pol_id, pair]]
}

denied_pair[pair] {
	policies[pol_id].effect = "deny"
	authz.has_subject[pol_id]
	pair_matches_resource[[pol_id, pair]]
	pair_matches_action[[pol_id, pair]]
}

authorized_pair[pair] {
	allowed_pair[pair]
	not denied_pair[pair]
}
