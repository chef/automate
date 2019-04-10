package authz

import data.common
import data.policies # our policies

default authorized = false

# Subjects, unlike resources and actions, are provided as an array.
# This checks that at least one of the input subjects matches a subject in the policy.
has_subject[pol_id] {
	policies[pol_id].subjects[_] = pol_sub
	input.subjects[_] = input_sub
	common.subject_matches(input_sub, pol_sub)
}

has_resource[pol_id] {
	common.resource_matches(input.resource, policies[pol_id].resource)
}

has_action[pol_id] {
	action_matches(input.action, policies[pol_id].action)
}

action_matches(in, stored) {
	in = stored
}

action_matches(in, stored) {
	stored = "*"
}

allow {
	policies[pol_id].effect = "allow"
	has_subject[pol_id]
	has_resource[pol_id]
	has_action[pol_id]
}

deny {
	policies[pol_id].effect = "deny"
	has_subject[pol_id]
	has_resource[pol_id]
	has_action[pol_id]
}

# our policies are Disorderly, meaning Deny overrides Allow
# for further explanation: https://blog.openpolicyagent.org/orderly-versus-disorderly-policies-717475c23d2f
authorized {
	allow
	not deny
}
