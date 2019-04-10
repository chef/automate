package authz

test_has_action_picks_up_action_from_policy_data {
	has_action.polid with data.policies as {"polid": {"action": "x"}}
		 with input.action as "x"
}

test_has_resource_picks_up_resource_from_policy_data {
	has_resource.polid with data.policies as {"polid": {"resource": "y"}}
		 with input.resource as "y"
}

test_has_subject_picks_up_first_subject_from_policy_data {
	has_subject.polid with data.policies as {"polid": {"subjects": ["z"]}}
		 with input.subjects as ["z"]
}

test_has_subject_picks_up_second_subject_from_policy_data {
	has_subject.polid with data.policies as {"polid": {"subjects": ["z0", "z1"]}}
		 with input.subjects as ["z1"]
}

test_has_subject_picks_up_second_subject_from_policy_data_and_input {
	has_subject.polid with data.policies as {"polid": {"subjects": ["z0", "z1"]}}
		 with input.subjects as ["z3", "z1"]
}

test_action_matches_direct_match {
	action_matches("x", "x")
}

test_action_matches_wildcard {
	action_matches("anything", "*")
}

test_deny_trumps_allow {
	not authorized with data.deny as true
		 with data.allow as true
}

test_authorized_defaults_to_false {
	not authorized
}

test_allow_matches_all_properties_and_effect_allow {
	allow with data.policies.polid as {"effect": "allow", "subjects": ["x"], "action": "y", "resource": "z"}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z"}
}

test_deny_matches_all_properties_and_effect_deny {
	deny with data.policies.polid as {"effect": "deny", "subjects": ["x"], "action": "y", "resource": "z"}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z"}
}
