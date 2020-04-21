package authz.introspection

import data.common

test_pair_matches_action_picks_up_INLINE_action {
	pair_matches_action[["polid", "statementid", {"action": "x"}]] with data.policies.polid.statements.statementid.actions as ["x"]
		 with input.pairs as [{"action": "x"}]
}

test_pair_matches_action_picks_up_ROLE_action {
	pair_matches_action[["polid", "statementid", {"action": "x"}]] with data.policies.polid.statements.statementid.role as "editor"
		 with data.roles.editor.actions as ["x"]
		 with input.pairs as [{"action": "x"}]
}

test_authorized_pair_returns_one_pair {
	authorized_pair[pair] with data.policies.polid as {"members": ["bob"], "statements": {"sid": {"effect": "allow", "actions": ["x"], "resources": ["y"]}}}
		 with input as {"subjects": ["bob"], "pairs": [{"action": "x", "resource": "y"}]}

	pair.action == "x"
	pair.resource == "y"
}

test_authorized_pair_returns_multiple_pairs {
	actual_pairs = authorized_pair with data.policies.polid as {
		"members": ["bob"],
		"statements": {
			"sid1": {"effect": "allow", "actions": ["x1"], "resources": ["y1"]},
			"sid2": {"effect": "allow", "actions": ["x2"], "resources": ["y2"]},
			"sid3": {"effect": "allow", "actions": ["x3"], "resources": ["y3"]},
		},
	}
		 with input as {
			"subjects": ["bob"],
			"pairs": [
				{"action": "x1", "resource": "y1"},
				{"action": "x2", "resource": "y2"},
			],
		}

	expected_pairs = {
		{"action": "x1", "resource": "y1"},
		{"action": "x2", "resource": "y2"},
	}

	expected_pairs == actual_pairs
}

test_authorized_pair_overrules_allowed_project_with_denied_project {
	actual_pairs = authorized_pair with data.policies.polid as {
		"members": ["bob"],
		"statements": {
			"sid1": {"effect": "allow", "actions": ["x1"], "resources": ["y1"]},
			"sid2": {"effect": "allow", "actions": ["x2"], "resources": ["y2"]},
			"sid3": {"effect": "allow", "actions": ["x3"], "resources": ["y3"]},
			"sid4": {"effect": "deny", "actions": ["x2"], "resources": ["y2"]},
		},
	}
		 with input as {
			"subjects": ["bob"],
			"pairs": [
				{"action": "x1", "resource": "y1"},
				{"action": "x2", "resource": "y2"},
				{"action": "x3", "resource": "y3"},
			],
		}

	expected_pairs = {
		{"action": "x1", "resource": "y1"},
		{"action": "x3", "resource": "y3"},
	}

	expected_pairs == actual_pairs
}

test_authorized_project_returns_multiple_projects_from_multiple_statements {
	actual_projects = authorized_project with data.policies.polid as {
		"members": ["bob"],
		"statements": {
			"sid1": {"effect": "allow", "projects": ["proj1", "proj2"]},
			"sid2": {"effect": "allow", "projects": ["proj1", "proj3"]},
			"sid3": {"effect": "allow", "projects": ["proj2"]},
		},
	}
		 with input.subjects as ["bob"]

	actual_projects == {"proj1", "proj2", "proj3"}
}

test_authorized_project_ignores_system_policies {
	actual_projects = authorized_project with data.policies as {
		"policy_id": {
			"members": ["user:local:bob"],
			"statements": {
				"sid1": {"effect": "allow", "projects": ["proj1", "proj2"]},
				"sid2": {"effect": "allow", "projects": ["proj1", "proj3"]},
				"sid3": {"effect": "allow", "projects": ["proj2"]},
			},
		},
		"policy_id2": {
			"type": "system",
			"members": ["user:local:*"],
			"statements": {"sid1": {"effect": "allow", "projects": [common.const_all_projects]}},
		},
	}
		 with input.subjects as ["user:local:bob"]

	actual_projects == {"proj1", "proj2", "proj3"}
}

# As long as some statement allows a project, it is possible for the user to see something there
test_authorized_project_includes_project_both_allowed_and_denied {
	actual_projects = authorized_project with data.policies.polid as {
		"members": ["bob"],
		"statements": {
			"sid1": {"effect": "allow", "projects": ["proj1"]},
			"sid2": {"effect": "deny", "projects": ["proj1"]},
		},
	}
		 with input.subjects as ["bob"]

	actual_projects == {"proj1"}
}

test_authorized_project_includes_allowed_projects_overlapping_denied_projects {
	actual_projects = authorized_project with data.policies.polid as {
		"members": ["bob"],
		"statements": {
			"sid1": {"effect": "allow", "projects": ["proj1", "proj2"]},
			"sid2": {"effect": "deny", "projects": ["proj1", "proj3"]},
		},
	}
		 with input.subjects as ["bob"]

	actual_projects == {"proj1", "proj2"}
}

test_authorized_project_ignores_denied_project_that_is_disjoint {
	actual_projects = authorized_project with data.policies.polid as {
		"members": ["bob"],
		"statements": {
			"sid1": {"effect": "allow", "projects": ["proj1"]},
			"sid2": {"effect": "deny", "projects": ["proj3"]},
		},
	}
		 with input.subjects as ["bob"]

	actual_projects == {"proj1"}
}
