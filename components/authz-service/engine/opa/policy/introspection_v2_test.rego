package authz_v2.introspection

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

test_authorized_project_returns_multiple_projects {
	actual_projects = authorized_project with data.policies.polid as {
		"members": ["bob"],
		"statements": {
			"sid1": {"effect": "allow", "actions": ["x1"], "resources": ["y1"], "projects": ["proj1", "proj2"]},
			"sid2": {"effect": "allow", "actions": ["x2"], "resources": ["y2"], "projects": ["proj1", "proj3"]},
			"sid3": {"effect": "allow", "actions": ["x3"], "resources": ["y3"], "projects": ["proj2"]},
		},
	}
		 with input as {
			"subjects": ["bob"],
			"pairs": [
				{"action": "x1", "resource": "y1"},
				{"action": "x2", "resource": "y2"},
			],
		}

	actual_projects == {"proj1", "proj2", "proj3"}
}

test_authorized_project_include_all_allowed_policies_and_omits_denied_policies {
	actual_projects = authorized_project with data.policies.polid as {
		"members": ["bob"],
		"statements": {
			# Allowed with single project
			"sid1": {"effect": "allow", "actions": ["x1"], "resources": ["y1"], "projects": ["proj2"]},
			# Allowed with multiple projects
			"sid2": {"effect": "allow", "actions": ["x2"], "resources": ["y2"], "projects": ["proj1", "proj3"]},
			# Allowed with some overlapping projects
			"sid3": {"effect": "allow", "actions": ["x2"], "resources": ["y2"], "projects": ["proj1", "proj2", "proj10"]},
			# Denied on allowed action/resource and same project
			"sid4": {"effect": "deny", "actions": ["x1"], "resources": ["y1"], "projects": ["proj1"]},
			# Denied on allowed action/resource and different project
			"sid5": {"effect": "deny", "actions": ["x1"], "resources": ["y1"], "projects": ["proj4"]},
			# Denied on some other action/resource and same project
			"sid6": {"effect": "deny", "actions": ["x3"], "resources": ["y3"], "projects": ["proj2", "proj5"]},
			# Denied on some other action/resource and different project
			"sid7": {"effect": "deny", "actions": ["x3"], "resources": ["y3"], "projects": ["proj6", "proj7", "proj8"]},
		},
	}
		 with input as {
			"subjects": ["bob"],
			"pairs": [
				{"action": "x1", "resource": "y1"},
				{"action": "x2", "resource": "y2"},
				{"action": "x3", "resource": "y3"},
				{"action": "x4", "resource": "y4"},
			],
		}

	actual_projects == {"proj1", "proj2", "proj3", "proj10"}
}
