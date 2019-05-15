package authz_v2.introspection

import data.common

test_pair_matches_action_picks_up_INLINE_action {
	pair_matches_action[["statementid", {"action": "x"}]] with data.statements.statementid.actions as ["x"]
		 with input.pairs as [{"action": "x"}]
}

test_pair_matches_action_picks_up_ROLE_action {
	pair_matches_action[["statementid", {"action": "x"}]] with data.statements.statementid.role as "editor"
		 with data.roles.editor.actions as ["x"]
		 with input.pairs as [{"action": "x"}]
}

test_authorized_pair_returns_one_pair {
	authorized_pair[pair] with data.statements.sid as {"members": ["bob"], "effect": "allow", "actions": ["x"], "resources": ["y"]}
		 with input as {"subjects": ["bob"], "pairs": [{"action": "x", "resource": "y"}]}

	pair.action == "x"
	pair.resource == "y"
}

test_authorized_pair_returns_multiple_pairs {
	actual_pairs = authorized_pair with data.statements as {
		"sid1": {"members": ["bob"], "effect": "allow", "actions": ["x1"], "resources": ["y1"]},
		"sid2": {"members": ["bob"], "effect": "allow", "actions": ["x2"], "resources": ["y2"]},
		"sid3": {"members": ["bob"], "effect": "allow", "actions": ["x3"], "resources": ["y3"]},
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
	actual_pairs = authorized_pair with data.statements as {
		"sid1": {"members": ["bob"], "effect": "allow", "actions": ["x1"], "resources": ["y1"]},
		"sid2": {"members": ["bob"], "effect": "allow", "actions": ["x2"], "resources": ["y2"]},
		"sid3": {"members": ["bob"], "effect": "allow", "actions": ["x3"], "resources": ["y3"]},
		"sid4": {"members": ["bob"], "effect": "deny", "actions": ["x2"], "resources": ["y2"]},
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
	actual_projects = authorized_project with data.statements as {
		"sid1": {"members": ["bob"], "effect": "allow", "actions": ["x1"], "resources": ["y1"], "projects": ["proj1", "proj2"]},
		"sid2": {"members": ["bob"], "effect": "allow", "actions": ["x2"], "resources": ["y2"], "projects": ["proj1", "proj3"]},
		"sid3": {"members": ["bob"], "effect": "allow", "actions": ["x3"], "resources": ["y3"], "projects": ["proj2"]},
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

test_authorized_project_ignores_system_policies {
	actual_projects = authorized_project with data.statements as {
		"sid1": {"members": ["user:local:bob"], "effect": "allow", "actions": ["x1"], "resources": ["y1"], "projects": ["proj1", "proj2"]},
		"sid2": {"members": ["user:local:bob"], "effect": "allow", "actions": ["x2"], "resources": ["y2"], "projects": ["proj1", "proj3"]},
		"sid3": {"members": ["user:local:bob"], "effect": "allow", "actions": ["x3"], "resources": ["y3"], "projects": ["proj2"]},
		"sid4": {"type": "system", "members": ["user:local:*"], "effect": "allow", "actions": ["x1"], "resources": ["y1"], "projects": [common.const_all_projects]},
	}
		 with input as {
			"subjects": ["user:local:bob"],
			"pairs": [
				{"action": "x1", "resource": "y1"},
				{"action": "x2", "resource": "y2"},
			],
		}

	actual_projects == {"proj1", "proj2", "proj3"}
}

test_authorized_project_include_all_allowed_policies_and_omits_denied_policies {
	actual_projects = authorized_project with data.statements as {
		# Allowed with single project
		"sid1": {"members": ["bob"], "effect": "allow", "actions": ["x1"], "resources": ["y1"], "projects": ["proj2"]},
		# Allowed with multiple projects
		"sid2": {"members": ["bob"], "effect": "allow", "actions": ["x2"], "resources": ["y2"], "projects": ["proj1", "proj3"]},
		# Allowed with some overlapping projects
		"sid3": {"members": ["bob"], "effect": "allow", "actions": ["x2"], "resources": ["y2"], "projects": ["proj1", "proj2", "proj10"]},
		# Denied on allowed action/resource and same project
		"sid4": {"members": ["bob"], "effect": "deny", "actions": ["x1"], "resources": ["y1"], "projects": ["proj1"]},
		# Denied on allowed action/resource and different project
		"sid5": {"members": ["bob"], "effect": "deny", "actions": ["x1"], "resources": ["y1"], "projects": ["proj4"]},
		# Denied on some other action/resource and same project
		"sid6": {"members": ["bob"], "effect": "deny", "actions": ["x3"], "resources": ["y3"], "projects": ["proj2", "proj5"]},
		# Denied on some other action/resource and different project
		"sid7": {"members": ["bob"], "effect": "deny", "actions": ["x3"], "resources": ["y3"], "projects": ["proj6", "proj7", "proj8"]},
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
