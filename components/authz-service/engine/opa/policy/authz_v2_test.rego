package authz_v2

import data.common

###############  has_action  ########################################

test_has_action_picks_up_INLINE_action {
	has_action.statementid with data.statements.statementid.actions as ["x"]
		 with input.action as "x"
}

test_has_action_ignores_other_inline_action {
	not has_action.statementid with data.statements.statementid.actions as ["y"]
		 with input.action as "x"
}

test_has_action_picks_up_ROLE_action {
	has_action.statementid with data.statements.statementid.role as "editor"
		 with data.roles.editor.actions as ["x"]
		 with input.action as "x"
}

test_has_action_ignores_same_action_from_different_role {
	not has_action.statementid with data.statements.statementid.role as "editor"
		 with data.roles.viewer.actions as ["x"]
		 with input.action as "x"
}

test_has_action_ignores_same_inline_action_from_different_statement {
	not has_action.statement1 with data.statements as {
		{"statement1": {"action": ["y"]}},
		{"statement2": {"action": ["x"]}},
	}
		 with input.action as "x"
}

test_has_action_ignores_same_role_action_from_different_statement {
	not has_action.statement1 with data.statements as {
		{"statement1": {"role": "editor"}},
		{"statement2": {"role": "viewer"}},
	}
		 with data.roles as {
			"editor": {"actions": ["y"]},
			"viewer": {"actions": ["x"]},
		}
		 with input.action as "x"
}

###############  has_resource #######################################

test_has_resource_picks_up_resource_from_policy_statement_data {
	has_resource.statementid with data.statements.statementid.resources as ["y"]
		 with input.resource as "y"
}

###############  has_member #########################################

test_has_member_picks_up_member_from_policy_data {
	has_member.statementid with data.statements.statementid.members as ["z"]
		 with input.subjects as ["z"]
}

###############  has_project ########################################

test_has_project_matches_policy_statement_when_input_project_list_matches {
	has_project[[project, "sid"]] with data.statements.sid.projects as ["a", "z"]
		 with input.projects as ["z"]

	project == "z"
}

test_has_project_ignores_input_with_different_project {
	project := {project_id | has_project[[project_id, "sid"]] with data.statements.sid.projects as ["a"]
		 with input.projects as ["z"]}

	count(project) == 0
}

test_has_project_matches_policy_statement_with_wildcard_with_some_input_projects {
	has_project[[project, "sid"]] with data.statements.sid.projects as [common.const_all_projects]
		 with input.projects as ["z"]

	project == "z"
}

###############  action_matches  ####################################

test_action_matches_direct_match {
	action_matches("svc:type:verb", "svc:type:verb")
}

test_action_matches_wildcard_match {
	action_matches("svc:type:verb", "*")
}

test_action_matches_service_match {
	action_matches("svc:type:verb", "svc:*")
}

test_action_matches_service_type_match {
	action_matches("svc:type:verb", "svc:type:*")
}

test_action_matches_verb_match {
	action_matches("svc:type:verb", "*:verb")
}

###############  base  ##############################################

test_deny_trumps_allow {
	not authorized with data.deny as true
		 with data.allow as true
}

test_authorized_defaults_to_false {
	not authorized
}

###############  allow/deny  #########################################

test_allow_matches_all_properties_with_INLINE_action_and_effect_allow {
	allow with data.statements.statementid as {"members": ["x"], "effect": "allow", "actions": ["y"], "resources": ["z"]}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z"}
}

test_allow_matches_all_properties_with_ROLE_action_and_effect_allow {
	allow with data.statements.statementid as {"members": ["x"], "effect": "allow", "role": "editor", "resources": ["z"]}
		 with data.roles.editor.actions as ["y"]
		 with input as {"subjects": ["x"], "action": "y", "resource": "z"}
}

test_deny_matches_all_properties_and_effect_deny {
	deny with data.statements.statementid as {"members": ["x"], "effect": "deny", "actions": ["y"], "resources": ["z"]}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z"}
}

###############  authorized  #########################################

test_not_authorized_when_only_not_matching_policies_with_effect_allow_are_present {
	not authorized with data.statements.statementid as {"members": ["x0"], "effect": "allow", "actions": ["y0"], "resources": ["z0"]}
		 with input as {"subjects": ["x1"], "action": "y1", "resource": "z1"}
}

test_authorized_when_one_among_a_group_of_members_is_present {
	authorized with data.statements.statementid as {"members": ["x0", "x1"], "effect": "allow", "actions": ["y0"], "resources": ["z0"]}
		 with input as {"subjects": ["x1"], "action": "y0", "resource": "z0"}
}

test_authorized_when_not_all_subjects_are_present_as_members {
	authorized with data.statements.statementid as {"members": ["x1"], "effect": "allow", "actions": ["y0"], "resources": ["z0"]}
		 with input as {"subjects": ["x1", "x2"], "action": "y0", "resource": "z0"}
}

test_authorized_when_not_matching_policy_with_effect_deny_is_present {
	authorized with data.statements.statementid as {"members": ["x"], "effect": "allow", "actions": ["y"], "resources": ["z"]}
		 with data.statements.statement1 as {"members": ["x0"], "effect": "deny", "actions": ["y0"], "resources": ["z0"]}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z"}
}

test_not_authorized_when_any_matching_policy_with_effect_deny_is_present {
	not authorized with data.statements.statement0 as {"members": ["x"], "effect": "allow", "actions": ["y"], "resources": ["z"]}
		 with data.statements.statement1 as {"members": ["x"], "effect": "deny", "actions": ["y"], "resources": ["z"]}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z"}
}

###############  authorized_project  #########################################

test_authorized_project_matches_single_input_project {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.statements.statementid as {"members": ["x"], "effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p1", "p2"]}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1"]}

	actual_projects == {"p1"}
}

test_authorized_project_matches_all_multiple_input_projects {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.statements.statementid as {"members": ["x"], "effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p1", "p2", "p3"]}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p3"]}

	actual_projects == {"p1", "p3"}
}

test_authorized_project_matches_some_multiple_input_projects {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.statements.statementid as {"members": ["x"], "effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p1", "p3"]}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p3", "p5"]}

	actual_projects == {"p1", "p3"}
}

test_authorized_project_returns_none_when_projects_do_not_match {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.statements as {
			"sid-1": {"members": ["x"], "effect": "deny", "role": "operator", "resources": ["*"], "projects": ["p1", "p2"]},
			"sid-2": {"members": ["x"], "effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p3"]},
		}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p4"]}

	actual_projects == set()
}

test_authorized_project_returns_all_input_projects_if_only_wildcard_statement_present {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.statements.statementid as {"members": ["x"], "effect": "allow", "role": "operator", "resources": ["*"], "projects": [common.const_all_projects]}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p3"]}

	actual_projects == {"p1", "p3"}
}

test_authorized_project_returns_all_input_projects_when_projects_mixed_with_wildcard_statement {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.statements as {
			"sid-1": {"members": ["x"], "effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p1", "p3", "p9"]},
			"sid-2": {"members": ["x"], "effect": "allow", "role": "operator", "resources": ["*"], "projects": [common.const_all_projects]},
		}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p3", "p5"]}

	actual_projects == {"p1", "p3", "p5"}
}

test_authorized_project_real_data {
	actual_projects = authorized_project with data.statements as {
		"sid1": {"members": ["team:local:viewers"], "effect": "allow", "actions": ["infra:*:list"], "resources": ["*"], "projects": ["project-p1", "project-p2"]},
		"sid2": {"members": ["team:local:viewers"], "effect": "allow", "actions": ["infra:ingest:create"], "resources": ["*"], "projects": ["project-p3"]},
	}
		 with input as {"subjects": ["team:local:viewers"], "action": "infra:ingest:create", "resource": "infra:nodes:52", "projects": ["project-p3"]}

	actual_projects == {"project-p3"}
}

test_authorized_project_denies_single_input_project_overruling_allow {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.statements as {
			"sid-1": {"members": ["x"], "effect": "deny", "role": "operator", "resources": ["*"], "projects": ["p1"]},
			"sid-2": {"members": ["x"], "effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p1"]},
		}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1"]}

	actual_projects == set()
}

test_authorized_project_denies_multiple_input_projects_where_all_allowed_projects_are_denied {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.statements as {
			"sid-1": {"members": ["x"], "effect": "deny", "role": "operator", "resources": ["*"], "projects": ["p1", "p2", "p3"]},
			"sid-2": {"members": ["x"], "effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p3"]},
		}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p3"]}

	actual_projects == set()
}

test_authorized_project_denies_multiple_input_projects_where_some_allowed_projects_denied {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.statements as {
			"sid-1": {"members": ["x"], "effect": "deny", "role": "operator", "resources": ["*"], "projects": ["p3"]},
			"sid-2": {"members": ["x"], "effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p1", "p2", "p3"]},
		}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p3"]}

	actual_projects == {"p1"}
}

test_authorized_project_denies_multiple_input_projects_where_some_denied_some_not_allowed {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.statements as {
			"sid-1": {"members": ["x"], "effect": "deny", "role": "operator", "resources": ["*"], "projects": ["p3"]},
			"sid-2": {"members": ["x"], "effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p1", "p3"]},
		}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p3", "p5"]}

	actual_projects == {"p1"}
}

test_authorized_project_deny_real_data {
	actual_projects = authorized_project with data.statements as {
		"sid1": {"members": ["team:local:viewers"], "effect": "allow", "actions": ["infra:*:list"], "resources": ["*"], "projects": ["project-p1", "project-p2"]},
		"sid2": {"members": ["team:local:viewers"], "effect": "deny", "actions": ["infra:ingest:create"], "resources": ["*"], "projects": ["project-p1"]},
	}
		 with input as {"subjects": ["team:local:viewers"], "action": "infra:ingest:create", "resource": "infra:nodes:52", "projects": ["project-p1"]}

	actual_projects == set()
}

test_authorized_project_returns_no_projects_when_all_projects_denied {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.statements as {
			"sid-1": {"members": ["x"], "effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p1", "p3"]},
			"sid-2": {"members": ["x"], "effect": "deny", "role": "operator", "resources": ["*"], "projects": [common.const_all_projects]},
		}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p3", "p5"]}

	actual_projects == set()
}

test_authorized_project_matches_only_allowed_projects_when_some_projects_denied {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.statements as {
			"sid-1": {"members": ["x"], "effect": "deny", "role": "operator", "resources": ["*"], "projects": ["p1", "p3"]},
			"sid-2": {"members": ["x"], "effect": "allow", "role": "operator", "resources": ["*"], "projects": [common.const_all_projects]},
		}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p2", "p3"]}

	actual_projects == {"p2"}
}
