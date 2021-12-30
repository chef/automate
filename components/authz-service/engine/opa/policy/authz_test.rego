package authz

import data.common

###############  has_action  ########################################

test_has_action_picks_up_INLINE_action {
	has_action[["polid", "statementid"]] with data.policies.polid.statements.statementid.actions as ["x"]
		 with input.action as "x"
}

test_has_action_ignores_other_inline_action {
	not has_action[["polid", "statementid"]] with data.policies.polid.statements.statementid.actions as ["y"]
		 with input.action as "x"
}

test_has_action_picks_up_ROLE_action {
	has_action[["polid", "statementid"]] with data.policies.polid.statements.statementid.role as "editor"
		 with data.roles.editor.actions as ["x"]
		 with input.action as "x"
}

test_has_action_ignores_same_action_from_different_role {
	not has_action[["polid", "statementid"]] with data.policies.polid.statements.statementid.role as "editor"
		 with data.roles.viewer.actions as ["x"]
		 with input.action as "x"
}

test_has_action_ignores_same_inline_action_from_different_statement {
	not has_action[["polid", "statement1"]] with data.policies.polid.statements as {
		{"statement1": {"action": ["y"]}},
		{"statement2": {"action": ["x"]}},
	}
		 with input.action as "x"
}

test_has_action_ignores_same_role_action_from_different_statement {
	not has_action[["polid", "statement1"]] with data.policies.polid.statements as {
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
	has_resource[["polid", "statementid"]] with data.policies as {"polid": {"statements": {"statementid": {"resources": ["y"]}}}}
		 with input.resource as "y"
}

###############  has_member #########################################

test_has_member_picks_up_member_from_policy_data {
	has_member.polid with data.policies as {"polid": {"members": ["z"]}}
		 with input.subjects as ["z"]
}

###############  has_project ########################################

test_has_project_matches_policy_statement_when_input_project_list_matches {
	has_project[[project, "polid", "sid"]] with data.policies.polid as {"statements": {"sid": {"projects": ["a", "z"]}}}
		 with input.projects as ["z"]

	project == "z"
}

test_has_project_ignores_input_with_different_project {
	project := {project_id | has_project[[project_id, "polid", "sid"]] with data.policies.polid as {"statements": {"sid": {"projects": ["a"]}}}
		 with input.projects as ["z"]}

	count(project) == 0
}

test_has_project_matches_policy_statement_with_wildcard_with_some_input_projects {
	has_project[[project, "polid", "sid"]] with data.policies.polid as {"statements": {"sid": {"projects": [common.const_all_projects]}}}
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
	allow with data.policies.polid as {"members": ["x"], "statements": {"statementid": {"effect": "allow", "actions": ["y"], "resources": ["z"]}}}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z"}
}

test_allow_matches_all_properties_with_ROLE_action_and_effect_allow {
	allow with data.policies.polid as {"members": ["x"], "statements": {"statementid": {"effect": "allow", "role": "editor", "resources": ["z"]}}}
		 with data.roles.editor.actions as ["y"]
		 with input as {"subjects": ["x"], "action": "y", "resource": "z"}
}

test_deny_matches_all_properties_and_effect_deny {
	deny with data.policies.polid as {"members": ["x"], "statements": {"statementid": {"effect": "deny", "actions": ["y"], "resources": ["z"]}}}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z"}
}

###############  authorized  #########################################

test_not_authorized_when_only_not_matching_policies_with_effect_allow_are_present {
	not authorized with data.policies.polid as {"members": ["x0"], "statements": {"statementid": {"effect": "allow", "actions": ["y0"], "resources": ["z0"]}}}
		 with input as {"subjects": ["x1"], "action": "y1", "resource": "z1"}
}

test_authorized_when_one_among_a_group_of_members_is_present {
	authorized with data.policies.polid as {"members": ["x0", "x1"], "statements": {"statementid": {"effect": "allow", "actions": ["y0"], "resources": ["z0"]}}}
		 with input as {"subjects": ["x1"], "action": "y0", "resource": "z0"}
}

test_authorized_when_not_all_subjects_are_present_as_members {
	authorized with data.policies.polid as {"members": ["x1"], "statements": {"statementid": {"effect": "allow", "actions": ["y0"], "resources": ["z0"]}}}
		 with input as {"subjects": ["x1", "x2"], "action": "y0", "resource": "z0"}
}

test_authorized_when_not_matching_policy_with_effect_deny_is_present {
	authorized with data.policies.polid as {"members": ["x"], "statements": {"statementid": {"effect": "allow", "actions": ["y"], "resources": ["z"]}}}
		 with data.policies.polid1 as {"members": ["x0"], "statements": {"statementid1": {"effect": "deny", "actions": ["y0"], "resources": ["z0"]}}}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z"}
}

test_not_authorized_when_any_matching_policy_with_effect_deny_is_present {
	not authorized with data.policies.polid0 as {"members": ["x"], "statements": {"statementid0": {"effect": "allow", "actions": ["y"], "resources": ["z"]}}}
		 with data.policies.polid1 as {"members": ["x"], "statements": {"statementid1": {"effect": "deny", "actions": ["y"], "resources": ["z"]}}}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z"}
}

###############  authorized_project  #########################################

test_authorized_project_matches_single_input_project {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.policies.polid as {"members": ["x"], "statements": {"statementid": {"effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p1", "p2"]}}}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1"]}

	actual_projects == {"p1"}
}

test_authorized_project_matches_all_multiple_input_projects {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.policies.polid as {"members": ["x"], "statements": {"statementid": {"effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p1", "p2", "p3"]}}}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p3"]}

	actual_projects == {"p1", "p3"}
}

test_authorized_project_matches_some_multiple_input_projects {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.policies.polid as {"members": ["x"], "statements": {"statementid": {"effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p1", "p3"]}}}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p3", "p5"]}

	actual_projects == {"p1", "p3"}
}

test_authorized_project_returns_none_when_projects_do_not_match {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.policies.polid as {
			"members": ["x"],
			"statements": {
				"sid-1": {"effect": "deny", "role": "operator", "resources": ["*"], "projects": ["p1", "p2"]},
				"sid-2": {"effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p3"]},
			},
		}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p4"]}

	actual_projects == set()
}

test_authorized_project_returns_all_input_projects_if_only_wildcard_statement_present {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.policies.polid as {"members": ["x"], "statements": {"statementid": {"effect": "allow", "role": "operator", "resources": ["*"], "projects": [common.const_all_projects]}}}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p3"]}

	actual_projects == {"p1", "p3"}
}

test_authorized_project_returns_all_input_projects_when_projects_mixed_with_wildcard_statement {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.policies.polid as {
			"members": ["x"],
			"statements": {
				"sid-1": {"effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p1", "p3", "p9"]},
				"sid-2": {"effect": "allow", "role": "operator", "resources": ["*"], "projects": [common.const_all_projects]},
			},
		}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p3", "p5"]}

	actual_projects == {"p1", "p3", "p5"}
}

test_authorized_project_with_multiple_policies_returns_all_input_projects_due_to_wildcard {
	actual_projects = authorized_project with data.policies as {
		"pol1": {
			"members": ["x"],
			"statements": {"s1": {"effect": "allow", "actions": ["y"], "resources": ["*"], "projects": ["p1", "p2"]}},
		},
		"pol2": {
			"members": ["*"],
			"statements": {"s2": {"effect": "allow", "actions": ["iam:introspect:*"], "resources": ["*"], "projects": [common.const_all_projects]}},
		},
	}
		 with input as {"subjects": ["x"], "action": "iam:introspect:getAllProjects", "resource": "z", "projects": ["p1", "p2", "p3"]}

	actual_projects == {"p1", "p2", "p3"}
}

test_authorized_project_real_data {
	actual_projects = authorized_project with data.policies.polid as {
		"members": ["team:local:viewers"],
		"statements": {
			"sid1": {"effect": "allow", "actions": ["infra:*:list"], "resources": ["*"], "projects": ["project-p1", "project-p2"]},
			"sid2": {"effect": "allow", "actions": ["ingest:nodes:create"], "resources": ["*"], "projects": ["project-p3"]},
		},
	}
		 with input as {"subjects": ["team:local:viewers"], "action": "ingest:nodes:create", "resource": "ingest:nodes:52", "projects": ["project-p3"]}

	actual_projects == {"project-p3"}
}

test_authorized_project_denies_single_input_project_overruling_allow {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.policies.polid as {
			"members": ["x"],
			"statements": {
				"sid-1": {"effect": "deny", "role": "operator", "resources": ["*"], "projects": ["p1"]},
				"sid-2": {"effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p1"]},
			},
		}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1"]}

	actual_projects == set()
}

test_authorized_project_denies_multiple_input_projects_where_all_allowed_projects_are_denied {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.policies.polid as {
			"members": ["x"],
			"statements": {
				"sid-1": {"effect": "deny", "role": "operator", "resources": ["*"], "projects": ["p1", "p2", "p3"]},
				"sid-2": {"effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p3"]},
			},
		}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p3"]}

	actual_projects == set()
}

test_authorized_project_denies_multiple_input_projects_where_some_allowed_projects_denied {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.policies.polid as {
			"members": ["x"],
			"statements": {
				"sid-1": {"effect": "deny", "role": "operator", "resources": ["*"], "projects": ["p3"]},
				"sid-2": {"effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p1", "p2", "p3"]},
			},
		}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p3"]}

	actual_projects == {"p1"}
}

test_authorized_project_denies_multiple_input_projects_where_some_denied_some_not_allowed {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.policies.polid as {
			"members": ["x"],
			"statements": {
				"sid-1": {"effect": "deny", "role": "operator", "resources": ["*"], "projects": ["p3"]},
				"sid-2": {"effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p1", "p3"]},
			},
		}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p3", "p5"]}

	actual_projects == {"p1"}
}

test_authorized_project_deny_real_data {
	actual_projects = authorized_project with data.policies.polid as {
		"members": ["team:local:viewers"],
		"statements": {
			"sid1": {"effect": "allow", "actions": ["infra:*:list"], "resources": ["*"], "projects": ["project-p1", "project-p2"]},
			"sid2": {"effect": "deny", "actions": ["ingest:nodes:create"], "resources": ["*"], "projects": ["project-p1"]},
		},
	}
		 with input as {"subjects": ["team:local:viewers"], "action": "ingest:nodes:create", "resource": "ingest:nodes:52", "projects": ["project-p1"]}

	actual_projects == set()
}

test_authorized_project_returns_no_projects_when_all_projects_denied {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.policies.polid as {
			"members": ["x"],
			"statements": {
				"sid-1": {"effect": "allow", "role": "operator", "resources": ["*"], "projects": ["p1", "p3"]},
				"sid-2": {"effect": "deny", "role": "operator", "resources": ["*"], "projects": [common.const_all_projects]},
			},
		}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p3", "p5"]}

	actual_projects == set()
}

test_authorized_project_matches_only_allowed_projects_when_some_projects_denied {
	actual_projects = authorized_project with data.roles.operator.actions as ["y"]
		 with data.policies.polid as {
			"members": ["x"],
			"statements": {
				"sid-1": {"effect": "deny", "role": "operator", "resources": ["*"], "projects": ["p1", "p3"]},
				"sid-2": {"effect": "allow", "role": "operator", "resources": ["*"], "projects": [common.const_all_projects]},
			},
		}
		 with input as {"subjects": ["x"], "action": "y", "resource": "z", "projects": ["p1", "p2", "p3"]}

	actual_projects == {"p2"}
}

test_authorized_project_returning_single_value_with_one_project {
	actual_projects = authorized_project with data.roles.project.actions as ["iam:teams:list"]
		 with data.policies.polid1 as {
			"members": ["user:local:dave"],
			"statements": {"sid1": {"effect": "allow", "actions": ["*"], "resources": ["*"], "projects": ["foo-project"]}},
		}
		 with data.policies.polid2 as {
			"members": ["user:local:dave"],
			"statements": {"sid2": {"effect": "allow", "role": "project", "resources": ["*"], "projects": ["foo-project"]}},
		}
		 with input as {"subjects": ["user:local:dave"], "action": "iam:teams:list", "resource": "iam:teams", "projects": ["foo-project", "(unassigned)"]}

	actual_projects == {"foo-project"}
}

test_authorized_project_returning_single_value_with_two_projects {
	actual_projects = authorized_project with data.roles.project.actions as ["iam:teams:list"]
		 with data.policies.polid1 as {
			"members": ["user:local:dave"],
			"statements": {"sid1": {"effect": "allow", "actions": ["*"], "resources": ["*"], "projects": ["project-1"]}},
		}
		 with data.policies.polid2 as {
			"members": ["user:local:dave"],
			"statements": {"sid2": {"effect": "allow", "role": "project", "resources": ["*"], "projects": ["project-2"]}},
		}
		 with input as {"subjects": ["user:local:dave"], "action": "iam:teams:list", "resource": "iam:teams", "projects": ["project-2", "project-1", "(unassigned)"]}

	actual_projects == {"project-1", "project-2"}
}

test_authorized_project_returning_set_value {
	authorized_project["foo-project"] with data.roles.project.actions as ["iam:teams:list"]
		 with data.policies.polid1 as {
			"members": ["user:local:dave"],
			"statements": {"sid1": {"effect": "allow", "actions": ["*"], "resources": ["*"], "projects": ["foo-project"]}},
		}
		 with input as {"subjects": ["user:local:dave"], "action": "iam:teams:list", "resource": "iam:teams", "projects": ["foo-project", "(unassigned)"]}
}

# Each element in the result set when using indexing is a map of variable bindings and
# those are not de-duplicated at present (see https://github.com/open-policy-agent/opa/issues/429)
# This unit test just shows that duplicates might occur so the Go code needs to de-duplicate.
# The OPA folks are considering a change to remove duplicates in the future, though.

# It is not returning duplicates as per OPA version 0.27.1
# if in case it is returning duplicate then change below count to 2
# (see https://github.com/open-policy-agent/opa/blob/main/CHANGELOG.md#backwards-compatibility-5)
test_authorized_project_returning_multiple_values {
	count([p | authorized_project[p]; p == "foo-project"]) == 1 with data.roles.project.actions as ["iam:teams:list"]
		 with data.policies.polid1 as {
			"members": ["user:local:dave"],
			"statements": {"sid1": {"effect": "allow", "actions": ["*"], "resources": ["*"], "projects": ["foo-project"]}},
		}
		 with data.policies.polid2 as {
			"members": ["user:local:dave"],
			"statements": {"sid2": {"effect": "allow", "role": "project", "resources": ["*"], "projects": ["foo-project"]}},
		}
		 with input as {"subjects": ["user:local:dave"], "action": "iam:teams:list", "resource": "iam:teams", "projects": ["foo-project", "(unassigned)"]}
}
