package authz_v2

import data.common
import data.roles
import data.statements

default authorized = false

has_member[statement_id] {
	pol_sub := statements[statement_id].members[_]
	input_sub := input.subjects[_]
	common.subject_matches(input_sub, pol_sub)
}

has_resource[statement_id] {
	statement_resource := statements[statement_id].resources[_]
	common.resource_matches(input.resource, statement_resource)
}

no_wildcard(a) {
	contains(a, "*") == false
}

action_matches(in, stored) {
	no_wildcard(stored)
	in == stored
}

action_matches(in, stored) = action_match(split(stored, ":"), split(in, ":"))

action_match([service, "*"], [service, _, _]) = true

action_match([service, type, "*"], [service, type, _]) = true

action_match([service, "*", verb], [service, _, verb]) = true

action_match(["*", verb], [_, _, verb]) = true

action_match(["*"], _) = true

has_action[statement_id] {
	statement_action := statements[statement_id].actions[_]
	action_matches(input.action, statement_action)
}

has_action[statement_id] {
	statements[statement_id].role = role_id
	roles[role_id].actions[_] = role_action
	action_matches(input.action, role_action)
}

has_project[[project, statement_id]] {
	proj := statements[statement_id].projects[_]
	projects := project_matches(proj)
	project := projects[_]
}

project_matches(proj) = projects {
	proj == common.const_all_projects
	projects := input.projects
}

project_matches(proj) = projects {
	proj != common.const_all_projects
	proj = input.projects[_]
	projects := [proj]
}

match[[effect, statement_id]] {
	effect := statements[statement_id].effect
	has_member[statement_id]
	has_resource[statement_id]
	has_action[statement_id]
}

allow = match[["allow", _]]

deny = match[["deny", _]]

authorized {
	allow
	not deny
}

allowed_project[project] {
	match[["allow", statement_id]]
	has_project[[project, statement_id]]
}

denied_project[project] {
	match[["deny", statement_id]]
	has_project[[project, statement_id]]
}

authorized_project[project] {
	allowed_project[project]
	not denied_project[project]
}
