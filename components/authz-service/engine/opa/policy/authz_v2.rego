package authz_v2

import data.common
import data.policies
import data.roles

default authorized = false

has_member[pol_id] {
	pol_sub := policies[pol_id].members[_]
	input_sub := input.subjects[_]
	common.subject_matches(input_sub, pol_sub)
}

has_resource[[pol_id, statement_id]] {
	statement_resource := policies[pol_id].statements[statement_id].resources[_]
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

has_action[[pol_id, statement_id]] {
	statement_action := policies[pol_id].statements[statement_id].actions[_]
	action_matches(input.action, statement_action)
}

has_action[[pol_id, statement_id]] {
	policies[pol_id].statements[statement_id].role = role_id
	roles[role_id].actions[_] = role_action
	action_matches(input.action, role_action)
}

has_project[[project, pol_id, statement_id]] {
	project := policies[pol_id].statements[statement_id].projects[_]
	project_matches(input.projects[_], project)
}

has_project[[project, pol_id, statement_id]] {
	project := policies[pol_id].statements[statement_id].projects[_]
	count(input.projects) == 0
}

has_project[[project, pol_id, statement_id]] {
	project := policies[pol_id].statements[statement_id].projects[_]
	not input.projects
}

project_matches(_, stored) {
	stored == "~~ALL-PROJECTS~~"
}

project_matches(in, stored) {
	stored == in
}

project_matches(in, _) {
	count(in) == 0
}

match[[effect, pol_id, statement_id]] {
	effect := policies[pol_id].statements[statement_id].effect
	has_member[pol_id]
	has_resource[[pol_id, statement_id]]
	has_action[[pol_id, statement_id]]
}

allow = match[["allow", _, _]]

deny = match[["deny", _, _]]

authorized {
	allow
	not deny
}

allowed_project[project] {
	match[["allow", pol_id, statement_id]]
	has_project[[project, pol_id, statement_id]]
}

authorized_project[project] {
	allowed_project[project]
}
