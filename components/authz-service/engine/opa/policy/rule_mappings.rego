package rule_mappings

import data.project_rules

rules_for_project[result] {
	result := data.project_rules[input.project_id][_]
}

rules_for_all_projects[result] {
	result := data.project_rules
}
