package rule_mappings

import data.rules

rules_for_project[result] {
	result := data.rules[input.project_id][_]
}

rules_for_all_projects[result] {
	result := data.rules
}
