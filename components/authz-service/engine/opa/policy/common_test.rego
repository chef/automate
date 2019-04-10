package common

test_resource_matches_exact_simple {
	resource_matches("compliance", "compliance")
}

test_resource_matches_exact_01 {
	resource_matches("compliance:profiles:foobee", "compliance:profiles:foobee")
}

test_resource_matches_exact_02 {
	resource_matches("cfgmgmt:nodes:nodeId:runs", "cfgmgmt:nodes:nodeId:runs")
}

test_resource_matches_exact_03 {
	resource_matches("cfgmgmt:nodes:nodeId:runs:runId", "cfgmgmt:nodes:nodeId:runs:runId")
}

test_resource_matches_wildcard_namespace_01 {
	resource_matches("compliance", "*")
}

test_resource_matches_wildcard_namespace_02 {
	resource_matches("compliance:jobs", "*")
}

test_resource_matches_wildcard_namespace_03 {
	resource_matches("compliance:jobs:foobear", "*")
}

test_resource_matches_wildcard_name {
	resource_matches("compliance:profiles", "compliance:*")
}

test_resource_matches_wildcard_name_one_further_sections {
	resource_matches("compliance:jobs", "compliance:*")
}

test_resource_matches_wildcard_name_two_further_sections {
	resource_matches("compliance:jobs:foobear", "compliance:*")
}

test_resource_matches_wildcard_name_matching_further_sections {
	resource_matches("cfgmgmt:nodes:nodeId:runs:runId", "cfgmgmt:nodes:*")
}

test_resource_matches_wildcard_name_matching_last_sections {
	resource_matches("cfgmgmt:nodes:nodeId:runs:runId", "cfgmgmt:nodes:nodeId:runs:*")
}
