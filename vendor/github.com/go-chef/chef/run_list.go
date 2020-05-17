package chef

// RunList represents the recipes and roles specified for a node or as part of a role.
type RunList []string

// EnvRunList represents the recipes and roles with environment specified for a node or as part of a role.
type EnvRunList map[string]RunList
