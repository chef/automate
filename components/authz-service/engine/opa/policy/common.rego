# Package common contains functions and rules that are shared
# between the authz and introspection rego logic
package common

const_all_projects = "~~ALL-PROJECTS~~"

#
# Variable expansion
#

no_variables(a) {
	contains(a, "${") == false
}

variables(a) {
	indexof(a, "${") < indexof(a, "}")
}

# Defines the function 'expand' which returns its value via the output variable
# 'expanded'.
# Note: currently, only expands the one variable we know: ${a2:username}.
expand(orig) = expanded {
	split(input.subjects[_], ":", ["user", _, username])
	expanded := replace(orig, "${a2:username}", username)
}

wildcard(a) {
	endswith(a, ":*")
}

# Check that it does not end with ":*" AND that it is not a solitary "*".
# Note: The latter is done so that we don't end up with 'input.resource = *'
# rules in our partial results.
# Note that we avoid "not", which hinders partial result optimizations, see
# https://github.com/open-policy-agent/opa/issues/709.
not_wildcard(a) {
	endswith(a, ":*") == false
	a != "*"
}

# This supports these business rules:
# (a) A wildcard may only occur in the last section.
# (b) A wildcard may not be combined with a prefix (e.g. cannot say "x:y:foo*").
# (c) A wildcard applies to the current section and any deeper sections
#     (e.g. "a:*" matches "a:b" and "a:b:c", etc.).
wildcard_match(a, b) {
	startswith(a, trim(b, "*"))
}

#
# Resource matching
#
resource_matches(in, stored) {
	no_variables(stored)
	not_wildcard(stored)
	in == stored
}

resource_matches(in, stored) {
	no_variables(stored)
	wildcard(stored)
	wildcard_match(in, stored)
}

resource_matches(in, stored) {
	variables(stored)
	not_wildcard(stored)
	in == expand(stored)
}

resource_matches(in, stored) {
	variables(stored)
	wildcard(stored)
	wildcard_match(in, expand(stored))
}

resource_matches(_, "*") = true

#
# Subject matching
#
subject_matches(in, stored) {
	not_wildcard(stored)
	in == stored
}

subject_matches(in, stored) {
	wildcard(stored)
	wildcard_match(in, stored)
}

subject_matches(_, "*") = true
