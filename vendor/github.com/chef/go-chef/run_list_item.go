package chef

import (
	"fmt"
	"regexp"
)

const (
	qualifiedRecipe            string = `^recipe\[([^\]@]+)(@([0-9]+(\.[0-9]+){1,2}))?\]$`
	qualifiedRole              string = `^role\[([^\]]+)\]$`
	versionedUnqualifiedRecipe string = `^([^@]+)(@([0-9]+(\.[0-9]+){1,2}))$`
	falseFriend                string = `[\[\]]`
)

var (
	qualifiedRecipeRegexp            *regexp.Regexp = regexp.MustCompile(qualifiedRecipe)
	qualifiedRoleRegexp              *regexp.Regexp = regexp.MustCompile(qualifiedRole)
	versionedUnqualifiedRecipeRegexp *regexp.Regexp = regexp.MustCompile(versionedUnqualifiedRecipe)
	falseFriendRegexp                *regexp.Regexp = regexp.MustCompile(falseFriend)
)

// RunListItem external representation of a run list
// This module is a direct port of the Chef::RunList::RunListItem class
// see: https://github.com/chef/chef/blob/master/lib/chef/run_list/run_list_item.rb
type RunListItem struct {
	Name    string
	Type    string
	Version string
}

// NewRunListItem parses a single item from a run list and returns a structure
func NewRunListItem(item string) (rli RunListItem, err error) {
	switch {
	case qualifiedRecipeRegexp.MatchString(item):
		// recipe[name]
		// recipe[name@1.0.0]
		rli.Type = "recipe"

		submatches := qualifiedRecipeRegexp.FindStringSubmatch(item)
		rli.Name = submatches[1]

		if len(submatches) > 2 {
			rli.Version = submatches[3]
		}
	case qualifiedRoleRegexp.MatchString(item):
		// role[role_name]
		rli.Type = "role"

		submatches := qualifiedRoleRegexp.FindStringSubmatch(item)
		rli.Name = submatches[1]
	case versionedUnqualifiedRecipeRegexp.MatchString(item):
		// recipe_name@1.0.0
		rli.Type = "recipe"
		submatches := versionedUnqualifiedRecipeRegexp.FindStringSubmatch(item)

		rli.Name = submatches[1]

		if len(submatches) > 2 {
			rli.Version = submatches[3]
		}
	case falseFriendRegexp.MatchString(item):
		// Recipe[recipe_name]
		// roles[role_name]
		err = fmt.Errorf("Invalid run-list item: %s", item)
		return RunListItem{}, err
	default:
		rli.Type = "recipe"
		rli.Name = item
	}

	return rli, nil
}

// String implements the String interface function
func (r RunListItem) String() (s string) {
	if r.Version != "" {
		s = fmt.Sprintf("%s[%s@%s]", r.Type, r.Name, r.Version)
	} else {
		s = fmt.Sprintf("%s[%s]", r.Type, r.Name)
	}

	return s
}

// IsRecipe Determines if the runlist item is a recipe
func (r RunListItem) IsRecipe() bool {
	return r.Type == "recipe"
}

// IsRole Determines if the runlist item is a role
func (r RunListItem) IsRole() bool {
	return r.Type == "role"
}
