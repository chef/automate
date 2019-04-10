package diagnostics

// Tag is a grouping identifier that can be used for filtering
type Tag string

// Tags is a group of tags
type Tags []Tag

// TagFilter represents the intent to filter tags
type TagFilter string

// IsNegate returns true if the intent is to negate the filter
func (t TagFilter) IsNegate() bool {
	return t[0] == '^' || t[0] == '~'
}

// Tag returns the Tag name of the filter as a Tag
// it will strip negations
func (t TagFilter) Tag() Tag {
	if t.IsNegate() {
		return Tag(t[1:])
	}
	return Tag(t)
}

// Matches returns true if there is a matching tag
func (tags Tags) Matches(tagToMatch Tag) bool {
	for _, tag := range tags {
		if tag == tagToMatch {
			return true
		}
	}
	return false
}

// StringArrayToTagFilters converts a string array to Tags
func StringArrayToTagFilters(strs []string) []TagFilter {
	tags := make([]TagFilter, len(strs))
	for i, t := range strs {
		tags[i] = TagFilter(t)
	}
	return tags
}
