package stringutils

import (
	"regexp"
	"strings"
)

// Title returns a copy of the string s with all Unicode letters that begin words mapped to their title case.
// strings.Title has been deprecated since Go 1.18
func Title(s string) string {
	s = strings.ToLower(s)
	return strings.Title(s)
}

// TitleSplit splits the string s by sep and returns a copy of the string s with all Unicode letters that begin words mapped to their title case.
func TitleSplit(s string, sep string) string {
	split := strings.Split(s, sep)
	for i, v := range split {
		split[i] = Title(v)
	}
	return strings.Join(split, " ")
}

func IsNumeric(word string) bool {
	return regexp.MustCompile(`^[0-9]+$`).MatchString(word)
}
