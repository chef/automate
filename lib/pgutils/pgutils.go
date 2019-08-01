package pgutils

import (
	"regexp"
	"strings"
)

func EscapeLiteralForPG(value string) string {
	value = strings.Replace(value, "'", "''", -1)
	value = strings.Replace(value, `\`, `\\`, -1)
	return value
}

func EscapeLiteralForPGPatternMatch(value string) string {
	value = EscapeLiteralForPG(value)
	value = strings.Replace(value, `_`, `\_`, -1)
	value = strings.Replace(value, `%`, `\%`, -1)
	return value
}

// Ensuring str only has the characters we support at the moment
// TODO: unit tests
func IsSqlSafe(str string) bool {
	r := regexp.MustCompile("^[a-zA-Z0-9\\._-]*$")
	return r.MatchString(str)
}
