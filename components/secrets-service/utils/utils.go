package utils

import (
	"regexp"
)

// Ensuring it only has valid UUID characters
// TODO: unit tests
func IsSafeUUID(uuid string) bool {
	r := regexp.MustCompile("^[a-fA-F0-9-]{36}$")
	return r.MatchString(uuid)
}
