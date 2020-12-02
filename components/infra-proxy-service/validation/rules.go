package validation

import (
	"fmt"
	"regexp"
)

// EmptyOrWhitespaceOnlyRE is a regex that checks for blank or whitespace only strings
var EmptyOrWhitespaceOnlyRE = regexp.MustCompile(`^\s*$`)

// RequiredField verifies that the given field is not empty
func RequiredField(field, fieldName, resourceName string) *FieldViolation {
	if EmptyOrWhitespaceOnlyRE.MatchString(field) {
		return &FieldViolation{
			Field:       fieldName,
			Description: fmt.Sprintf("%s %s is required and must contain at least one non-whitespace character", resourceName, fieldName),
		}
	}
	return nil
}
