package validate

import (
	"fmt"
	"regexp"
)

type hasID interface {
	GetId() string
}

type hasName interface {
	GetName() string
}

// EmptyOrWhitespaceOnlyRE is a regex that checks for blank or whitespace only strings
var EmptyOrWhitespaceOnlyRE = regexp.MustCompile(`^\s*$`)

// RequiredIDandName verifies that the ID and Name fields are not empty
func RequiredIDandName(obj interface {
	hasID
	hasName
}, resourceName string) error {
	err := RequiredID(obj, resourceName)
	if err != nil {
		return err
	}
	return RequiredName(obj, resourceName)
}

// RequiredField verifies that the given field is not empty
func RequiredField(field, fieldName, resourceName string) error {
	if EmptyOrWhitespaceOnlyRE.MatchString(field) {
		return fmt.Errorf("a %s %s is required and must contain at least one non-whitespace character", resourceName, fieldName)
	}
	return nil
}

// RequiredID verifies that the id is not empty
func RequiredID(obj hasID, resourceName string) error {
	return RequiredField(obj.GetId(), "id", resourceName)
}

// RequiredName verifies that the name is not empty
func RequiredName(obj hasName, resourceName string) error {
	return RequiredField(obj.GetName(), "name", resourceName)
}
