package validate

import (
	"fmt"
	"regexp"

	constants "github.com/chef/automate/components/authz-service/constants"
	"github.com/pkg/errors"
)

type hasID interface {
	GetId() string
}

type hasName interface {
	GetName() string
}

type hasProjects interface {
	GetProjects() []string
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

// RequiredFieldsAndProjects verifies that all the required inputs are not empty
func RequiredFieldsAndProjects(obj interface {
	hasID
	hasName
	hasProjects
}, resourceName string) error {
	err := RequiredIDandName(obj, resourceName)
	if err != nil {
		return err
	}
	return RequiredProjects(obj.GetProjects())
}

// RequiredProjects verifies that the projects are not empty and do not include '(unassigned)'
func RequiredProjects(projects []string) error {
	for _, project := range projects {
		if EmptyOrWhitespaceOnlyRE.MatchString(project) {
			e := fmt.Sprintf("projects must contain at least one non-whitespace character")
			return errors.New(e)
		}
		if project == constants.UnassignedProjectID {
			return errors.Errorf("%q cannot explicitly be set. "+
				"If you wish to create an object in %q, you should pass no projects on creation.",
				constants.UnassignedProjectID, constants.UnassignedProjectID)
		}
	}
	return nil
}
