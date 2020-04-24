package validate

import (
	"fmt"
	"regexp"

	constants "github.com/chef/automate/components/authz-service/constants"
	"github.com/pkg/errors"
)

// EmptyOrWhitespaceOnlyRE is a regex that checks for blank or whitespace only strings
var EmptyOrWhitespaceOnlyRE = regexp.MustCompile(`^\s*$`)

// RequiredIDandName verifies that the ID and Name fields are not empty
func RequiredIDandName(id, name, resourceName string) error {
	err := RequiredID(id, resourceName)
	if err != nil {
		return err
	}
	return RequiredName(name, resourceName)
}

// RequiredField verifies that the given field is not empty
func RequiredField(field, fieldName, resourceName string) error {
	if EmptyOrWhitespaceOnlyRE.MatchString(field) {
		return fmt.Errorf("a %s %s is required and must contain at least one non-whitespace character", resourceName, fieldName)
	}
	return nil
}

// RequiredID verifies that the id is not empty
func RequiredID(id, resourceName string) error {
	return RequiredField(id, "id", resourceName)
}

// RequiredName verifies that the name is not empty
func RequiredName(name, resourceName string) error {
	return RequiredField(name, "name", resourceName)
}

// RequiredFieldsAndProjects verifies that all the required inputs are not empty
func RequiredFieldsAndProjects(id string, name string, projectIDs []string, resourceName string) error {
	err := RequiredIDandName(id, name, resourceName)
	if err != nil {
		return err
	}
	return RequiredProjects(projectIDs)
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
