package v2

import (
	"fmt"
	"regexp"

	constants "github.com/chef/automate/components/authz-service/constants"
	"github.com/pkg/errors"
)

var emptyOrWhitespaceOnlyRE = regexp.MustCompile(`^\s*$`)

func confirmRequiredIDandName(id, name, resourceName string) error {
	err := confirmRequiredID(id, resourceName)
	if err != nil {
		return err
	}
	return confirmRequiredName(name, resourceName)
}

func confirmRequiredField(field, fieldName, resourceName string) error {
	if emptyOrWhitespaceOnlyRE.MatchString(field) {
		return fmt.Errorf("a %s %s is required and must contain at least one non-whitespace character", resourceName, fieldName)
	}
	return nil
}

func confirmRequiredID(id, resourceName string) error {
	return confirmRequiredField(id, "id", resourceName)
}

func confirmRequiredName(name, resourceName string) error {
	return confirmRequiredField(name, "name", resourceName)
}

func confirmRequiredProjectID(projectID, resourceName string) error {
	return confirmRequiredField(projectID, "project_id", resourceName)
}

func validateRequiredFieldsAndProjects(id string, name string, projectIDs []string, resourceName string) error {
	err := confirmRequiredIDandName(id, name, resourceName)
	if err != nil {
		return err
	}
	return validateProjects(projectIDs)
}

func validateRequiredFieldsAndProjectsForPolicy(id string, name string, projectIDs []string) error {
	return validateRequiredFieldsAndProjects(id, name, projectIDs, "policy")
}

func validateRequiredFieldsAndProjectsForRole(id string, name string, projectIDs []string) error {
	return validateRequiredFieldsAndProjects(id, name, projectIDs, "role")
}

func validateProjects(projects []string) error {
	for _, project := range projects {
		if emptyOrWhitespaceOnlyRE.MatchString(project) {
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
