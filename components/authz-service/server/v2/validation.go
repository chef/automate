package v2

import (
	"fmt"
	"regexp"

	constants "github.com/chef/automate/components/authz-service/constants"
	"github.com/pkg/errors"
)

var emptyOrWhitespaceOnlyRE = regexp.MustCompile(`^\s*$`)

func validateIDandName(id, name, resourceName string) error {
	err := validateEmptyOrWhiteSpaceOnly(id, "id", resourceName)
	if err != nil {
		return err
	}
	err = validateEmptyOrWhiteSpaceOnly(name, "name", resourceName)
	return err
}

func validateEmptyOrWhiteSpaceOnly(field, fieldName, resourceName string) error {
	if emptyOrWhitespaceOnlyRE.MatchString(field) {
		e := fmt.Sprintf("a %s %s is required and must contain at least one non-whitespace character", resourceName, fieldName)
		return errors.New(e)
	}
	return nil
}

func validateID(id, resourceName string) error {
	return validateEmptyOrWhiteSpaceOnly(id, "id", resourceName)
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
