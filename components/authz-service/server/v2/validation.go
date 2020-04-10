package v2

import (
	"fmt"
	"regexp"

	constants "github.com/chef/automate/components/authz-service/constants"
	"github.com/pkg/errors"
)

var emptyOrWhitespaceOnlyRE = regexp.MustCompile(`^\s*$`)

func confirmRequiredIDandName(id, name, resourceName string) error {
	err := confirmRequiredField(id, "id", resourceName)
	if err != nil {
		return err
	}
	err = confirmRequiredField(name, "name", resourceName)
	return err
}

func confirmRequiredField(field, fieldName, resourceName string) error {
	if emptyOrWhitespaceOnlyRE.MatchString(field) {
		e := fmt.Sprintf("a %s %s is required and must contain at least one non-whitespace character", resourceName, fieldName)
		return errors.New(e)
	}
	return nil
}

func confirmRequiredID(id, resourceName string) error {
	return confirmRequiredField(id, "id", resourceName)
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
