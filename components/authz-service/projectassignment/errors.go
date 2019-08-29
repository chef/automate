package projectassignment

import (
	"fmt"
)

// ProjectsMissingErr occurs when some of the projects in the project diff
// did not exist.
type ProjectsMissingErr struct {
	projectsMissing []string
}

func NewProjectsMissingError(projectsMissing []string) error {
	return &ProjectsMissingErr{projectsMissing: projectsMissing}
}

func (e *ProjectsMissingErr) Error() string {
	var errorStr string
	if len(e.projectsMissing) > 1 {
		errorStr = fmt.Sprintf("You attempted to modify projects for this object but some did not exist: %q", e.projectsMissing)
	} else {
		errorStr = fmt.Sprintf("You attempted to modify a project for this object but it did not exist: %q", e.projectsMissing)
	}
	return errorStr
}

// ProjectsUnauthorizedForAssignmentErr occurs when some of the projects in the project diff
// were not authorized for a specific set of subjects.
type ProjectsUnauthorizedForAssignmentErr struct {
	projectsUnauthorized []string
}

func NewProjectsUnauthorizedForAssignmentError(projectsUnauthorized []string) error {
	return &ProjectsUnauthorizedForAssignmentErr{projectsUnauthorized: projectsUnauthorized}
}

func (e *ProjectsUnauthorizedForAssignmentErr) Error() string {
	var errorStr string
	if len(e.projectsUnauthorized) > 1 {
		errorStr = fmt.Sprintf("You attempted to modify projects for this object but you are missing iam:projects:assign on some: %q", e.projectsUnauthorized)
	} else {
		errorStr = fmt.Sprintf("You attempted to modify a project for this object but you are missing iam:projects:assign on it: %q", e.projectsUnauthorized)
	}
	return errorStr
}
