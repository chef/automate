package projectassignment

import (
	"fmt"
)

// ProjectsMissingError occurs when some of the projects in the project diff
// did not exist.
type ProjectsMissingError struct {
	projectsMissing []string
}

func NewProjectsMissingErroror(projectsMissing []string) error {
	return &ProjectsMissingError{projectsMissing: projectsMissing}
}

func (e *ProjectsMissingError) Error() string {
	var errorStr string
	if len(e.projectsMissing) > 1 {
		errorStr = fmt.Sprintf("You cannot modify projects for this object because these projects did not exist: %q", e.projectsMissing)
	} else {
		errorStr = fmt.Sprintf("You cannot modify a project for this object because this project did not exist: %q", e.projectsMissing)
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
		errorStr = fmt.Sprintf("You cannot modify projects for this object because you are missing iam:projects:assign on these projects: %q", e.projectsUnauthorized)
	} else {
		errorStr = fmt.Sprintf("You cannot modify a project for this object because you are missing iam:projects:assign on this project: %q", e.projectsUnauthorized)
	}
	return errorStr
}
