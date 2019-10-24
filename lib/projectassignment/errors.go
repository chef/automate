package projectassignment

import (
	"fmt"
)

// ProjectsMissingError occurs when some of the projects in the project diff
// did not exist.
type ProjectsMissingError struct {
	projectsMissing []string
}

func NewProjectsMissingError(projectsMissing []string) error {
	return &ProjectsMissingError{projectsMissing: projectsMissing}
}

func (e *ProjectsMissingError) Error() string {
	var errorStr string
	if len(e.projectsMissing) > 1 {
		errorStr = fmt.Sprintf("Projects cannot be modified because these projects do not exist: %q", e.projectsMissing)
	} else {
		errorStr = fmt.Sprintf("Projects cannot be modified because project %q does not exist", e.projectsMissing)
	}
	return errorStr
}

// ProjectsUnauthorizedForAssignmentErr occurs when some of the projects in the project diff
// were not authorized for a specific set of subjects.
type ProjectsUnauthorizedForAssignmentErr struct {
	subjectsUnauthorized []string
	projectsUnauthorized []string
}

func NewProjectsUnauthorizedForAssignmentError(projectsUnauthorized, subjectsUnauthorized []string) error {
	return &ProjectsUnauthorizedForAssignmentErr{
		projectsUnauthorized: projectsUnauthorized,
		subjectsUnauthorized: subjectsUnauthorized,
	}
}

func (e *ProjectsUnauthorizedForAssignmentErr) Error() string {
	sub := "subject is"
	proj := "this project"
	authedSubs := e.subjectsUnauthorized

	if len(e.projectsUnauthorized) > 1 {
		proj = "these projects"
	}

	if len(authedSubs) > 1 {
		sub = "subjects are"
	}

	// just don't want the error msg out of hand
	if len(authedSubs) > 5 {
		sub = "(truncated) subjects are"
		authedSubs = authedSubs[0:5]
	}

	return fmt.Sprintf("Projects cannot be modified because the %q %s missing iam:projects:assign on %s: %q",
		e.subjectsUnauthorized,
		sub,
		proj,
		e.projectsUnauthorized)
}

// InvalidCreateRequest occurs when a create request contains original projects
// when it should only contain new projects.
type InvalidCreateRequest struct{}

func NewInvalidCreateRequestError() error {
	return &InvalidCreateRequest{}
}

func (e *InvalidCreateRequest) Error() string {
	return fmt.Sprint("OldProjects field must be empty for create request")
}
