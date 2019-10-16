package projectassignment

import (
	"context"

	"github.com/chef/automate/components/authz-service/engine"
)

func EnsureProjectAssignmentAuthorized(ctx context.Context, authorizer engine.V2Authorizer, subjects, projectIDs []string) error {
	engineResp, err := authorizer.V2ProjectsAuthorized(ctx,
		engine.Subjects(subjects),
		engine.Action("iam:projects:assign"),
		engine.Resource("*"),
		engine.ProjectList(projectIDs...))
	if err != nil {
		return err
	}

	authorized := make(map[string]bool)
	for _, project := range engineResp {
		authorized[project] = true
	}

	var unauthorizedProjects []string
	for _, proj := range projectIDs {
		if !authorized[proj] {
			unauthorizedProjects = append(unauthorizedProjects, proj)
		}
	}

	if len(unauthorizedProjects) != 0 {
		return NewProjectsUnauthorizedForAssignmentError(unauthorizedProjects)
	}
	return nil
}

// calculateProjectsToAuthorize returns the symmetric difference of oldProjects and newProjects,
// meaning that any project that is not in the intersection of oldProjects and newProjects
// will be returned. It also accounts for the internal understanding of no projects to mean
// '(unassigned)' project, which requires authorization.
func calculateProjectsToAuthorize(oldProjects, newProjects []string, isUpdateRequest bool) []string {
	projectDiff := []string{}
	previouslyUnassigned := len(oldProjects) == 0
	newlyUnassigned := len(newProjects) == 0

	// because we don't do patchy updates, users will need to submit the projects
	// field on update even if they are not changing it. While a user is often
	// not allowed to _create_ an unassigned resource, they may be permitted to
	// change the name of one, provided the projects are not altered. In those
	// cases, we do not want to check authorization for moving resources to
	// being unassigned.
	if isUpdateRequest && previouslyUnassigned && newlyUnassigned {
		return projectDiff
	}

	oldProjectMap := make(map[string]bool, len(oldProjects))
	for _, oldProj := range oldProjects {
		oldProjectMap[oldProj] = true
	}
	newProjectMap := make(map[string]bool, len(newProjects))
	for _, newProj := range newProjects {
		newProjectMap[newProj] = true
	}

	// any projects were removed, put in diff
	for _, oldProj := range oldProjects {
		if !newProjectMap[oldProj] {
			projectDiff = append(projectDiff, oldProj)
		}
	}

	// any projects were added, put in diff
	for _, newProj := range newProjects {
		if !oldProjectMap[newProj] {
			projectDiff = append(projectDiff, newProj)
		}
	}

	// if project goes from no projects to some or the reverse
	if previouslyUnassigned || newlyUnassigned {
		projectDiff = append(projectDiff, "(unassigned)")
	}

	return projectDiff
}
