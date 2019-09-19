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

// CalculateProjectDiff returns the symmetric difference of oldProjects and newProjects,
// meaning that any project that is not in the intersection of oldProjects and newProjects
// will be returned.
func CalculateProjectDiff(oldProjects []string, newProjects []string) []string {
	oldProjectMap := make(map[string]bool, len(oldProjects))
	for _, oldProj := range oldProjects {
		oldProjectMap[oldProj] = true
	}
	newProjectMap := make(map[string]bool, len(newProjects))
	for _, newProj := range newProjects {
		newProjectMap[newProj] = true
	}
	projectDiff := []string{}

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

	return projectDiff
}
