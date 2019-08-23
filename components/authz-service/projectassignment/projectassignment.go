package projectassignment

import (
	"context"

	"github.com/chef/automate/components/authz-service/engine"
)

func ErrIfProjectAssignmentUnauthroized(ctx context.Context, authorizer engine.V2Authorizer, subjects, projectIDs []string) error {
	engineResp, err := authorizer.V2ProjectsAuthorized(ctx,
		engine.Subjects(subjects),
		engine.Action("iam:projects:assign"),
		engine.Resource("*"), // TODO (tc): Should this be iam:projects or something?
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
		_, inNew := newProjectMap[oldProj]
		if !inNew {
			projectDiff = append(projectDiff, oldProj)
		}
	}

	// any projects were added, put in diff
	for _, newProj := range newProjects {
		_, inOld := oldProjectMap[newProj]
		if !inOld {
			projectDiff = append(projectDiff, newProj)
		}
	}

	return projectDiff
}
