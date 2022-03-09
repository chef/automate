package pipeline

import (
	"context"

	"github.com/chef/automate/components/infra-proxy-service/service"
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/infra-proxy-service/pipeline"
)

type PhaseTwoPipeline struct {
	in chan<- PipelineData
}

type PhaseTwoPipelineProcessor func(<-chan PipelineData) <-chan PipelineData

// PopulateOrgsSrc populate the orgs based on actions and returns PhaseTwoPipelineProcessor
func PopulateOrgsSrc(service *service.Service) PhaseTwoPipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return populateOrgs(result, service)
	}
}

func populateOrgs(result <-chan PipelineData, service *service.Service) <-chan PipelineData {
	log.Info("Starting populateOrgs routine")
	out := make(chan PipelineData, 100)

	go func() {
		for res := range result {
			log.Info("Processing to populateOrgs...")
			_, err := service.Migration.StartOrgMigration(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID)
			if err != nil {
				log.Errorf("Failed to update start 'StartOrgMigration' status in DB for migration id: %s :%s", res.Result.Meta.MigrationID, err)
				res.Done <- err
				return
			}
			result, err := StoreOrgs(res.Ctx, service.Storage, service.AuthzProjectClient, res.Result)
			if err != nil {
				log.Errorf("Failed to populate orgs for migration id: %s :%s", res.Result.Meta.MigrationID, err)
				_, _ = service.Migration.FailedOrgMigration(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, err.Error(), result.ParsedResult.OrgsUsersAssociationsCount.Succeeded, result.ParsedResult.OrgsUsersAssociationsCount.Skipped, result.ParsedResult.OrgsUsersAssociationsCount.Failed)
				res.Done <- err
				return
			}
			_, err = service.Migration.CompleteOrgMigration(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, result.ParsedResult.OrgsUsersAssociationsCount.Succeeded, result.ParsedResult.OrgsUsersAssociationsCount.Skipped, result.ParsedResult.OrgsUsersAssociationsCount.Failed)
			if err != nil {
				log.Errorf("Failed to update start 'CompleteOrgMigration' status in DB for migration id: %s : %s", res.Result.Meta.MigrationID, err.Error())
				res.Done <- err
				return
			}

			res.Result = result
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		log.Info("Closing populateOrgs routine")
		close(out)
	}()
	return out
}

// PopulateOrgs returns PhaseTwoPipelineProcessor
func CreateProject() PhaseTwoPipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return createProject(result)
	}
}

func createProject(result <-chan PipelineData) <-chan PipelineData {
	log.Info("Starting CreateProject routine")
	out := make(chan PipelineData, 100)

	go func() {
		for res := range result {
			log.Info("Processing to createProject...")
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		log.Info("Closing CreateProject routine")
		close(out)
	}()
	return out
}

// PopulateUsers returns PhaseTwoPipelineProcessor
func PopulateUsers() PhaseTwoPipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return populateUsers(result)
	}
}

func populateUsers(result <-chan PipelineData) <-chan PipelineData {
	log.Info("Starting PopulateUsers routine")
	out := make(chan PipelineData, 100)

	go func() {
		for res := range result {
			log.Info("Processing to populateUsers...")
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		log.Info("Closing PopulateUsers routine")
		close(out)
	}()
	return out
}

// PopulateOrgsUsersAssociationSrc populate the org user association in DB and returns PhaseTwoPipelineProcessor
func PopulateOrgsUsersAssociationSrc(service *service.Service) PhaseTwoPipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return populateOrgsUsersAssociation(result, service)
	}
}

func populateOrgsUsersAssociation(result <-chan PipelineData, service *service.Service) <-chan PipelineData {
	log.Info("Starting to populate_orgs_user_association pipeline")
	out := make(chan PipelineData, 100)

	go func() {
		for res := range result {
			log.Info("Processing to populate orgs users association...")
			_, err := service.Migration.StartAssociation(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID)
			if err != nil {
				log.Errorf("Failed to update start 'StartAssociation' status in DB for migration id: %s :%s", res.Result.Meta.MigrationID, err.Error())
				res.Done <- err
				return
			}
			result, err := StoreOrgsUsersAssociation(res.Ctx, service.Storage, res.Result)
			if err != nil {
				log.Errorf("Failed to store org users association for migration id: %s :%s", res.Result.Meta.MigrationID, err.Error())
				_, _ = service.Migration.FailedAssociation(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, err.Error(), result.ParsedResult.OrgsUsersAssociationsCount.Succeeded, result.ParsedResult.OrgsUsersAssociationsCount.Skipped, result.ParsedResult.OrgsUsersAssociationsCount.Failed)
				res.Done <- err
				return
			}
			_, err = service.Migration.CompleteAssociation(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, result.ParsedResult.OrgsUsersAssociationsCount.Succeeded, result.ParsedResult.OrgsUsersAssociationsCount.Skipped, result.ParsedResult.OrgsUsersAssociationsCount.Failed)
			if err != nil {
				log.Errorf("Failed to update start 'CompleteAssociation' status in DB for migration id: %s :%s", res.Result.Meta.MigrationID, err.Error())
				res.Done <- err
				return
			}
			res.Result = result
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		log.Info("Closing populate_orgs_user_association pipeline")
		close(out)
	}()
	return out
}

// PopulateMembersPolicy returns PhaseTwoPipelineProcessor
func PopulateMembersPolicy() PhaseTwoPipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return populateMembersPolicy(result)
	}
}

func populateMembersPolicy(result <-chan PipelineData) <-chan PipelineData {
	log.Info("Starting PopulateMembersPolicy routine")
	out := make(chan PipelineData, 100)

	go func() {
		for res := range result {
			log.Info("Processing to populateMembersPolicy...")
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		log.Info("Closing PopulateMembersPolicy routine")
		close(out)
	}()
	return out
}

func migrationTwoPipeline(source <-chan PipelineData, pipes ...PhaseTwoPipelineProcessor) {
	log.Info("Phase two pipeline started...")
	go func() {
		for _, pipe := range pipes {
			source = pipe(source)
		}

		for s := range source {
			s.Done <- nil
		}

	}()

}

func SetupPhaseTwoPipeline(service *service.Service) PhaseTwoPipeline {
	c := make(chan PipelineData, 100)
	migrationTwoPipeline(c,
		PopulateOrgsSrc(service),
		// CreateProject(),
		// PopulateUsers(),
		PopulateOrgsUsersAssociationSrc(service),
		// PopulateMembersPolicy(),
	)
	return PhaseTwoPipeline{in: c}
}

func (p *PhaseTwoPipeline) Run(result pipeline.Result, service *service.Service) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := make(chan error)
	select {
	case p.in <- PipelineData{Result: result, Done: done, Ctx: ctx}:
	}
	err := <-done
	if err != nil {
		MigrationError(err, service.Migration, ctx, result.Meta.MigrationID, result.Meta.ServerID)
		log.Errorf("Phase two pipeline received error for migration %s: %s", result.Meta.MigrationID, err)
		return
	}
	MigrationSuccess(service.Migration, ctx, result.Meta.MigrationID, result.Meta.ServerID)
}
