package pipeline

import (
	"context"

	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/infra-proxy-service/pipeline"
	"github.com/chef/automate/components/infra-proxy-service/service"
	"github.com/chef/automate/components/infra-proxy-service/storage"
)

type PipelineData struct {
	Result pipeline.Result
	Done   chan<- error
	Ctx    context.Context
}

type PhaseOnePipeline struct {
	in chan<- PipelineData
}

type PhaseOnePipelineProcessor func(<-chan PipelineData) <-chan PipelineData

// ParseOrg unzip the backup file and returns PhaseOnePipelineProcessor
func UnzipSrc(service *service.Service) PhaseOnePipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return unzipSrc(result, service)
	}
}

func unzipSrc(result <-chan PipelineData, service *service.Service) <-chan PipelineData {
	log.Info("Starting unzip pipeline")
	out := make(chan PipelineData, 100)
	go func() {
		for res := range result {
			// Start of Unzip pipeline
			_, err := service.Migration.StartUnzip(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID)
			if err != nil {
				log.Errorf("Failed to update start 'StartUnzip' status in DB: %s :%s", res.Result.Meta.MigrationID, err)
			}

			result, err := Unzip(res.Ctx, res.Result)
			if err != nil {
				// Failed Unzip pipeline
				_, err := service.Migration.FailedUnzip(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, "cannot open zipfile", 0, 0, 0)
				if err != nil {
					log.Errorf("Failed to update `FailedUnzip` status in DB: %s :%s", res.Result.Meta.MigrationID, err)
				}
				return
			}

			res.Result = result

			// Complete Unzip pileline
			_, err = service.Migration.CompleteUnzip(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, 0, 0, 0)
			if err != nil {
				log.Errorf("Failed to update `CompleteUnzip` status in DB: %s :%s", result.Meta.MigrationID, err)
			}

			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		log.Info("Closing unzip")
		close(out)
	}()
	return out
}

// ParseOrg parse the orgs and returns PhaseOnePipelineProcessor
func ParseOrgSrc(service *service.Service) PhaseOnePipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return parseOrgSrc(result, service)
	}
}

func parseOrgSrc(result <-chan PipelineData, service *service.Service) <-chan PipelineData {
	log.Info("Starting to parse_orgs pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		log.Info("Processing to parse orgs...")
		for res := range result {
			result, err := ParseOrgs(res.Ctx, service.Storage, service.Migration, res.Result)
			if err != nil {
				return
			}
			res.Result = result
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		log.Info("Closing parse_orgs pipeline")
		close(out)
	}()
	return out
}

// ParseOrg parse the org user association and returns PhaseOnePipelineProcessor
func ParseOrgUserAssociationSrc(service *service.Service) PhaseOnePipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return parseOrgUserAssociationSrc(result, service)
	}
}

func parseOrgUserAssociationSrc(result <-chan PipelineData, service *service.Service) <-chan PipelineData {
	log.Info("Starting to parse_orgs_user_association pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		log.Info("Processing to parse orgs user association...")
		for res := range result {
			_, err := service.Migration.StartUserAssociationParsing(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID)
			if err != nil {
				log.Errorf("Failed to update the status for start org user association for the migration id %s : %s", res.Result.Meta.MigrationID, err.Error())
				return
			}
			result, err := ParseOrgUserAssociation(res.Ctx, service.Storage, res.Result)
			if err != nil {
				_, _ = service.Migration.FailedUserAssociationParsing(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, err.Error(), 0, 0, 0)
				return
			}
			_, err = service.Migration.CompleteUserAssociationParsing(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, 0, 0, 0)
			if err != nil {
				log.Errorf("Failed to update the status for complete org user association for the migration id %s : %s", res.Result.Meta.MigrationID, err.Error())
				return
			}
			res.Result = result
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		log.Info("Closing parse_orgs_user_association pipeline")
		close(out)
	}()
	return out
}

// ParseOrg returns PhaseOnePipelineProcessor
func CreatePreviewSrc(service *service.Service) PhaseOnePipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return createPreviewSrc(result, service)
	}
}

func createPreviewSrc(result <-chan PipelineData, service *service.Service) <-chan PipelineData {
	log.Info("Starting to preview pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		log.Info("Processing to preview pipeline...")
		for res := range result {
			result, err := CreatePreview(res.Ctx, service.Storage, service.Migration, res.Result)
			if err != nil {
				return
			}
			res.Result = result
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		log.Info("CLosing preview pipeline")
		close(out)
	}()
	return out
}

// ParseUser returns PhaseOnePipelineProcessor
func ParseUserSrc(service *service.Service) PhaseOnePipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return parseUserSrc(result, service)
	}
}

func parseUserSrc(result <-chan PipelineData, service *service.Service) <-chan PipelineData {
	log.Info("Starting to parse_user pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		log.Info("Processing to parse_user...")
		for res := range result {
			// Update start of parse user pipeline
			_, err := service.Migration.StartUsersParsing(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID)
			if err != nil {
				log.Errorf("Failed to update `StartUsersParsing` status in DB: %s :%s", res.Result.Meta.MigrationID, err)
			}

			result, err := GetUsersForBackup(res.Ctx, service.Storage, service.LocalUser, res.Result)
			if err != nil {
				// Update failed to parse user
				_, err := service.Migration.FailedUsersParsing(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, "error parsing user", 0, 0, 0)
				if err != nil {
					log.Errorf("Failed to update `FailedUsersParsing` status in DB: %s :%s", res.Result.Meta.MigrationID, err)
				}
				return
			}

			res.Result = result

			// Update completion of parsing user
			_, err = service.Migration.CompleteUsersParsing(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, 0, 0, 0)
			if err != nil {
				log.Errorf("Failed to update `CompleteUsersParsing` status in DB: %s :%s", result.Meta.MigrationID, err)
			}

			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		log.Info("Closing parse_user")
		close(out)
	}()

	return out
}

// ConflictingUsers returns PhaseOnePipelineProcessor
func ConflictingUsers() PhaseOnePipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return conflictingUsers(result)
	}
}

func conflictingUsers(result <-chan PipelineData) <-chan PipelineData {
	log.Info("Starting to conflicting_user check pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		log.Info("Processing to conflicting_user users...")

		for res := range result {
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		log.Info("Closing conflicting_user")
		close(out)

	}()

	return out
}

// OrgMembers returns PhaseOnePipelineProcessor
func OrgMembers() PhaseOnePipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return orgMembers(result)
	}
}

func orgMembers(result <-chan PipelineData) <-chan PipelineData {
	log.Info("Starting to org_user check pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		log.Info("Processing to check org_user association...")
		for res := range result {
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		log.Info("Closing org_user association check")
		close(out)

	}()

	return out
}

// AdminUsers Return PhaseOnePipelineProcessor
func AdminUsers() PhaseOnePipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return adminUsers(result)
	}
}

func adminUsers(result <-chan PipelineData) <-chan PipelineData {
	log.Info("Starting org admin_users check ")

	out := make(chan PipelineData, 100)

	go func() {
		log.Info("Processing to to check admin_users...")
		for res := range result {
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		log.Info("Closing admin_users")
		close(out)

	}()

	return out
}

func migrationPipeline(source <-chan PipelineData, pipes ...PhaseOnePipelineProcessor) {
	log.Info("Pipeline started...")
	go func() {
		for _, pipe := range pipes {
			source = pipe(source)
		}

		for s := range source {
			s.Done <- nil
		}
	}()
}

func SetupPhaseOnePipeline(service *service.Service) PhaseOnePipeline {
	c := make(chan PipelineData, 100)
	migrationPipeline(c,
		UnzipSrc(service),
		ParseOrgSrc(service),
		ParseUserSrc(service),
		ParseOrgUserAssociationSrc(service),
		CreatePreviewSrc(service),
		// ConflictingUsers(),
		// OrgMembers(),
		// AdminUsers(),
	)
	return PhaseOnePipeline{in: c}
}

func (p *PhaseOnePipeline) Run(result pipeline.Result, service *service.Service) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	done := make(chan error)
	select {
	case p.in <- PipelineData{Result: result, Done: done, Ctx: ctx}:
	}
	err := <-done
	if err != nil {
		MigrationError(err, service.Migration, ctx, result.Meta.MigrationID, result.Meta.ServerID)
		log.Errorf("Phase one pipeline received error for migration %s: %s", result.Meta.MigrationID, err)
	}
	log.Info("received done")
}

func MigrationError(err error, st storage.MigrationStorage, ctx context.Context, migrationId, serviceId string) {
	_, err = st.FailedMigration(ctx, migrationId, serviceId, err.Error(), 0, 0, 0)
	if err != nil {
		log.Errorf("received error while updating for migration id %s: %s", migrationId, err)
	}
}

func MigrationSuccess(st storage.MigrationStorage, ctx context.Context, migrationId, serviceId string) {
	_, err := st.CompleteMigration(ctx, migrationId, serviceId, 0, 0, 0)
	if err != nil {
		log.Errorf("received error while updating for migration id %s: %s", migrationId, err)
	}
}
