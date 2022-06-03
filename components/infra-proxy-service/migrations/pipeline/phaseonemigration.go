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
			log.Info("Processing to unzip the file...")
			// Start of Unzip pipeline
			_, err := service.Migration.StartUnzip(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID)
			if err != nil {
				log.Errorf("Failed to update start 'StartUnzip' status in DB for migration id : %s :%s", res.Result.Meta.MigrationID, err)
				res.Done <- err
				continue
			}
			result, err := Unzip(res.Result)
			if err != nil {
				log.Errorf("Failed to unzip the file for migration ID: %s :%s", res.Result.Meta.MigrationID, err)
				// Failed Unzip pipeline
				_, _ = service.Migration.FailedUnzip(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, err.Error(), 0, 0, 0)
				res.Done <- err
				continue
			}
			// Complete Unzip pileline
			_, err = service.Migration.CompleteUnzip(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, 0, 0, 0)
			if err != nil {
				log.Errorf("Failed to update `CompleteUnzip` status in DB for migration id : %s :%s", result.Meta.MigrationID, err)
				res.Done <- err
				continue
			}
			res.Result = result
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
		for res := range result {
			log.Info("Processing to parse orgs...")
			_, err := service.Migration.StartOrgParsing(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID)
			if err != nil {
				log.Errorf("Failed to update start 'StartOrgParsing' status in DB for migration id: %s :%s", res.Result.Meta.MigrationID, err)
				res.Done <- err
				continue
			}
			result, err := ParseOrgs(res.Ctx, service.Storage, service.Migration, res.Result)
			if err != nil {
				log.Errorf("Failed to parse orgs for migration ID: %s :%s", res.Result.Meta.MigrationID, err)
				// Failed parse org pipeline
				_, _ = service.Migration.FailedOrgParsing(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, err.Error(), 0, 0, 0)
				res.Done <- err
				continue
			}
			// Complete org parsing pipeline
			_, err = service.Migration.CompleteOrgParsing(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, 0, 0, 0)
			if err != nil {
				log.Errorf("Failed to update `CompleteOrgParsing` status in DB for migration id: %s :%s", result.Meta.MigrationID, err)
				res.Done <- err
				continue
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
		for res := range result {
			log.Info("Processing to parse orgs user association...")
			_, err := service.Migration.StartUserAssociationParsing(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID)
			if err != nil {
				log.Errorf("Failed to update `StartUserAssociationParsing` status in DB for migration id %s : %s", res.Result.Meta.MigrationID, err.Error())
				res.Done <- err
				continue
			}
			result, err := ParseOrgUserAssociation(res.Ctx, service.Storage, res.Result)
			if err != nil {
				log.Errorf("Failed to parse org user association for migration ID: %s :%s", res.Result.Meta.MigrationID, err)
				_, _ = service.Migration.FailedUserAssociationParsing(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, err.Error(), 0, 0, 0)
				res.Done <- err
				continue
			}
			_, err = service.Migration.CompleteUserAssociationParsing(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, 0, 0, 0)
			if err != nil {
				log.Errorf("Failed to update `CompleteUserAssociationParsing` status in DB for migration id %s : %s", res.Result.Meta.MigrationID, err.Error())
				res.Done <- err
				continue
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
	log.Info("Starting to create preview pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		for res := range result {
			log.Info("Processing to create preview pipeline...")
			_, err := service.Migration.StartCreatePreview(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID)
			if err != nil {
				log.Errorf("Failed to update `StartCreatePreview` status in DB for migration id %s :%s", res.Result.Meta.MigrationID, err)
				res.Done <- err
				continue
			}
			result, err := CreatePreview(res.Ctx, service.Storage, service.Migration, res.Result)
			if err != nil {
				log.Errorf("Failed to create preview for migration ID: %s :%s", res.Result.Meta.MigrationID, err)
				_, _ = service.Migration.FailedCreatePreview(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, err.Error(), 0, 0, 0)
				res.Done <- err
				continue
			}
			_, err = service.Migration.CompleteCreatePreview(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, 0, 0, 0)
			if err != nil {
				log.Errorf("Failed to update `CompleteCreatePreview` status in DB for migration id %s : %s", result.Meta.MigrationID, err.Error())
				res.Done <- err
				continue
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
		for res := range result {
			log.Info("Processing to parse_user...")
			// Update start of parse user pipeline
			_, err := service.Migration.StartUsersParsing(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID)
			if err != nil {
				log.Errorf("Failed to update `StartUsersParsing` status in DB for migration id: %s :%s", res.Result.Meta.MigrationID, err)
				res.Done <- err
				continue
			}
			result, err := GetUsersForBackup(res.Ctx, service.Storage, service.LocalUser, res.Result)
			if err != nil {
				log.Errorf("Failed to parse users for migration ID: %s :%s", res.Result.Meta.MigrationID, err)
				// Update failed to parse user
				_, _ = service.Migration.FailedUsersParsing(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, err.Error(), 0, 0, 0)
				res.Done <- err
				continue
			}
			// Update completion of parsing user
			_, err = service.Migration.CompleteUsersParsing(res.Ctx, res.Result.Meta.MigrationID, res.Result.Meta.ServerID, 0, 0, 0)
			if err != nil {
				log.Errorf("Failed to update `CompleteUsersParsing` status in DB for migration id: %s :%s", result.Meta.MigrationID, err)
				res.Done <- err
				continue
			}
			res.Result = result

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

	// Clear backup files
	if err = ClearBackUpFiles(result.Meta.ZipFile); err != nil {
		log.Errorf("cannot delete the zipfile: %+v", err)
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
