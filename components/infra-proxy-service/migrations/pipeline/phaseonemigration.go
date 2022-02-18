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

type PhaseOnePipleine struct {
	in chan<- PipelineData
}

type PhaseOnePipelineProcessor func(<-chan PipelineData) <-chan PipelineData

// ParseOrg returns PhaseOnePipelineProcessor
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
			result, err := Unzip(res.Ctx, service.Migration, res.Result)
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
		log.Info("Closing unzip")
		close(out)
	}()
	return out
}

// ParseOrg returns PhaseOnePipelineProcessor
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
func ParseUserSrc() PhaseOnePipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return parseUserSrc(result)
	}
}

func parseUserSrc(result <-chan PipelineData) <-chan PipelineData {
	log.Info("Starting to parse_user pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		log.Info("Processing to parse_user...")
		for res := range result {
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

func SetupPhaseOnePipeline(service *service.Service) PhaseOnePipleine {
	c := make(chan PipelineData, 100)
	migrationPipeline(c,
		UnzipSrc(service),
		ParseOrgSrc(service),
		CreatePreviewSrc(service),
		// ParseUser(),
		// ConflictingUsers(),
		// OrgMembers(),
		// AdminUsers(),
	)
	return PhaseOnePipleine{in: c}
}

func (p *PhaseOnePipleine) Run(result pipeline.Result, service *service.Service) {
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
