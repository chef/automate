package backup

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/boltdb/bolt"
	dc "github.com/chef/automate/api/config/deployment"
	papi "github.com/chef/automate/api/config/platform"
	gc "github.com/chef/automate/api/config/shared"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/habapi"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/persistence/boltdb"
	"github.com/chef/automate/components/automate-deployment/pkg/services"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/platform/command"
	platform_config "github.com/chef/automate/lib/platform/config"
	"github.com/chef/automate/lib/platform/pg"
	"github.com/chef/automate/lib/secrets"
	"github.com/chef/automate/lib/tls/certs"
	"github.com/pkg/errors"
)

var ErrServiceMissingFromManifest error = errors.New("Service not found in manifest")

type Restorer struct {
	//backupID     string
	//locationSpec LocationSpecification
	target target.Target
	//deployment *deployment.Deployment
	backup *Backup
}

type RestorerOpts struct {
	ExportProxyVariables bool
	SkipBootstrap        bool
	SHA256Checksum       string
}

type Backup struct {
	manifest                  *manifest.A2
	config                    *dc.AutomateConfig
	backupsRepositoryLocation LocationSpecification
	backupID                  string
}

func (b *Backup) SetConfig(cfg *dc.AutomateConfig) error {
	b.config = cfg
	b.hydrateGlobalConfig()
	return b.backupsRepositoryLocation.SetGlobalConfig(b.config.Global)
}

func (b *Backup) PatchConfig(cfg *dc.AutomateConfig) error {
	if err := b.config.OverrideConfigValues(cfg); err != nil {
		return err
	}
	b.hydrateGlobalConfig()
	return b.backupsRepositoryLocation.SetGlobalConfig(b.config.Global)
}

func (b *Backup) GetID() string {
	return b.backupID
}

func (b *Backup) GetLocationSpec() LocationSpecification {
	return b.backupsRepositoryLocation
}

func (b *Backup) GetConfig() *dc.AutomateConfig {
	return b.config
}

func (b *Backup) GetManifest() *manifest.A2 {
	return b.manifest
}

func (b *Backup) Validate() error {
	return b.config.Validate()
}

func (b *Backup) BackupBucket() Bucket {
	return b.backupsRepositoryLocation.ToBucket(b.backupID)
}

func (b *Backup) hydrateGlobalConfig() {
	if b.config.Global == nil {
		b.config.Global = &gc.GlobalConfig{}
	}
	if b.config.Global.V1 == nil {
		b.config.Global.V1 = &gc.V1{}
	}
}

func LoadBackup(
	ctx context.Context,
	backupID string,
	backupsRepositoryLocation LocationSpecification,
) (*Backup, error) {
	// TODO: validate backupID
	// (we want to make sure its at least no automate-elasticsearch-data)
	backupsRepositoryBucket := backupsRepositoryLocation.ToBucket("")
	_, backups, err := backupsRepositoryBucket.List(ctx, "", true)
	if err != nil {
		return nil, err
	}
	found := false
	for _, b := range backups {
		b := strings.Trim(string(b), "/")
		fmt.Printf("Found backup %s\n", b)
		if string(b) == backupID {
			found = true
			break
		}
	}

	if !found {
		return nil, errors.New("Not found")
	}

	backupBucket := backupsRepositoryLocation.ToBucket(backupID)
	manifestProvider, err := NewBucketManifestLocation(ctx, backupBucket)
	if err != nil {
		return nil, err
	}

	m, err := manifestProvider.Provider().GetCurrentManifest(ctx, "")
	if err != nil {
		return nil, err
	}

	b := &Backup{
		backupID:                  backupID,
		manifest:                  m,
		backupsRepositoryLocation: backupsRepositoryLocation,
	}

	oldDeployment, err := LoadDeployment(ctx, backupBucket, &NoOpObjectVerifier{})
	if err != nil {
		return nil, err
	}

	cfg := oldDeployment.GetUserOverrideConfigForPersistence()

	if err := b.SetConfig(cfg); err != nil {
		return nil, err
	}

	return b, nil
}

func NewRestorer(
	target target.Target,
	b *Backup,
) (*Restorer, error) {
	return &Restorer{
		target: target,
		backup: b,
	}, nil
}

func (r *Restorer) initializeDeployment() (*deployment.Deployment, error) {
	d, err := deployment.CreateDeploymentWithUserOverrideConfig(r.backup.GetConfig())
	if err != nil {
		return nil, err
	}
	d.CurrentReleaseManifest = r.backup.GetManifest()
	return d, nil
}

func (r *Restorer) Restore(ctx context.Context) error {
	d, err := r.initializeDeployment()
	if err != nil {
		return err
	}

	if err := r.bootstrapHabitat(); err != nil {
		return err
	}

	if err := r.bootstrapDeploymentService(d); err != nil {
		return err
	}

	restoreCtx, err := r.newRestoreContext(ctx, d)
	if err != nil {
		return err
	}

	versioned := serviceToVersioned(d.ExpectedServices)

	errors := []error{}
	for _, svc := range versioned {
		if err := r.restoreService(restoreCtx, d, svc.Name(), &NoOpObjectVerifier{}); err != nil {
			errors = append(errors, err)
			if !canContinueRestore(svc.Name(), err) {
				break
			}
		}
	}

	return nil
}

func (r *Restorer) newRestoreContext(ctx context.Context, d *deployment.Deployment) (*Context, error) {
	connInfo, err := pgConnInfoFromConfig(d.Config)
	if err != nil {
		return nil, err
	}

	cf, err := connFactory()
	if err != nil {
		return nil, err
	}

	secretStore, err := secrets.NewDefaultSecretStore()
	if err != nil {
		return nil, err
	}

	builderMinioLocationSpec, err := builderMinioLocationSpecFromConfigFromConfig(
		d.Config,
		r.target,
		d.CA().RootCert(),
		secretStore,
	)

	restoreCtx := NewContext(
		WithContextCtx(ctx),
		WithContextBackupID(r.backup.GetID()),
		WithContextBackupLocationSpecification(r.backup.GetLocationSpec()),
		WithContextBackupRestoreLocationSpecification(r.backup.GetLocationSpec()),
		WithContextPgConnInfo(connInfo),
		WithContextEsSidecarInfo(esSidecarInfoFromConfig(d.Config)),
		WithContextConnFactory(cf),
		WithContextReleaseManifest(d.CurrentReleaseManifest),
		WithContextBuilderMinioLocationSpec(builderMinioLocationSpec),
	)
	return &restoreCtx, nil
}

func canContinueRestore(serviceName string, err error) bool {
	return true
}

func (r *Restorer) restoreService(ctx *Context, d *deployment.Deployment, serviceName string, verifier ObjectVerifier) error {
	bucket := ctx.restoreBucket

	svc := deployment.ServiceFromManifest(r.backup.GetManifest(), serviceName)
	if svc == nil {
		return ErrServiceMissingFromManifest
	}

	if err := r.installService(svc); err != nil {
		return err
	}

	metadata, err := LoadServiceMetadata(
		ctx.ctx,
		bucket,
		svc.Name(),
		verifier,
	)

	// If the metadata file exists but we failed to load it for whatever
	// reason, like a network issue or corrupted metadata file, then we want to
	// error out.
	if err != nil && !IsNotExist(err) {
		return err
	}

	if err == nil {
		//spec := Spec{Name: svc.Name()}
		spec := *metadata.Spec
		SetCommandExecutor(spec, command.NewExecExecutor())
		for _, o := range spec.SyncOps() {
			err := o.Restore(*ctx, serviceName, metadata.Verifier(), NewNoopOperationProgressReporter())
			if err != nil {
				return err
			}
		}
	}

	if err := r.startService(d, svc); err != nil {
		return err
	}
	return nil
}

func (r *Restorer) installService(svc *deployment.Service) error {
	if err := r.target.InstallService(context.TODO(), svc, ""); err != nil {
		return err
	}

	// Install any binlinks for this package
	binlinks := services.BinlinksForPackage(svc.Name())
	for _, cmd := range binlinks {
		if _, err := r.target.BinlinkPackage(context.TODO(), svc, cmd); err != nil {
			return err
		}
	}
	return nil
}

func (r *Restorer) startService(d *deployment.Deployment, svc *deployment.Service) error {
	userToml, err := deployment.UserTomlForService(d, svc.Name())
	if err != nil {
		return err
	}

	if err := r.target.SetUserToml(svc.Name(), userToml); err != nil {
		return err
	}

	bindInfo, err := services.AllBinds.DefaultsForService(svc.Name())
	if err != nil {
		return err
	}

	if err := r.target.LoadService(context.TODO(), svc, target.BindMode(bindInfo.Mode),
		target.Binds(bindInfo.Specs)); err != nil {
		return err
	}

	if err := waitForHealthy(context.TODO(), r.target, svc.Name()); err != nil {
		return err
	}
	return nil
}

func (r *Restorer) bootstrapHabitat() error {
	return nil
}

func (r *Restorer) bootstrapDeploymentService(d *deployment.Deployment) error {
	svc := deployment.ServiceFromManifest(r.backup.GetManifest(), "deployment-service")
	if svc == nil {
		return ErrServiceMissingFromManifest
	}

	if err := r.unloadServices(context.TODO(), []habpkg.VersionedPackage{svc}); err != nil {
		return err
	}

	if err := r.initializeDeploymentServiceData(d); err != nil {
		return err
	}

	if err := r.installService(svc); err != nil {
		return err
	}

	if err := r.startService(d, svc); err != nil {
		return err
	}

	return nil
}

func (r *Restorer) initializeDeploymentServiceData(d *deployment.Deployment) error {
	if err := os.MkdirAll("/hab/svc/deployment-service/data", 0755); err != nil {
		return err
	}

	if err := writeConvergeDisableFile(); err != nil {
		return err
	}

	if err := initializeDeploymentDatabase(d); err != nil {
		return err
	}

	return nil
}

// unloadServices unloads the services and waits for them to be down
func (r *Restorer) unloadServices(ctx context.Context, versioned []habpkg.VersionedPackage) error {
	// Unload the services
	for _, svc := range versioned {
		if err := r.target.UnloadService(ctx, svc); err != nil {
			return err
		}
	}

	return habapi.WaitForUnload(r.target.HabAPIClient(), versioned, 5*time.Minute)
}

func serviceToVersioned(svcs []*deployment.Service) []habpkg.VersionedPackage {
	versioned := make([]habpkg.VersionedPackage, 0, len(svcs))
	for i := range svcs {
		versioned[i] = svcs[i]
	}
	return versioned
}

func writeConvergeDisableFile() error {
	// Remove read and execute from other
	if err := os.Chmod(filepath.Dir(api.ConvergeDisableFilePath), 0750); err != nil {
		return err
	}

	f, err := os.OpenFile(api.ConvergeDisableFilePath, os.O_RDONLY|os.O_CREATE, 0700)
	if err != nil {
		return err
	}

	if err := f.Close(); err != nil {
		return err
	}

	return nil
}

func initializeDeploymentDatabase(d *deployment.Deployment) error {
	if err := d.InitCA("/hab/svc/deployment-service/data"); err != nil {
		return err
	}

	if err := d.EnsureCerts(); err != nil {
		return err
	}

	dbFile := "/hab/svc/deployment-service/data/bolt.db"

	database, err := bolt.Open(dbFile, 0600, nil)
	if err != nil {
		return err
	}
	defer database.Close()

	deploymentDB := boltdb.NewDeploymentStore(database)
	if err := deploymentDB.Initialize(); err != nil {
		return err
	}

	if _, err := deploymentDB.UpdateDeployment(func(v *deployment.Deployment) error {
		*v = *d
		return nil
	}); err != nil {
		return err
	}

	return nil
}

func waitForHealthy(ctx context.Context, trgt target.Target, serviceName string) error {
	ticker := time.NewTicker(250 * time.Millisecond)
	defer ticker.Stop()
	for {
		select {
		case <-ctx.Done():
			return errors.Wrapf(ctx.Err(), "Timed out waiting for service %s to be healthy", serviceName)
		case <-ticker.C:
			status := trgt.Status(context.Background(), []string{serviceName})

			if status.ServiceHealthy(serviceName) {
				return nil
			}
		}
	}
}

func pgConnInfoFromConfig(cfg *dc.AutomateConfig) (pg.ConnInfo, error) {
	platformConfig := platform_config.Config{
		Config: &papi.Config{
			Service: &papi.Config_Service{
				// NOTE (jaym): The below hack is to allow deployment-service to find the
				// root cert for external postgres.
				// deployment-service doesn't fully participate in the platform config, and
				// so it does not get the external postgres root cert automatically
				Path: "/hab/svc/automate-pg-gateway",
			},
			Postgresql: &papi.Config_Postgresql{
				Ip: cfg.GetPgGateway().GetV1().GetSys().GetService().GetHost().GetValue(),
				Cfg: &papi.Config_Postgresql_Cfg{
					Port: int64(cfg.GetPgGateway().GetV1().GetSys().GetService().GetPort().GetValue()),
				},
			},
			Platform: &papi.Config_Platform{
				ExternalPostgresql: cfg.GetGlobal().GetV1().GetExternal().GetPostgresql(),
			},
		},
	}

	pgConnInfo, err := pg.SuperuserConnInfoFromPlatformConfig(&platformConfig)
	if err != nil {
		return nil, err
	}
	return pgConnInfo, nil
}

func esSidecarInfoFromConfig(cfg *dc.AutomateConfig) ESSidecarConnInfo {
	return ESSidecarConnInfo{
		Host: cfg.GetEsSidecar().GetV1().GetSys().GetService().GetHost().GetValue(),
		Port: cfg.GetEsSidecar().GetV1().GetSys().GetService().GetPort().GetValue(),
	}
}

func connFactory() (*secureconn.Factory, error) {
	c := certs.TLSConfig{
		CertPath:       "/hab/svc/deployment-service/data/deployment-service.crt",
		KeyPath:        "/hab/svc/deployment-service/data/deployment-service.key",
		RootCACertPath: "/hab/svc/deployment-service/data/root.crt",
	}

	certData, err := c.ReadCerts()
	if err != nil {
		return nil, err
	}
	return secureconn.NewFactory(*certData), nil
}

func builderMinioLocationSpecFromConfigFromConfig(cfg *dc.AutomateConfig, trgt target.Target, rootCert string, secretStore secrets.SecretStore) (LocationSpecification, error) {
	port := cfg.GetMinio().GetV1().GetSys().GetService().GetPort().GetValue()
	ips := trgt.IPs()
	if len(ips) < 1 {
		return nil, errors.New("no IP address known for target")
	}
	return NewMinioLocationSpec(
		fmt.Sprintf("https://%s:%d", ips[0].String(), port),
		"depot",
		"",
		"minio",
		[]byte(rootCert),
		secretStore,
	)

}
