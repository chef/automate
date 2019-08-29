package deployment

import (
	"fmt"
	"strings"
	"sync"
	"time"

	"github.com/gofrs/uuid"
	"github.com/golang/protobuf/ptypes"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	dc "github.com/chef/automate/api/config/deployment"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/certauthority"
	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/services"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
)

// Deployment represents a given deployment of Chef Automate.
type Deployment struct {
	ID        string
	CreatedAt time.Time
	// TODO: I wish we had some safety around modifications to this
	Config     *dc.AutomateConfig
	LastAction string
	Deployed   bool
	// ExpectedServices is a map from a string in the form of
	// origin/name to Service structs that represent the desired
	// state of the service per the user's configuration.
	ExpectedServices       []*Service
	CurrentReleaseManifest *manifest.A2

	ca                 *certauthority.CertAuthority
	target             target.Target
	mu                 sync.Mutex
	userOverrideConfig *dc.AutomateConfig
}

// CreateDeployment creates a new, unconfigured deployment.
func CreateDeployment() (*Deployment, error) {
	id, err := deploymentID()
	if err != nil {
		return nil, errors.Wrap(err, "failed to generate deployment id")
	}

	return &Deployment{
		CreatedAt:  time.Now(),
		LastAction: "created",
		ID:         id,
	}, nil
}

// CreateDeploymentWithUserOverrideConfig creates a configured deployment.
func CreateDeploymentWithUserOverrideConfig(config *dc.AutomateConfig) (*Deployment, error) {
	d, err := CreateDeployment()
	if err != nil {
		return nil, err
	}
	err = d.UpdateWithUserOverrideConfig(config)
	return d, err
}

// RestoreDeploymentFromUserOverrideConfig is used by the persistence layer to
// regenerate the deployment struct from the persisted override config before
// then restoring the rest of the deployment state
func RestoreDeploymentFromUserOverrideConfig(config *dc.AutomateConfig) (*Deployment, error) {
	if config == nil {
		return &Deployment{}, nil
	}
	return newDeploymentFromUserOverrideConfig(config)
}

func newDeploymentFromUserOverrideConfig(config *dc.AutomateConfig) (*Deployment, error) {
	mergedConfig, err := dc.MergeWithDefaults(config)
	if err != nil {
		return nil, err
	}
	mergedConfig.SetGlobalConfig()
	return &Deployment{
		Config:             mergedConfig,
		userOverrideConfig: config,
	}, nil
}

// UpdateWithUserOverrideConfig creates a new Deployment struct with a
// user-provided configuration as input. This input is stored
// internally and is what is persisted when we persist the Deployment
// to the database. The result of merging the user-provided
// configuration with the default configuration values is available to
// use via the Config field. We also initialize the set of expected
// services
func (d *Deployment) UpdateWithUserOverrideConfig(config *dc.AutomateConfig) error {
	mergedConfig, err := dc.MergeWithDefaults(config)
	if err != nil {
		return errors.Wrap(err, "error merging configuration with defaults")
	}

	mergedConfig.SetGlobalConfig()
	d.Config = mergedConfig
	d.userOverrideConfig = config

	expectedServices, err := initExpectedServices(d.Config.Deployment)
	if err != nil {
		return errors.Wrap(err, "error initializing expected services list")
	}

	d.ExpectedServices = expectedServices
	return nil
}

// GetUserOverrideConfigForPersistence returns the user-provided
// configuration. This function is used for persistence. Seriously, this
// function is used for persistence.
func (d *Deployment) GetUserOverrideConfigForPersistence() *dc.AutomateConfig {
	return d.userOverrideConfig
}

// MergeIntoUserOverrideConfig merges the provided config into the user override
// config. The input should be a sparse override config.
func (d *Deployment) MergeIntoUserOverrideConfig(config *dc.AutomateConfig) error {
	err := d.userOverrideConfig.OverrideConfigValues(config)
	if err != nil {
		return err
	}

	mergedConfig, err := dc.MergeWithDefaults(d.userOverrideConfig)
	if err != nil {
		return err
	}
	mergedConfig.SetGlobalConfig()
	d.Config = mergedConfig

	return nil
}

// ReplaceUserOverrideConfig replaces the deployments override config with the
// given config. The input should be a sparse override config.
func (d *Deployment) ReplaceUserOverrideConfig(config *dc.AutomateConfig) error {
	d.userOverrideConfig = config

	mergedConfig, err := dc.MergeWithDefaults(d.userOverrideConfig)
	if err != nil {
		return err
	}
	mergedConfig.SetGlobalConfig()
	d.Config = mergedConfig

	return nil
}

func ContainsAutomateCollection(c *dc.ConfigRequest) bool {
	products := c.GetV1().GetSvc().GetProducts()
	if len(products) > 0 {
		return services.ContainsCollection("automate", products)
	}
	return true
}

func ExpectedServiceIDsForConfig(c *dc.ConfigRequest) ([]habpkg.HabPkg, error) {
	var collections []string

	if len(c.GetV1().GetSvc().GetProducts()) > 0 {
		collections = c.GetV1().GetSvc().GetProducts()
	} else {
		collections = []string{"automate"}

		if c.GetV1().GetSvc().GetEnableChefServer().GetValue() {
			collections = append(collections, "chef-server")
		}

		if c.GetV1().GetSvc().GetEnableWorkflow().GetValue() {
			collections = append(collections, "workflow")
		}

		if c.GetV1().GetSvc().GetEnableDevMonitoring().GetValue() {
			collections = append(collections, "monitoring")
		}
	}

	serviceIDs, err := services.ServicesInCollections(collections)
	if err != nil {
		collectionsList := strings.Join(collections, ", ")
		return nil, errors.Wrapf(err, "unable to get list of services for collection(s) %s", collectionsList)
	}
	return serviceIDs, nil
}

// initExpectedServices is called when constructing a new deployment.
// It constructs Service objects from the expected manifest of
// services.
func initExpectedServices(c *dc.ConfigRequest) ([]*Service, error) {
	serviceIDs, err := ExpectedServiceIDsForConfig(c)
	if err != nil {
		return nil, err
	}
	allServices := make([]*Service, len(serviceIDs))
	for i, sp := range serviceIDs {
		svc := NewServiceFromHabPackage(sp)
		allServices[i] = svc
	}
	return allServices, nil
}

// Lock takes an exclusive lock of the deployment struct for writing. It will
// block until the lock is available.
func (d *Deployment) Lock() {
	d.mu.Lock()
}

// Unlock releases a previously help lock.  Panics if called when the
// lock is not held.
func (d *Deployment) Unlock() {
	d.mu.Unlock()
}

//
// ExpectedState mutation functions
//
// All modifications to our expected state should happen via these
// functions.
//

// SetRunningExpectations sets the expectation of each named service to Running
func (d *Deployment) SetRunningExpectations(serviceNames []string) error {
	return d.setServiceExpectations(serviceNames, Running)
}

// SetRemovedExpectations sets the expectation of each named service to Removed
func (d *Deployment) SetRemovedExpectations(serviceNames []string) error {
	return d.setServiceExpectations(serviceNames, Removed)
}

// SetInstalledExpectations sets the expectation of each named service to Installed.
func (d *Deployment) SetInstalledExpectations(serviceNames []string) error {
	return d.setServiceExpectations(serviceNames, Installed)
}

func (d *Deployment) setServiceExpectations(serviceNames []string, state ServiceDeploymentState) error {
	for _, name := range serviceNames {
		svc, found := d.ServiceByName(name)
		if !found {
			return errors.Errorf("could not find service %s in expected service list", name)
		}
		svc.DeploymentState = state
	}
	return nil
}

// EnsureCerts ensures that every ExpectedService has a generated
// TLS certificate and key
func (d *Deployment) EnsureCerts() error {
	for _, service := range d.ExpectedServices {
		req := certauthority.NewCertRequest(service.Name(), d.target.IPs(), []string{})
		if d.certGenerationRequired(service, req) {
			certData, err := d.ca.CertDataForService(req)
			if err != nil {
				return errors.Wrap(err, "certificate generation for service failed")
			}
			service.SSLKey = certData.Key
			service.SSLCert = certData.Cert
		}
	}
	return nil
}

func (d *Deployment) certGenerationRequired(service *Service, req certauthority.CertRequest) bool {
	logctx := logrus.WithFields(logrus.Fields{
		"service":      service.Name(),
		"cert_request": req,
	})
	serviceHasCertData := service.SSLKey != "" && service.SSLCert != ""
	if !serviceHasCertData {
		logctx.Info("No existing certificate data exists for service. Certificate generation required")
		return true
	}

	cert, err := certauthority.PEMToCert(service.SSLCert)
	if err != nil {
		logctx.WithError(err).Info("Could not parse certificate data for service. Certificate regeneration required")
		return true
	}

	logctx = logctx.WithField("certificate", certauthority.CertForLog(cert))
	err = d.ca.ValidateCertificateForRequest(cert, req)
	if err != nil {
		logctx.WithError(err).Info("Certificate regeneration required.")
		return true
	}

	return false
}

// UpdateExpectedServicesFromManifest updates the expected services
// from.....the internal manifest.
func (d *Deployment) UpdateExpectedServicesFromManifest() error {
	serviceIDs, err := ExpectedServiceIDsForConfig(d.Config.GetDeployment())
	if err != nil {
		return err
	}

	// Add any new services
	serviceMap := makeServiceMap(d.ExpectedServices)
	for _, pkg := range serviceIDs {
		desiredSvc := NewServiceFromHabPackage(pkg)
		_, found := serviceMap[pkg.Name()]

		if !found {
			// We're going to automatically start new services we find if a deployment has been done
			if d.Deployed {
				desiredSvc.DeploymentState = Running
			} else {
				desiredSvc.DeploymentState = Skip
			}
			d.ExpectedServices = append(d.ExpectedServices, desiredSvc)
			serviceMap[pkg.Name()] = desiredSvc
		}
	}

	// NOTE(ssd) 2018-07-16: Fix for A1 upgrade bug where services are stuck in
	// Skip state since they were added post-upgrade.
	if ContainsAutomateCollection(d.Config.GetDeployment()) {
		a2ServicesWayBackWhen := map[string]bool{"authn-service": true, "authz-service": true, "automate-cli": true, "automate-dex": true, "automate-elasticsearch": true, "automate-gateway": true, "automate-load-balancer": true, "automate-postgresql": true, "automate-ui": true, "compliance-service": true, "config-mgmt-service": true, "deployment-service": true, "es-sidecar-service": true, "ingest-service": true, "license-control-service": true, "local-user-service": true, "notifications-service": true, "session-service": true, "teams-service": true}
		diff := make([]string, 0, len(serviceMap))
		for key := range serviceMap {
			if _, ok := a2ServicesWayBackWhen[key]; !ok {
				diff = append(diff, key)
			}
		}

		for _, svc := range diff {
			_, found := serviceMap[svc]
			if found {
				if serviceMap["automate-gateway"].DeploymentState == Running &&
					serviceMap[svc].DeploymentState == Skip {

					logrus.Warnf("Detected %s in Skip state with automate-gateway in Running. Applying fix", svc)
					d.Deployed = true
					serviceMap[svc].DeploymentState = Running
				}
			}
		}
	}

	// Remove any removed services
	expectedServices := make([]*Service, len(serviceIDs))
	for i, pkg := range serviceIDs {
		expectedServices[i] = serviceMap[pkg.Name()]
	}
	d.ExpectedServices = expectedServices
	return nil
}

// NotSkippedServiceNames returns a slice of strings that represent services
// that have not been skipped.
func (d *Deployment) NotSkippedServiceNames() []string {
	notSkipped := []string{}

	for _, svc := range d.ExpectedServices {
		if svc.DeploymentState != Skip {
			notSkipped = append(notSkipped, svc.Name())
		}
	}

	return notSkipped
}

func makeServiceMap(svcs []*Service) map[string]*Service {
	svcMap := make(map[string]*Service, len(svcs))
	for _, svc := range svcs {
		svcMap[svc.Name()] = svc
	}
	return svcMap
}

// ServiceByName searches for the given service by name, returning a
// pointer to the service and true if it is found or nil and false if
// it is not found.
func (d *Deployment) ServiceByName(name string) (*Service, bool) {
	for _, svc := range d.ExpectedServices {
		if svc.Name() == name {
			return svc, true
		}
	}
	return nil, false
}

// ServiceNames returns a slice of the expected service names
func (d *Deployment) ServiceNames() []string {
	ret := make([]string, len(d.ExpectedServices))
	for i, svc := range d.ExpectedServices {
		ret[i] = svc.Name()
	}
	return ret
}

// InitCA initializes the certificate authority for this deployment.
// Take care to keep this safe to call multiple times.
func (d *Deployment) InitCA(dataDir string) error {
	if d.ca == nil {
		d.ca = certauthority.NewCertstrapCertAuthority(dataDir, d.ID)
	}

	return d.ca.InitAuthority()
}

// CA returns the underlying CertificateAuthority
//
// Currently initializing the CA just depends on the authority_id
// which is linked to the deployment_id
func (d *Deployment) CA() *certauthority.CertAuthority {
	return d.ca
}

// Status returns the current status of the Deployment.
func (d *Deployment) Status() api.DeploymentID {
	// FIXME: handle error on time parsing.
	createTime, _ := ptypes.TimestampProto(d.CreatedAt)
	return api.DeploymentID{
		Id:         d.ID,
		CreatedAt:  createTime,
		LastAction: d.LastAction,
	}
}

// Channel returns the Habitat channel for deployment
func (d *Deployment) Channel() string {
	return d.Config.GetDeployment().GetV1().GetSvc().GetChannel().GetValue()
}

// FQDN returns the stack's fqdn
func (d *Deployment) FQDN() string {
	return d.Config.GetGlobal().GetV1().GetFqdn().GetValue()
}

// BackupGatewayEndpoint is the listen address of the backup-gateway. As the
// backup-gateway has to listen on the public address for multi-node we'll use
// the target IP address and pull the port from our config.
func (d *Deployment) BackupGatewayEndpoint() string {
	return fmt.Sprintf("%v:%d",
		d.Target().IPs()[0],
		d.Config.GetBackupGateway().GetV1().GetSys().GetService().GetPort().GetValue(),
	)
}

// Target returns the deployment's target
//
// TODO: sd 2017/12/07 - we'd like to have target auto-populate
// based on the AutomateConfig stored in the Deployment, but right
// now that would cause a nasty go dependency cycle that's better
// left to a future change
//
// I'd like this to work in the auto-populate fashion so that we
// have to do less work when restoring the deployment from a the
// database. e.g. we just come back up, read the struct out, and
// then when the server needs access to the target or the
// eventsender then we'll simply create them from the config if
// needed
func (d *Deployment) Target() target.Target {
	return d.target
}

// SetTarget sets the target of the deployment
//
// TODO: this is a not-so-temporary solution to the problem described
// in Target()
func (d *Deployment) SetTarget(t target.Target) {
	d.target = t
}

// ServicesByName - Sorting Interface for ServiceStates
type ServicesByName api.ServiceStatus

func (ss ServicesByName) Len() int { return len(ss.Services) }
func (ss ServicesByName) Swap(i, j int) {
	ss.Services[i], ss.Services[j] = ss.Services[j], ss.Services[i]
}
func (ss ServicesByName) Less(i, j int) bool {
	return ss.Services[i].Name < ss.Services[j].Name
}

func deploymentID() (string, error) {
	//Generate a UUID for deployment ID
	id, err := uuid.NewV4()
	if err != nil {
		return "", err
	}
	return id.String(), nil
}
