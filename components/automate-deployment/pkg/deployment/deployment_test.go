package deployment

import (
	"testing"

	"github.com/stretchr/testify/assert"

	dc "github.com/chef/automate/api/config/deployment"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

func TestCreateDeploymentWithUserOverrideConfig(t *testing.T) {
	config := &dc.AutomateConfig{}
	d, err := CreateDeploymentWithUserOverrideConfig(config)
	// should we test that mutating config doesn't mess with the
	// resulting deployment's state?
	// TODO: yes we probably should :)
	assert.Equal(t, nil, err)
	assert.Equal(t, "created", d.LastAction)
}

func TestDeploymentID(t *testing.T) {
	config := &dc.AutomateConfig{}
	d, err := CreateDeploymentWithUserOverrideConfig(config)
	assert.Equal(t, nil, err)

	status := d.Status()
	assert.Equal(t, status.Id, d.ID)
	assert.Equal(t, status.LastAction, d.LastAction)
}

func TestInitExpectedServicesReturnsInitializedServiceObjects(t *testing.T) {
	config := dc.NewConfigRequest()
	config.V1.Svc.Channel = w.String("test-channel")
	config.V1.Svc.UpgradeStrategy = w.String("test-strat")

	svcs, err := initExpectedServices(config)
	assert.Nil(t, err)
	for _, s := range svcs {
		assert.NotEqual(t, "", s.Name())
		assert.NotEqual(t, "", s.Origin())
	}
}

func TestSetRunningExpectations(t *testing.T) {
	config := dc.NewAutomateConfig()
	deployment, _ := CreateDeploymentWithUserOverrideConfig(config)
	svc, found := deployment.ServiceByName("local-user-service")
	err := deployment.SetRunningExpectations([]string{"local-user-service"})
	assert.Nil(t, err)
	assert.True(t, found)
	assert.NotNil(t, svc)
	assert.Equal(t, svc.DeploymentState, Running)
}

func TestSetRemovedExpectations(t *testing.T) {
	config := dc.NewAutomateConfig()
	deployment, _ := CreateDeploymentWithUserOverrideConfig(config)
	svc, _ := deployment.ServiceByName("local-user-service")
	err := deployment.SetRemovedExpectations([]string{"local-user-service"})
	assert.Nil(t, err)
	assert.Equal(t, svc.DeploymentState, Removed)
}

func TestUpdateExpectedServicesFromManifest(t *testing.T) {
	config := dc.NewAutomateConfig()

	t.Run("test adding services when in undeployed state adds them as Skip", func(t *testing.T) {
		deployment, _ := CreateDeploymentWithUserOverrideConfig(config)
		firstSvc := deployment.ExpectedServices[0]
		deployment.ExpectedServices = deployment.ExpectedServices[1:]
		deployment.Deployed = false
		deployment.UpdateExpectedServicesFromManifest()
		for _, svc := range deployment.ExpectedServices {
			if svc.Name() == firstSvc.Name() {
				assert.Equal(t, Skip, svc.DeploymentState)
			}
		}
	})

	t.Run("test adding services when in deployed state adds them as Running", func(t *testing.T) {
		deployment, _ := CreateDeploymentWithUserOverrideConfig(config)
		firstSvc := deployment.ExpectedServices[0]
		deployment.ExpectedServices = deployment.ExpectedServices[1:]
		deployment.Deployed = true
		deployment.UpdateExpectedServicesFromManifest()
		for _, svc := range deployment.ExpectedServices {
			if svc.Name() == firstSvc.Name() {
				assert.Equal(t, Running, svc.DeploymentState)
			}
		}
	})

}

func TestSetInstalledExpectations(t *testing.T) {
	config := dc.NewAutomateConfig()
	deployment, _ := CreateDeploymentWithUserOverrideConfig(config)
	svc, _ := deployment.ServiceByName("local-user-service")
	err := deployment.SetInstalledExpectations([]string{"local-user-service"})
	assert.Nil(t, err)
	assert.Equal(t, svc.DeploymentState, Installed)
}
