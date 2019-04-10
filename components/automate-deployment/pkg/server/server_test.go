// Copyright © 2017 Chef Software

package server

import (
	"context"
	"io/ioutil"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
	mock "github.com/stretchr/testify/mock"
	"github.com/stretchr/testify/require"

	dc "github.com/chef/automate/api/config/deployment"
	w "github.com/chef/automate/api/config/shared/wrappers"

	"github.com/chef/automate/api/config/shared"
	api "github.com/chef/automate/api/interservice/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/events"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/components/automate-deployment/pkg/usermgmt/mockclient"
)

func buildServiceStatus(statusMap map[string]api.ServiceState_State) *api.ServiceStatus {
	status := &api.ServiceStatus{}
	for service, state := range statusMap {
		status.Add(&api.ServiceState{
			Name:  service,
			State: state,
		})
	}
	return status
}

func testServer(t target.Target) server {
	c := dc.NewAutomateConfig()
	c.Global.V1.Fqdn = w.String("fqdn")
	c.LoadBalancer.V1.Sys.FrontendTls = []*shared.FrontendTLSCredential{
		{
			ServerName: "foo",
			Key:        "foo",
			Cert:       "foo",
		},
	}
	c.Deployment.V1.Svc.DeploymentType = w.String("local")
	c.Deployment.V1.Svc.Channel = w.String("dev")
	c.Deployment.V1.Svc.UpgradeStrategy = w.String("at-once")
	c.Deployment.V1.Svc.AdminUser.Name = w.String("Cowboy")
	c.Deployment.V1.Svc.AdminUser.Username = w.String("cowboy")
	c.Deployment.V1.Svc.AdminUser.Password = w.String("ponies")

	d, _ := deployment.CreateDeployment()
	d.UpdateWithUserOverrideConfig(c)
	d.SetTarget(t)

	return server{deployment: d, ensureStatusInterval: time.Nanosecond}
}

func TestEnsureStatusReturnsStatus(t *testing.T) {
	mockTarget := new(target.MockTarget)
	ctx := context.Background()

	stubStatus := buildServiceStatus(map[string]api.ServiceState_State{
		"automate-load-balancer": api.ServiceState_OK,
		"compliance":             api.ServiceState_OK,
	})

	x := testServer(mockTarget)
	mockEventSender := new(events.MockEventSender)
	s := &errDeployer{server: &x, err: nil, sender: mockEventSender}
	services := s.deployment.ServiceNames()

	mockTarget.On("Status", ctx, services).Return(stubStatus, nil)
	mockEventSender.On("Phase", api.Running, events.CheckingServiceHealth)
	mockEventSender.On("PhaseStep", api.CompleteOk, events.CheckingServiceHealth, "all services healthy", "")
	mockEventSender.On("Phase", api.CompleteOk, events.CheckingServiceHealth)

	s.ensureStatus(ctx, services, 10*time.Second)
	mockTarget.AssertExpectations(t)
	mockEventSender.AssertExpectations(t)
}

func TestEnsureStatusWithErrorRetires(t *testing.T) {
	mockTarget := new(target.MockTarget)
	ctx := context.Background()

	errorStatus := buildServiceStatus(map[string]api.ServiceState_State{
		"automate-load-balancer": api.ServiceState_OK,
		"compliance":             api.ServiceState_CRITICAL,
	})
	successStatus := buildServiceStatus(map[string]api.ServiceState_State{
		"automate-load-balancer": api.ServiceState_OK,
		"compliance":             api.ServiceState_OK,
	})

	x := testServer(mockTarget)
	mockEventSender := new(events.MockEventSender)
	s := &errDeployer{server: &x, err: nil, sender: mockEventSender}
	services := s.deployment.ServiceNames()

	mockTarget.On("Status", ctx, services).Return(errorStatus, nil).Once()
	mockTarget.On("Status", ctx, services).Return(successStatus, nil).Once()
	mockEventSender.On("Phase", api.Running, events.CheckingServiceHealth)
	mockEventSender.On("PhaseStep", api.Running, events.CheckingServiceHealth, ".", "")
	mockEventSender.On("PhaseStep", api.CompleteOk, events.CheckingServiceHealth, "all services healthy", "")
	mockEventSender.On("Phase", api.CompleteOk, events.CheckingServiceHealth)

	s.ensureStatus(ctx, services, 10*time.Second)
	mockTarget.AssertExpectations(t)
	mockEventSender.AssertExpectations(t)
}

func TestEnsureStatusWhenTimeoutExceeded(t *testing.T) {
	mockTarget := new(target.MockTarget)
	ctx := context.Background()

	stubStatus := buildServiceStatus(map[string]api.ServiceState_State{
		"automate-load-balancer": api.ServiceState_OK,
		"compliance":             api.ServiceState_CRITICAL,
	})

	x := testServer(mockTarget)
	mockEventSender := new(events.MockEventSender)
	s := &errDeployer{server: &x, err: nil, sender: mockEventSender}

	services := s.deployment.ServiceNames()

	mockTarget.On("Status", ctx, services).Return(stubStatus, nil)

	mockEventSender.On("Phase", api.Running, events.CheckingServiceHealth)
	mockEventSender.On("PhaseStep", api.Running, events.CheckingServiceHealth, ".", "")
	mockEventSender.On("PhaseStep", api.CompleteFail, events.CheckingServiceHealth, "", mock.AnythingOfType("string"))
	mockEventSender.On("Phase", api.CompleteFail, events.CheckingServiceHealth)
	s.ensureStatusInterval = 5 * time.Nanosecond
	s.ensureStatus(ctx, services, 1*time.Nanosecond)
	te, ok := s.err.(*deployment.StatusTimeoutError)
	assert.True(t, ok)
	assert.Equal(t, stubStatus, te.Status)
	mockTarget.AssertExpectations(t)
}

func TestParseRequestedServices(t *testing.T) {
	mockTarget := new(target.MockTarget)
	x := testServer(mockTarget)
	t.Run("it rejects malformed services",
		func(t *testing.T) {
			_, err := x.parseRequestedServices([]string{"chef/something/ruroh/foo/bar"})
			assert.Equal(t, ErrorServiceNameInvalid, err)
		})
	t.Run("it rejects services not in expected services",
		func(t *testing.T) {
			_, err := x.parseRequestedServices([]string{"chef/idk"})
			assert.Equal(t, ErrorNoSuchService, err)
		})
	t.Run("it returns a list of service names",
		func(t *testing.T) {
			res, err := x.parseRequestedServices([]string{"chef/local-user-service", "chef/teams-service"})
			require.Nil(t, err)
			assert.Equal(t, []string{"local-user-service", "teams-service"}, res)
		})
}

func TestAttemptToCreateInitialUser(t *testing.T) {
	ctx := context.Background()
	mockEventSender := new(events.MockEventSender)
	adminUser := &dc.ConfigRequest_V1_AdminUser{
		Username: w.String("Test User"),
		Name:     w.String("testuser"),
		Password: w.String("GottaCatchEmAll"),
	}
	mockUUID := "response_uuid"

	t.Run("when user didn't exist, is properly created, and added to the admins team", func(t *testing.T) {
		mockClient := mockclient.NewMockUserMgmtClient(mockUUID, true, nil, nil)
		mockEventSender.On("Phase", api.CompleteOk, events.CreateAdminUser)

		err := attemptToCreateInitialUser(ctx, mockClient, mockEventSender, adminUser)
		require.Nil(t, err)
	})

	t.Run("when admin user has legacy email and username fields, is created and added to admins team", func(t *testing.T) {
		mockClient := mockclient.NewMockUserMgmtClient(mockUUID, true, nil, nil)
		mockEventSender.On("Phase", api.CompleteOk, events.CreateAdminUser)
		adminUser := &dc.ConfigRequest_V1_AdminUser{
			Email:    w.String("testuser"),
			Username: w.String("Test User"),
			Password: w.String("GottaCatchEmAll"),
		}

		err := attemptToCreateInitialUser(ctx, mockClient, mockEventSender, adminUser)
		require.Nil(t, err)
	})

	t.Run("when user already exists and is added to the admins team", func(t *testing.T) {
		mockClient := mockclient.NewMockUserMgmtClient(mockUUID, false, nil, nil)
		mockEventSender.On("Phase", api.CompleteOk, events.CreateAdminUser)
		mockEventSender.On("PhaseStep", api.CompleteOk, events.CreateAdminUser,
			"Skipping user creation because username exists", "")

		err := attemptToCreateInitialUser(ctx, mockClient, mockEventSender, adminUser)
		require.Nil(t, err)
	})

	t.Run("when user creation fails unexpectedly", func(t *testing.T) {
		mockClient := mockclient.NewMockUserMgmtClient("", false, errors.New("fail"), nil)
		mockEventSender.On("PhaseStep", api.CompleteFail, events.CreateAdminUser, "", mock.AnythingOfType("string"))
		mockEventSender.On("Phase", api.CompleteFail, events.CreateAdminUser)

		err := attemptToCreateInitialUser(ctx, mockClient, mockEventSender, adminUser)
		require.NotNil(t, err)
		assert.Equal(t, "fail", err.Error())
	})

	t.Run("when adding the user to the admins team fails unexpectedly", func(t *testing.T) {
		mockClient := mockclient.NewMockUserMgmtClient(mockUUID, false, nil, errors.New("fail"))
		mockEventSender.On("Phase", api.CompleteFail, events.CreateAdminUser)

		err := attemptToCreateInitialUser(ctx, mockClient, mockEventSender, adminUser)
		require.NotNil(t, err)
		assert.Equal(t, "fail", err.Error())
	})
}

func TestConvergeDisabled(t *testing.T) {
	mockTarget := new(target.MockTarget)
	svr := testServer(mockTarget)
	svr.serverConfig = &Config{}

	t.Run("disabled when disable file exists", func(t *testing.T) {
		tempDir, _ := ioutil.TempDir("", "converge-disabled")
		defer os.RemoveAll(tempDir)
		disableFile := filepath.Join(tempDir, "converge_disabled")
		// create the file
		os.OpenFile(disableFile, os.O_RDONLY|os.O_CREATE, 0750)

		svr.serverConfig.ConvergeDisableFile = disableFile
		assert.True(t, svr.convergeDisabled(), "does not disable converge when disable file is present")
	})

	t.Run("enabled when disable file does not exist", func(t *testing.T) {
		tempDir, _ := ioutil.TempDir("", "converge-disabled")
		defer os.RemoveAll(tempDir)
		disableFile := filepath.Join(tempDir, "converge_disabled")

		svr.serverConfig.ConvergeDisableFile = disableFile
		assert.False(t, svr.convergeDisabled(), "disables the converging when disable file is not present")
	})
}
