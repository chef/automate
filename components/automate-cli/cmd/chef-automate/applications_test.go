package main

import (
	"errors"
	"testing"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/majorupgrade_utils"
	"github.com/spf13/cobra"
	"github.com/stretchr/testify/assert"
)

type MockExecutor struct{}

func (me MockExecutor) execCommand(cmd string) (string, error) {
	return `id   	svc.group    	release                         	FQDN                                       	app:env            	status
37035	redis.default	core/redis/4.0.14/20220311173537	ip-172-31-41-174.eu-west-3.compute.internal	DummyApp:acceptance	OK
37033	nginx.default	core/nginx/1.21.4/20220311143833	ip-172-31-41-174.eu-west-3.compute.internal	DummyApp:acceptance	OK
37034	postgresql94.default	core/postgresql94/9.4.26/20220311203133	ip-172-31-41-174.eu-west-3.compute.internal	DummyApp:acceptance	OK
`, nil
}

func (me MockExecutor) runCommandOnSingleAutomateNode(cmd *cobra.Command, args []string) (string, error) {
	return "", nil
}

func TestExecCommand(t *testing.T) {
	e := Executor{}
	tests := []struct {
		TestName       string
		Input          string
		ExpectedOutput string
		ExpectedError  error
	}{
		{"Valid Command", "echo Hello", "Hello\n", nil},
		{"Invalid Command", "ech Hello", "", errors.New("")},
	}
	for _, tt := range tests {
		t.Run(tt.TestName, func(t *testing.T) {
			output, err := e.execCommand(tt.Input)
			if tt.ExpectedError != nil {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				assert.Equal(t, tt.ExpectedOutput, output)
			}
		})
	}
}

func TestRemoveApplicationsHA(t *testing.T) {
	mockExec := MockExecutor{}
	actualExec := Executor{}
	cmd := &cobra.Command{}
	args := []string{}
	YesMockWriter := majorupgrade_utils.NewCustomWriterWithInputs("y")
	NoMockWriter := majorupgrade_utils.NewCustomWriterWithInputs("n")
	tests := []struct {
		TestName               string
		Executor               IExecutor
		CliWriter              *cli.Writer
		AutoApproveFlagEnabled bool
		ExpectedError          error
	}{
		{
			TestName:               "used -y flag while running the command and everything works fine",
			Executor:               mockExec,
			CliWriter:              YesMockWriter.CliWriter,
			AutoApproveFlagEnabled: true,
			ExpectedError:          nil,
		},
		{
			TestName:               "not used -y flag and mocked show command returned list of services and user entered 'y' for confirmation prompt to remove",
			Executor:               mockExec,
			CliWriter:              YesMockWriter.CliWriter,
			AutoApproveFlagEnabled: false,
			ExpectedError:          nil,
		},
		{
			TestName:               "not used -y flag and but show command failed to run",
			Executor:               actualExec,
			CliWriter:              YesMockWriter.CliWriter,
			AutoApproveFlagEnabled: false,
			ExpectedError:          errors.New(""),
		},
		{
			TestName:               "not used -y flag, mocked show command returned list of services but user entered 'n' for confirmation prompt to remove",
			Executor:               mockExec,
			CliWriter:              NoMockWriter.CliWriter,
			AutoApproveFlagEnabled: false,
			ExpectedError:          nil,
		},
		{
			TestName:               "not used -y flag, mocked show command returned list of services but writer gave some error",
			Executor:               mockExec,
			CliWriter:              writer,
			AutoApproveFlagEnabled: false,
			ExpectedError:          errors.New("failed to read user input in confirmation: EOF"),
		},
		{
			TestName:               "used -y flag while running the command but Command running on automate node not successfully ended",
			Executor:               actualExec,
			CliWriter:              YesMockWriter.CliWriter,
			AutoApproveFlagEnabled: true,
			ExpectedError:          errors.New("Automate Ha infra confile file not exist"),
		},
	}

	for _, tt := range tests {
		t.Run(tt.TestName, func(t *testing.T) {
			removeSvcsFlags.yes = tt.AutoApproveFlagEnabled
			err := removeApplicationsHA(cmd, args, tt.Executor, tt.CliWriter)
			if tt.ExpectedError != nil {
				assert.Error(t, err)
				assert.ErrorContains(t, err, tt.ExpectedError.Error())
			} else {
				assert.NoError(t, err)
			}
		})
	}
	// Resetting the Global variable to its default value
	removeSvcsFlags.yes = false
}

func TestRunApplicationsRemoveSvcsCmd(t *testing.T) {
	cmd := &cobra.Command{}
	args := []string{}

	tests := []struct {
		TestName       string
		FilterApplied  bool
		AllFlagEnabled bool
		ExpectedError  error
	}{
		{
			TestName:       "Didn't used all or any filter flag",
			FilterApplied:  false,
			AllFlagEnabled: false,
			ExpectedError:  errors.New("You must filter the services to be deleted or pass the --all flag to delete all services"),
		},
		{
			TestName:       "used all flag",
			FilterApplied:  false,
			AllFlagEnabled: true,
			ExpectedError:  errors.New(""),
		},
		{
			TestName:       "used some filter flag",
			FilterApplied:  true,
			AllFlagEnabled: false,
			ExpectedError:  errors.New(""),
		},
	}

	for _, tt := range tests {
		t.Run(tt.TestName, func(t *testing.T) {
			removeSvcsFlags.all = tt.AllFlagEnabled
			if tt.FilterApplied {
				applicationsServiceFiltersFlags.serviceName = "anything"
			} else {
				applicationsServiceFiltersFlags.serviceName = ""
			}
			err := runApplicationsRemoveSvcsCmd(cmd, args)
			if tt.ExpectedError != nil {
				assert.Error(t, err)
				assert.ErrorContains(t, err, tt.ExpectedError.Error())
			} else {
				assert.NoError(t, err)
			}
		})
	}

	// Resetting the Global variable to its default value
	removeSvcsFlags.all = false
	applicationsServiceFiltersFlags.serviceName = ""
}

func TestShowApplicationsHA(t *testing.T) {
	mockExec := MockExecutor{}
	actualExec := Executor{}
	cmd := &cobra.Command{}
	args := []string{}

	tests := []struct {
		TestName      string
		Executor      IExecutor
		ExpectedError error
	}{
		{
			TestName:      "Command Ran Successfully",
			Executor:      mockExec,
			ExpectedError: nil,
		},
		{
			TestName:      "Command Gave Error ",
			Executor:      actualExec,
			ExpectedError: errors.New(""),
		},
	}

	for _, tt := range tests {
		t.Run(tt.TestName, func(t *testing.T) {
			err := showApplicationsHA(cmd, args, tt.Executor)
			if tt.ExpectedError != nil {
				assert.Error(t, err)
				assert.ErrorContains(t, err, tt.ExpectedError.Error())
			} else {
				assert.NoError(t, err)
			}
		})
	}
}

func TestRunApplicationsShowSvcsCmd(t *testing.T) {
	cmd := &cobra.Command{}
	args := []string{}

	tests := []struct {
		TestName      string
		ExpectedError error
	}{
		{
			TestName:      "Standalone System",
			ExpectedError: errors.New(""),
		},
	}

	for _, tt := range tests {
		t.Run(tt.TestName, func(t *testing.T) {
			err := runApplicationsShowSvcsCmd(cmd, args)
			if tt.ExpectedError != nil {
				assert.Error(t, err)
				assert.ErrorContains(t, err, tt.ExpectedError.Error())
			} else {
				assert.NoError(t, err)
			}
		})
	}
}

func TestMakeServicesReqWithFilters(t *testing.T) {
	applicationsServiceFiltersFlags.origin = "origin_value"
	applicationsServiceFiltersFlags.serviceName = "servicename_value"
	applicationsServiceFiltersFlags.version = "version_value"
	applicationsServiceFiltersFlags.channel = "channel_value"
	applicationsServiceFiltersFlags.application = "app_value"
	applicationsServiceFiltersFlags.environment = "environment_value"
	applicationsServiceFiltersFlags.site = "site_value"
	applicationsServiceFiltersFlags.buildTimestamp = "timestamp_value"
	applicationsServiceFiltersFlags.groupName = "group_value"
	res := makeServicesReqWithFilters()
	ExpectedResp := &applications.ServicesReq{
		Filter: []string{
			"origin:origin_value",
			"service:servicename_value",
			"version:version_value",
			"channel:channel_value",
			"application:app_value",
			"environment:environment_value",
			"site:site_value",
			"buildstamp:timestamp_value",
			"group:group_value",
		},
	}
	assert.Equal(t, ExpectedResp, res)

	// Setting Disconnected Flag
	applicationsServiceFiltersFlags.disconnected = true
	res = makeServicesReqWithFilters()
	ExpectedResp = &applications.ServicesReq{
		Filter: []string{
			"status:disconnected",
			"origin:origin_value",
			"service:servicename_value",
			"version:version_value",
			"channel:channel_value",
			"application:app_value",
			"environment:environment_value",
			"site:site_value",
			"buildstamp:timestamp_value",
			"group:group_value",
		},
	}
	assert.Equal(t, ExpectedResp, res)

	// Resetting the Global variable to its default value
	applicationsServiceFiltersFlags = applicationsServiceFilters{}
}

func TestPrintTSV(t *testing.T) {
	s := serviceSet{
		services: []*applications.Service{
			{
				Id:           "37035",
				Group:        "redis.default",
				Release:      "core/redis/4.0.14/20220311173537",
				Fqdn:         "ip-172-31-41-174.eu-west-3.compute.internal",
				Application:  "DummyApp",
				Environment:  "acceptance",
				Disconnected: true,
			},
			{
				Id:           "37035",
				Group:        "redis.default",
				Release:      "core/redis/4.0.14/20220311173537",
				Fqdn:         "ip-172-31-41-174.eu-west-3.compute.internal",
				Application:  "DummyApp",
				Environment:  "acceptance",
				Disconnected: false,
			},
		},
	}
	err := s.PrintTSV()
	assert.NoError(t, err)
}
