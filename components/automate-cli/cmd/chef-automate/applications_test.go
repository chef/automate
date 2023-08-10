package main

import (
	"errors"
	"os"
	"runtime"
	"testing"

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

func (me MockExecutor) RunCommandOnSingleAutomateNode(cmd *cobra.Command, args []string) (string, error) {
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
			RemoveSvcsFlags.yes = tt.AutoApproveFlagEnabled
			err := RemoveApplicationsHA(cmd, args, tt.Executor, tt.CliWriter)
			if tt.ExpectedError != nil {
				assert.Error(t, err)
				assert.ErrorContains(t, err, tt.ExpectedError.Error())
			} else {
				assert.NoError(t, err)
			}
		})
	}
}

func TestRunApplicationsRemoveSvcsCmd(t *testing.T) {
	cmd := &cobra.Command{}
	args := []string{}

	tests := []struct {
		TestName       string
		FilterApplied  bool
		AllFlagEnabled bool
		HA             bool
		ExpectedError  error
	}{
		{
			TestName:       "HA System && Didn't used all or any filter flag",
			FilterApplied:  false,
			AllFlagEnabled: false,
			HA:             true,
			ExpectedError:  errors.New("You must filter the services to be deleted or pass the --all flag to delete all services"),
		},
		{
			TestName:       "HA System && used all flag",
			FilterApplied:  false,
			AllFlagEnabled: true,
			HA:             true,
			ExpectedError:  errors.New(""),
		},
		{
			TestName:       "HA System && used some filter flag",
			FilterApplied:  true,
			AllFlagEnabled: false,
			HA:             true,
			ExpectedError:  errors.New(""),
		},
		{
			TestName:       "Standalone System",
			FilterApplied:  false,
			AllFlagEnabled: true,
			HA:             false,
			ExpectedError:  errors.New(""),
		},
	}

	for _, tt := range tests {
		t.Run(tt.TestName, func(t *testing.T) {
			if tt.HA {
				// In mac we can't create directory at root level, but for this test we need directory at root level.
				// So we are omitting the test in case of Mac OS.
				if runtime.GOOS == "darwin" {
					return
				}
				defer os.Remove(MakeHASystem(t).Name())
			}
			RemoveSvcsFlags.all = tt.AllFlagEnabled
			if tt.FilterApplied {
				ApplicationsServiceFiltersFlags.serviceName = "anything"
			} else {
				ApplicationsServiceFiltersFlags.serviceName = ""
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
			err := ShowApplicationsHA(cmd, args, tt.Executor)
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
		HA            bool
		ExpectedError error
	}{
		{
			TestName:      "HA System",
			HA:            true,
			ExpectedError: errors.New(""),
		},
		{
			TestName:      "Standalone System",
			HA:            false,
			ExpectedError: errors.New(""),
		},
	}

	for _, tt := range tests {
		t.Run(tt.TestName, func(t *testing.T) {
			if tt.HA {
				// In mac we can't create directory at root level, but for this test we need directory at root level.
				// So we are omitting the test in case of Mac OS.
				if runtime.GOOS == "darwin" {
					return
				}
				defer os.Remove(MakeHASystem(t).Name())
			}
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

func MakeHASystem(t *testing.T) *os.File {
	dirPath := initConfigHabA2HAPathFlag.a2haDirPath
	filePath := dirPath + "/a2ha.rb"

	err := os.MkdirAll(dirPath, os.ModePerm)
	assert.NoError(t, err, "Error creating directories")

	file, err := os.Create(filePath)
	assert.NoError(t, err, "Error creating file")
	return file
}
