package config_test

import (
	"io/ioutil"
	"os"
	"testing"

	"github.com/stretchr/testify/assert"

	subject "github.com/chef/automate/components/ingest-service/config"
	project_update_lib "github.com/chef/automate/lib/authz"
)

const cFile = "/tmp/.ingest-service-test-delete-me.toml"

func TestManagerNewDefaultConfig(t *testing.T) {
	config, err := subject.NewManager(cFile)
	defer config.Close()
	assert.NoError(t, err)

	projectUpdateConfig := config.GetProjectUpdateConfig()
	assert.Equal(t, project_update_lib.NotRunningState, projectUpdateConfig.State, "the project update should be not running by default")
}

func TestManagerBadFile(t *testing.T) {
	_, err := subject.NewManager("")
	assert.Error(t, err)
}

func TestManagerConfigFileExistsButItsCorrupt(t *testing.T) {
	// Writing config file
	data := []byte(
		`
[	es_job_ids = ["Cmv2zbcVT4KSEz3b98IsmQ:94389","Cmv2zbcVT4KSEz3b98IsmQ:94429"]
	project_update_id = "4256e26e-92b1-4b1d-8679-44ec74b5299a"
	state = "running"
  `)
	err := ioutil.WriteFile(cFile, data, 0644)
	defer os.Remove(cFile)
	assert.Nil(t, err)

	// When the file is corrupt, we log out the failure and use the default config
	configMgr, err := subject.NewManager(cFile)
	defer configMgr.Close()
	assert.NoError(t, err)

	projectUpdateConfig := configMgr.GetProjectUpdateConfig()
	assert.Equal(t, project_update_lib.NotRunningState,
		projectUpdateConfig.State, "the scheduler should be running by default")
}

func TestManagerWriteConfigOnUpdatesAndLoadChanges(t *testing.T) {
	// Make sure there is no config file
	os.Remove(cFile)

	// Default config
	var (
		config, err = subject.NewManager(cFile)
	)
	defer config.Close()
	assert.NoError(t, err)

	projectUpdateConfig := config.GetProjectUpdateConfig()
	assert.Equal(t, project_update_lib.NotRunningState,
		projectUpdateConfig.State, "the scheduler should be running by default")

	projectUpdateConfig.State = project_update_lib.RunningState
	config.UpdateProjectUpdateConfig(projectUpdateConfig)
	// Check that we saved the config on disk
	_, err = os.Stat(cFile)
	assert.Nil(t, err, "config file should exist")

	// Create a new Manager config that should load the config automatically
	newConfig, err := subject.NewManager(cFile)
	defer newConfig.Close()
	assert.NoError(t, err)

	projectUpdateConfig = newConfig.GetProjectUpdateConfig()
	assert.Equal(t, project_update_lib.RunningState,
		projectUpdateConfig.State, "the scheduler should be running after the update")
}

func TestManagerWriteConfigThreadSafe(t *testing.T) {
	// default config
	config, err := subject.NewManager(cFile)
	defer config.Close()
	assert.NoError(t, err)

	// Lets use this job to send multiple updates
	projectUpdateConfig := config.GetProjectUpdateConfig()
	// Send 500 updates
	for i := 0; i < 500; i++ {
		// turn job on and off
		if projectUpdateConfig.State == project_update_lib.NotRunningState {
			projectUpdateConfig.State = project_update_lib.RunningState
		} else {
			projectUpdateConfig.State = project_update_lib.NotRunningState
		}
		config.UpdateProjectUpdateConfig(projectUpdateConfig)
	}

	// After smashing the config with updates
	newPUConfig := config.GetProjectUpdateConfig()
	assert.Equal(t, projectUpdateConfig.State, newPUConfig.State, "config should match")

	// Create a new Manager config that should load the config automatically
	newConfig, err := subject.NewManager(cFile)
	defer newConfig.Close()
	assert.NoError(t, err)

	newPUConfig = newConfig.GetProjectUpdateConfig()
	assert.Equal(t, projectUpdateConfig.State, newPUConfig.State, "config should match")
}

func TestManagerConfigProjectUpdateConfig(t *testing.T) {
	// Writing config file
	data := []byte(`
[project_update_config]
	es_job_ids = ["Cmv2zbcVT4KSEz3b98IsmQ:94389","Cmv2zbcVT4KSEz3b98IsmQ:94429"]
	project_update_id = "4256e26e-92b1-4b1d-8679-44ec74b5299a"
	state = "not_running"
  `)
	err := ioutil.WriteFile(cFile, data, 0644)
	defer os.Remove(cFile)
	assert.Nil(t, err)

	// New config should load the file
	configMgr, err := subject.NewManager(cFile)
	defer configMgr.Close()
	assert.NoError(t, err)

	projectUpdateConfig := configMgr.GetProjectUpdateConfig()
	assert.Equal(t, project_update_lib.NotRunningState, projectUpdateConfig.State)
	assert.Equal(t, "4256e26e-92b1-4b1d-8679-44ec74b5299a", projectUpdateConfig.ProjectUpdateID)
	assert.Equal(t,
		[]string{"Cmv2zbcVT4KSEz3b98IsmQ:94389", "Cmv2zbcVT4KSEz3b98IsmQ:94429"}, projectUpdateConfig.EsJobIDs)

	projectUpdateConfig.State = "running"
	err = configMgr.UpdateProjectUpdateConfig(projectUpdateConfig)
	assert.NoError(t, err)

	projectUpdateConfig = configMgr.GetProjectUpdateConfig()
	assert.Equal(t, "running", projectUpdateConfig.State)
}
