package config_test

import (
	"io/ioutil"
	"os"
	"testing"

	subject "github.com/chef/automate/components/compliance-service/config"
	project_update_lib "github.com/chef/automate/lib/authz"
	"github.com/stretchr/testify/assert"
)

const cFile = "/tmp/.compliance-service-test-delete-me.toml"

func TestManagerNewDefaultConfig(t *testing.T) {
	os.Remove(cFile)
	configMgr, err := subject.NewConfigManager(cFile)
	defer configMgr.Close()
	assert.NoError(t, err)

	// Only three jobs configured
	assert.Equal(t, project_update_lib.NotRunningState, configMgr.GetProjectUpdateConfig().State)
}

func TestManagerBadFile(t *testing.T) {
	_, err := subject.NewConfigManager("")
	assert.Error(t, err)
}

func TestManagerConfigProjectUpdateConfig(t *testing.T) {
	// Writing config file
	data := []byte(`
	
[project_update_config]
	es_job_ids = ["Cmv2zbcVT4KSEz3b98IsmQ:57148","Cmv2zbcVT4KSEz3b98IsmQ:57186"]
	project_update_id = "4256e26e-92b1-4b1d-8679-44ec74b5299a"
	state = "not_running"
  `)
	err := ioutil.WriteFile(cFile, data, 0644)
	defer os.Remove(cFile)
	assert.Nil(t, err)

	// New config should load the file
	configMgr, err := subject.NewConfigManager(cFile)
	defer configMgr.Close()
	assert.NoError(t, err)
	projectUpdateConfig := configMgr.GetProjectUpdateConfig()
	assert.Equal(t, project_update_lib.NotRunningState, projectUpdateConfig.State)
	assert.Equal(t, "4256e26e-92b1-4b1d-8679-44ec74b5299a", projectUpdateConfig.ProjectUpdateID)
	assert.Equal(t, 2, len(projectUpdateConfig.EsJobIDs))

	projectUpdateConfig.State = "running"
	err = configMgr.UpdateProjectUpdateConfig(projectUpdateConfig)
	assert.NoError(t, err)

	projectUpdateConfig = configMgr.GetProjectUpdateConfig()
	assert.Equal(t, "running", projectUpdateConfig.State)
}
