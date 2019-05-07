package config_test

import (
	"io/ioutil"
	"os"
	"testing"

	"github.com/chef/automate/components/authz-service/config"
	event_ids "github.com/chef/automate/lib/event"
	"github.com/stretchr/testify/assert"
)

const cFile = "/tmp/.authz-service-test-delete-me.toml"

func TestManagerConfigProjectUpdateConfig(t *testing.T) {
	// Writing config file
	data := []byte(`
	
[project_update_config]
	failed = false
	failure_message = ""
	project_update_id = "4256e26e-92b1-4b1d-8679-44ec74b5299a"
	state = "not_running"

  [[project_update_config.domain_services]]
    complete = false
    estimated_time_compelete = 0001-01-01T00:00:00Z
    failed = false
    failure_message = ""
    last_update = 2019-04-26T21:13:34Z
    name = "complianceInspecReport"
    percentage_complete = 0.0

  [[project_update_config.domain_services]]
    complete = false
    estimated_time_compelete = 0001-01-01T00:00:00Z
    failed = false
    failure_message = ""
    last_update = 2019-04-26T21:13:34Z
    name = "infraClientRuns"
    percentage_complete = 0.0
  `)
	err := ioutil.WriteFile(cFile, data, 0644)
	defer os.Remove(cFile)
	assert.Nil(t, err)

	// New config should load the file
	configMgr, err := config.NewManager(cFile)
	defer configMgr.Close()
	assert.NoError(t, err)

	projectUpdateStage := configMgr.GetProjectUpdateStage()
	assert.Equal(t, config.NotRunningState, projectUpdateStage.State)
	assert.Equal(t, "4256e26e-92b1-4b1d-8679-44ec74b5299a", projectUpdateStage.ProjectUpdateID)
	assert.Equal(t, 2, len(projectUpdateStage.DomainServices))

	projectUpdateStage.State = "running"
	err = configMgr.UpdateProjectUpdateStage(projectUpdateStage)
	assert.NoError(t, err)

	projectUpdateStage = configMgr.GetProjectUpdateStage()
	assert.Equal(t, "running", projectUpdateStage.State)
}

func TestManagerBadFile(t *testing.T) {
	_, err := config.NewManager("")
	assert.Error(t, err)
}

func TestManagerConfigProjectUpdateConfigDefault(t *testing.T) {
	os.Remove(cFile)
	configManager, err := config.NewManager(cFile)
	defer configManager.Close()
	assert.NoError(t, err)

	projectUpdateStage := configManager.GetProjectUpdateStage()
	assert.Equal(t, config.NotRunningState, projectUpdateStage.State)
	assert.Equal(t, 2, len(projectUpdateStage.DomainServices))

	names := make([]string, 2)
	for index, domainService := range projectUpdateStage.DomainServices {
		names[index] = domainService.Name
	}
	assert.ElementsMatch(t,
		[]string{event_ids.ComplianceInspecReportProducerID, event_ids.InfraClientRunsProducerID}, names)
}

func TestProjectUpdateStageEqual(t *testing.T) {
	stage := config.ProjectUpdateStage{}
	assert.True(t, stage.Equal(config.ProjectUpdateStage{}))

	assert.False(t, stage.Equal(config.ProjectUpdateStage{
		State: config.NotRunningState,
	}))

	assert.False(t, stage.Equal(config.ProjectUpdateStage{
		Failed: true,
	}))

	assert.False(t, stage.Equal(config.ProjectUpdateStage{
		ProjectUpdateID: "id",
	}))

	assert.False(t, stage.Equal(config.ProjectUpdateStage{
		FailureMessage: "err",
	}))

	assert.False(t, stage.Equal(config.ProjectUpdateStage{
		DomainServices: []config.ProjectUpdateDomainService{
			config.ProjectUpdateDomainService{
				Name: "one",
			},
			config.ProjectUpdateDomainService{
				Name: "two",
			},
		},
	}))
}

func TestProjectUpdateStageEqual2(t *testing.T) {
	stage := config.ProjectUpdateStage{
		DomainServices: []config.ProjectUpdateDomainService{
			config.ProjectUpdateDomainService{
				Name: "one",
			},
			config.ProjectUpdateDomainService{
				Name: "two",
			},
		},
	}

	assert.False(t, stage.Equal(config.ProjectUpdateStage{
		DomainServices: []config.ProjectUpdateDomainService{
			config.ProjectUpdateDomainService{
				Name: "three",
			},
			config.ProjectUpdateDomainService{
				Name: "four",
			},
		},
	}))

	assert.False(t, stage.Equal(config.ProjectUpdateStage{
		DomainServices: []config.ProjectUpdateDomainService{
			config.ProjectUpdateDomainService{
				PercentageComplete: 0.3,
			},
			config.ProjectUpdateDomainService{
				PercentageComplete: 0.9,
			},
		},
	}))

	assert.True(t, stage.Equal(config.ProjectUpdateStage{
		DomainServices: []config.ProjectUpdateDomainService{
			config.ProjectUpdateDomainService{
				Name: "one",
			},
			config.ProjectUpdateDomainService{
				Name: "two",
			},
		},
	}))
}
