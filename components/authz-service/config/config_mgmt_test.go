package config_test

import (
	"testing"

	"github.com/chef/automate/components/authz-service/config"
	"github.com/stretchr/testify/assert"
)

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
		FailureMessages: []string{"err"},
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
