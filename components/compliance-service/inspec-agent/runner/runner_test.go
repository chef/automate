package runner

import (
	"testing"

	"github.com/chef/automate/components/compliance-service/inspec"
	"github.com/chef/automate/components/compliance-service/inspec-agent/types"
	"github.com/stretchr/testify/assert"
)

func TestNodeHasSecrets(t *testing.T) {
	tc := inspec.TargetConfig{
		TargetBaseConfig: inspec.TargetBaseConfig{
			Backend: "ssh",
		},
		SecretsArr: []*inspec.Secrets{},
	}
	res := nodeHasSecrets(&tc)
	assert.Equal(t, false, res)

	tc.User = "my-user"
	res = nodeHasSecrets(&tc)
	assert.Equal(t, false, res)

	tc.Password = "my-password"
	res = nodeHasSecrets(&tc)
	assert.Equal(t, true, res)
}

func TestPotentialTargetConfigs(t *testing.T) {
	job := types.InspecJob{
		TargetConfig: inspec.TargetConfig{
			TargetBaseConfig: inspec.TargetBaseConfig{
				Backend:  "ssh",
				Hostname: "blabla",
			},
			SecretsArr: []*inspec.Secrets{
				{User: "a", Password: "b"},
				{User: "c", Password: "d"},
			},
		},
	}

	targetConfigs := potentialTargetConfigs(&job)

	assert.Equal(t, inspec.TargetConfig{
		TargetBaseConfig: inspec.TargetBaseConfig{
			Backend:  "ssh",
			Hostname: "blabla",
		}, Secrets: inspec.Secrets{
			User:     "a",
			Password: "b",
		},
	}, targetConfigs[0])

	assert.Equal(t, inspec.TargetConfig{
		TargetBaseConfig: inspec.TargetBaseConfig{
			Backend:  "ssh",
			Hostname: "blabla",
		}, Secrets: inspec.Secrets{
			User:     "c",
			Password: "d",
		},
	}, targetConfigs[1])
}
