package migrations

import (
	"context"
	"testing"

	"github.com/stretchr/testify/require"
)

type MockProbe struct {
	ctx             context.Context
	deployedProdcts []string
}

func (m MockProbe) Ctx() context.Context {
	if m.ctx == nil {
		return context.TODO()
	}
	return m.ctx
}

func (m MockProbe) DeployedProducts() []string {
	return m.deployedProdcts
}

func TestCheckWorkflowNotDeployed(t *testing.T) {
	check := CheckWorkflowNotDeployed()
	require.NotEmpty(t, check.Name)
	require.NotEmpty(t, check.Description)
	t.Run("success is false if deployed products contains workflow", func(t *testing.T) {
		probe := MockProbe{
			deployedProdcts: []string{"automate", "workflow"},
		}
		checkStatus, err := check.Run(probe)
		require.NoError(t, err)
		require.False(t, checkStatus.Success)
		require.NotEmpty(t, checkStatus.Message)
		require.NotEmpty(t, checkStatus.Remediation)
	})

	t.Run("success is true if deployed products does not contain workflow", func(t *testing.T) {
		probe := MockProbe{
			deployedProdcts: []string{"automate", "chef-server"},
		}
		checkStatus, err := check.Run(probe)
		require.NoError(t, err)
		require.True(t, checkStatus.Success)
		require.NotEmpty(t, checkStatus.Message)
		require.Empty(t, checkStatus.Remediation)
	})
}
