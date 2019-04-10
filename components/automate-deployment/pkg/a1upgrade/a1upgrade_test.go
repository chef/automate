package a1upgrade

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestInitWithoutExplicitA2ConfigRespectsHartifactsAndOrigin(t *testing.T) {
	upgrade, err := NewA1Upgrade(
		WithDeliverySecrets(fixturePath("delivery-secrets.json")),
		WithDeliveryRunning(fixturePath("delivery-running.json")),
		WithOverrideOrigin("hab-origin"),
		WithHartifactsPath(fixturePath("")),
	)
	require.NoError(t, err)
	upgrade.GenerateA2ConfigIfNoneProvided("")

	// At the time of writing this test, this will be nil when the thing we're
	// testing is broken:
	require.NotNil(t, upgrade.A2Config.Deployment.V1.Svc.OverrideOrigin)
	require.NotNil(t, upgrade.A2Config.Deployment.V1.Svc.HartifactsPath)

	actualOrigin := upgrade.A2Config.Deployment.V1.Svc.OverrideOrigin.Value
	assert.Equal(t, actualOrigin, "hab-origin")

	actualHartsPath := upgrade.A2Config.Deployment.V1.Svc.HartifactsPath.Value
	assert.Equal(t, actualHartsPath, fixturePath(""))
}
