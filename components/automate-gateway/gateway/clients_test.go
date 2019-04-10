package gateway

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestConfigureDefaultEndpoints(t *testing.T) {
	assert := assert.New(t)
	cc := &ClientConfig{
		Endpoints: map[string]ConnectionOptions{
			"teams-service": ConnectionOptions{
				Target: "192.168.0.123:9966",
				Secure: false,
			},
		},
	}

	cc.configureDefaultEndpoints()

	// Make sure it doesn't overwrite our configured endpoints
	assert.Equal("192.168.0.123:9966", cc.Endpoints["teams-service"].Target)
	assert.Equal(false, cc.Endpoints["teams-service"].Secure)

	// Make sure it adds all default endpoints
	for service, target := range defaultEndpoints {
		if service == "teams-service" {
			continue
		}

		assert.Equal(target, cc.Endpoints[service].Target)
		assert.Equal(true, cc.Endpoints[service].Secure)
	}
}
