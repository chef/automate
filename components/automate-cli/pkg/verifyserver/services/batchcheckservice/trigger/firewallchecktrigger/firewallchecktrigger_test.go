package firewallchecktrigger

import (
	"testing"

	"github.com/chef/automate/components/automate-cli/pkg/verifyserver/models"
	"github.com/chef/automate/lib/logger"
	"github.com/stretchr/testify/require"
)

func TestFirewallCheck_Run(t *testing.T) {
	t.Run("All checks passes", func(t *testing.T) {
		fwc := NewFirewallCheck(logger.NewLogrusStandardLogger(), "")
		config := models.Config{}
		resp := fwc.Run(config)
		require.Nil(t, resp)
	})
}
