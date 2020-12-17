package pg_gateway

import (
	"testing"

	ac "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/golang/protobuf/ptypes/wrappers"
	"github.com/stretchr/testify/require"
)

func TestPgGateway(t *testing.T) {
	t.Run("check nameserver by setting the enable nameserver flag", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.V1.Sys.Resolvers.EnableSystemNameservers = w.Bool(true)
		c.PrepareSystemConfig(&ac.TLSCredentials{})

		require.True(t, len(c.V1.Sys.Resolvers.NameServers) > 0)
	})

	t.Run("check nameserver by setting the nameservers", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.V1.Sys.Resolvers.NameServers = []*wrappers.StringValue{w.String("111.11.111.0")}
		c.PrepareSystemConfig(&ac.TLSCredentials{})

		require.True(t, len(c.V1.Sys.Resolvers.NameServers) > 0)
		require.False(t, c.V1.Sys.Resolvers.EnableSystemNameservers.GetValue())
	})

	t.Run("check nameserver by setting both the enable nameserver flag and the nameservers", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.V1.Sys.Resolvers.NameServers = []*wrappers.StringValue{w.String("111.11.111.0")}
		c.V1.Sys.Resolvers.EnableSystemNameservers = w.Bool(true)
		c.PrepareSystemConfig(&ac.TLSCredentials{})

		require.Equal(t, len(c.V1.Sys.Resolvers.NameServers), 1)
		require.Equal(t, c.V1.Sys.Resolvers.NameServers[0].GetValue(), "111.11.111.0")
		require.True(t, c.V1.Sys.Resolvers.EnableSystemNameservers.GetValue())
	})
}
