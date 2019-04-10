package session

import (
	"testing"

	"github.com/stretchr/testify/assert"

	config "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

func TestValidateConfigRequestValid(t *testing.T) {
	t.Run("bare-bones config returns no error", func(t *testing.T) {
		c := newValidConfigRequest()
		err := c.Validate()
		assert.Nil(t, err)
	})

	t.Run("with bldr config returns no error", func(t *testing.T) {
		c := newValidBldrConfigRequest()
		err := c.Validate()
		assert.Nil(t, err)
	})

	t.Run("with any bldr config that is missing a value returns an error", func(t *testing.T) {
		configs := newInvalidPartialBldrConfigRequestPermutations()

		for _, cfg := range configs {
			err := cfg.Validate()
			// This needs to be not nil because the Validate returns a custom error
			// type that isn't a go Error.
			assert.NotNil(t, err)
		}
	})
}

func TestSetGlobalConfig(t *testing.T) {
	t.Run("it configures the MLSA",
		func(t *testing.T) {
			c := newValidConfigRequest()
			c.V1.Sys.Mlsa.Accept = w.Bool(false)
			g := config.DefaultGlobalConfig()
			g.V1.Mlsa.Accept = w.Bool(true)

			c.SetGlobalConfig(g)

			assert.True(t, c.V1.Sys.Mlsa.Accept.Value)
		},
	)
	t.Run("it sets the FQDN",
		func(t *testing.T) {
			c := newValidConfigRequest()
			c.V1.Sys.Service.ExternalFqdn = w.String("localhost")

			g := config.DefaultGlobalConfig()
			g.V1.Fqdn = w.String("external_fqdn")

			c.SetGlobalConfig(g)

			assert.Equal(t, "external_fqdn", c.V1.Sys.Service.ExternalFqdn.Value)
		},
	)
}

func TestIfEmptyButOthersAreNot(t *testing.T) {
	t.Run("when the slice is empty return false", func(t *testing.T) {
		assert.Equal(t, IfEmptyButOthersAreNot("test", []string{}), false)
	})

	t.Run("when the first string is empty and the slice is empty return false", func(t *testing.T) {
		assert.Equal(t, IfEmptyButOthersAreNot("", []string{}), false)
	})

	t.Run("when the first string is empty and so are the others return false", func(t *testing.T) {
		assert.Equal(t, IfEmptyButOthersAreNot("", []string{"", ""}), false)
	})

	t.Run("when the first string isn't empty but the others are returns false", func(t *testing.T) {
		assert.Equal(t, IfEmptyButOthersAreNot("notempty", []string{"", ""}), false)
	})

	t.Run("when the first string is empty but the others are not returns true", func(t *testing.T) {
		assert.Equal(t, IfEmptyButOthersAreNot("", []string{"", "test"}), true)
	})
}

func newValidConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.ExternalFqdn = w.String("localhost")

	return c
}

func newValidBldrConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	c.V1.Sys.Service.ExternalFqdn = w.String("localhost")
	c.V1.Sys.Service.BldrSigninUrl = w.String("https://someurl.com/callback")
	c.V1.Sys.Service.BldrClientId = w.String("myclient")
	c.V1.Sys.Service.BldrClientSecret = w.String("somesecret")

	return c
}

func newInvalidPartialBldrConfigRequestPermutations() []*ConfigRequest {
	c1 := NewConfigRequest()
	c1.V1.Sys.Service.ExternalFqdn = w.String("localhost")
	c1.V1.Sys.Service.BldrSigninUrl = w.String("https://someurl.com/callback")

	c2 := NewConfigRequest()
	c2.V1.Sys.Service.ExternalFqdn = w.String("localhost")
	c2.V1.Sys.Service.BldrClientSecret = w.String("some_client_secret")

	c3 := NewConfigRequest()
	c3.V1.Sys.Service.ExternalFqdn = w.String("localhost")
	c3.V1.Sys.Service.BldrClientId = w.String("some_client_id")

	return []*ConfigRequest{c1, c2, c3}
}
