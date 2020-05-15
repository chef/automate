package shared_test

import (
	"testing"

	"github.com/chef/automate/api/config/dex"
	"github.com/chef/automate/api/config/shared"
	"github.com/chef/toml"
	"github.com/stretchr/testify/assert"
)

func TestAddKeyMissing(t *testing.T) {
	c := shared.NewInvalidConfigError()
	c.AddMissingKey("foo")

	assert.EqualValues(t, c.MissingKeys(), []string{"foo"})
}

func TestAddInvalidValue(t *testing.T) {
	c := shared.NewInvalidConfigError()
	c.AddInvalidValue("foo", "bar")

	assert.EqualValues(t, c.InvalidValues(), map[string]string{"foo": "bar"})
}

func TestDexSAMLAllowedGroupsOptionalList(t *testing.T) {
	t.Run("no list", func(t *testing.T) {
		c := loadDexSAMLConfigFromToml(`
groups_attr = "groups"
`)
		setting := c.GetAllowedGroups()
		assert.Equal(t, "groups", c.GetGroupsAttr().Value)
		assert.Equal(t, []string(nil), setting) // not set means nil
	})

	t.Run("empty list", func(t *testing.T) {
		c := loadDexSAMLConfigFromToml(`
allowed_groups = []
`)
		setting := c.GetAllowedGroups()
		assert.EqualValues(t, []string{}, setting) // set to empty list means []string{}
	})

	t.Run("non-empty list", func(t *testing.T) {
		c := loadDexSAMLConfigFromToml(`
allowed_groups = ["alices", "bobs"]
`)
		setting := c.GetAllowedGroups()
		assert.EqualValues(t, []string{"alices", "bobs"}, setting)
	})
}

func loadDexSAMLConfigFromToml(s string) *dex.ConfigRequest_V1_Saml {
	c := &dex.ConfigRequest_V1_Saml{}
	err := toml.Unmarshal([]byte(s), c)
	if err != nil {
		panic(err)
	}
	return c
}
