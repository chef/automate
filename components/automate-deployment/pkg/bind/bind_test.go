package bind_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/bind"
)

func TestParseData(t *testing.T) {
	t.Run("can read one dep per line", func(t *testing.T) {
		data := `
foo REQUIRED bar
baz REQUIRED qux
quux OPTIONAL quuz
garply OPTIONAL fred
`
		b, err := bind.ParseData([]byte(data))
		require.NoError(t, err)
		expected := bind.Binds{
			Required: bind.Bindmap{
				"foo": []string{"bar"},
				"baz": []string{"qux"},
			},
			Optional: bind.Bindmap{
				"quux":   []string{"quuz"},
				"garply": []string{"fred"},
			},
			BindMode: bind.BindModeMap{},
		}
		assert.Equal(t, expected, b)
	})

	t.Run("can read multiple deps per line", func(t *testing.T) {
		data := `
foo REQUIRED bar baz
quux OPTIONAL quuz fred
`
		b, err := bind.ParseData([]byte(data))
		require.NoError(t, err)
		expected := bind.Binds{
			Required: bind.Bindmap{
				"foo": []string{"bar", "baz"},
			},
			Optional: bind.Bindmap{
				"quux": []string{"quuz", "fred"},
			},
			BindMode: bind.BindModeMap{},
		}
		assert.Equal(t, expected, b)
	})

	t.Run("skips empty lines", func(t *testing.T) {
		data := `
foo REQUIRED bar baz

quux OPTIONAL quuz fred
`
		b, err := bind.ParseData([]byte(data))
		require.NoError(t, err)
		expected := bind.Binds{
			Required: bind.Bindmap{
				"foo": []string{"bar", "baz"},
			},
			Optional: bind.Bindmap{
				"quux": []string{"quuz", "fred"},
			},
			BindMode: bind.BindModeMap{},
		}
		assert.Equal(t, expected, b)
	})

	t.Run("returns an error if an unknown binding type is used", func(t *testing.T) {
		data := "foo BAR baz\n"
		_, err := bind.ParseData([]byte(data))
		assert.Equal(t, bind.ErrUnknownBindType, err)
	})

	t.Run("returns an error there are not binds for a listed component", func(t *testing.T) {
		data := "foo OPTIONAL\n"
		_, err := bind.ParseData([]byte(data))
		assert.Equal(t, bind.ErrNotEnoughParams, err)
	})

	t.Run("can read binding mode", func(t *testing.T) {
		data := "foo BINDING_MODE relaxed\nbar BINDING_MODE strict"
		expected := bind.Binds{
			Required: make(bind.Bindmap),
			Optional: make(bind.Bindmap),
			BindMode: bind.BindModeMap{
				"foo": "relaxed",
				"bar": "strict",
			},
		}
		actual, err := bind.ParseData([]byte(data))
		require.NoError(t, err)
		assert.Equal(t, expected, actual)
	})

	t.Run("returns an error for invalid binding mode", func(t *testing.T) {
		data := "foo BINDING_MODE invalid-data-here"
		_, err := bind.ParseData([]byte(data))
		assert.Error(t, err)
		assert.Equal(t, `Unknown binding mode "invalid-data-here"; valid values are "strict" or "relaxed"`, err.Error())
	})
}

func TestBindsCollection(t *testing.T) {
	binds := bind.Binds{
		Required: bind.Bindmap{
			"foo": []string{"bar", "baz"},
		},
		Optional: bind.Bindmap{
			"quux": []string{"quuz", "fred"},
		},
		BindMode: bind.BindModeMap{
			"foo":  "strict",
			"quux": "relaxed",
		},
	}

	t.Run("returns all the binds for a given service", func(t *testing.T) {
		expectedFooBinds := []string{"bar", "baz"}
		expectedQuuxBinds := []string{"quuz", "fred"}

		assert.Equal(t, expectedFooBinds, binds.AllForService("foo"))
		assert.Equal(t, expectedQuuxBinds, binds.AllForService("quux"))
		assert.Empty(t, binds.AllForService("no-binds-service"))
	})

	t.Run("returns binding mode for a given service", func(t *testing.T) {
		actual, err := binds.ModeForService("foo")
		require.NoError(t, err)
		assert.Equal(t, "strict", actual)
		actual, err = binds.ModeForService("quux")
		require.NoError(t, err)
		assert.Equal(t, "relaxed", actual)
	})

	t.Run("returns default 'strict' for unknown service bind mode", func(t *testing.T) {
		_, err := binds.ModeForService("invalid-service-name")
		assert.Error(t, err)
		assert.Equal(t, "No binding mode data exists for service \"invalid-service-name\"", err.Error())
	})

}

func TestBinds(t *testing.T) {
	t.Run("Can create a unbound bind", func(t *testing.T) {
		bind := bind.New("foo", "")
		assert.Equal(t, "foo", bind.Name())
		assert.Equal(t, "", bind.ServiceGroup())
		assert.Equal(t, false, bind.IsBound())
	})

	t.Run("Can create a bound bind", func(t *testing.T) {
		bind := bind.New("foo", "bar.default")
		assert.Equal(t, "foo", bind.Name())
		assert.Equal(t, "bar.default", bind.ServiceGroup())
		assert.Equal(t, true, bind.IsBound())
	})

	t.Run("Zero length binds are equal", func(t *testing.T) {
		bindsA := bind.WithBinds()
		bindsB := bind.WithBinds()

		assert.True(t, bindsA.Equal(bindsB))
		assert.True(t, bindsB.Equal(bindsA))
	})

	t.Run("Binds equality doesn't care about sort order", func(t *testing.T) {
		bindsA := bind.WithBinds(
			bind.New("foo", ""),
			bind.New("bar", ""),
			bind.New("baz", ""),
		)

		bindsB := bind.WithBinds(
			bind.New("baz", ""),
			bind.New("foo", ""),
			bind.New("bar", ""),
		)

		assert.True(t, bindsA.Equal(bindsB))
		assert.True(t, bindsB.Equal(bindsA))
	})

	t.Run("Not equal binds", func(t *testing.T) {

		t.Run("One empty", func(t *testing.T) {
			bindsA := bind.WithBinds(
				bind.New("foo", ""),
				bind.New("bar", ""),
				bind.New("baz", ""),
			)

			bindsB := bind.WithBinds()

			assert.False(t, bindsA.Equal(bindsB))
			assert.False(t, bindsB.Equal(bindsA))
		})

		t.Run("Different sized and filled", func(t *testing.T) {
			bindsA := bind.WithBinds(
				bind.New("foo", ""),
				bind.New("bar", ""),
				bind.New("baz", ""),
			)

			bindsB := bind.WithBinds(
				bind.New("foo", ""),
				bind.New("bar", ""),
			)
			assert.False(t, bindsA.Equal(bindsB))
			assert.False(t, bindsB.Equal(bindsA))
		})

	})
}

func TestDefaultsForService(t *testing.T) {
	binds := bind.Binds{
		Required: bind.Bindmap{
			"req-service":         []string{"baz", "bar"},
			"opt-and-req-service": []string{"fred"},
		},
		Optional: bind.Bindmap{
			"bad-mode-service":    []string{"quuz", "fred"},
			"opt-and-req-service": []string{"quuz"},
		},
		BindMode: bind.BindModeMap{
			"req-service":         "strict",
			"opt-and-req-service": "relaxed",
			"bad-mode-service":    "",
		},
	}
	t.Run("returns an error if the bind mode isn't found", func(t *testing.T) {
		_, err := binds.DefaultsForService("not-there")
		assert.Error(t, err)
	})

	t.Run("returns an error if the bind mode is empty", func(t *testing.T) {
		_, err := binds.DefaultsForService("bad-mode-service")
		assert.Error(t, err)
	})

	t.Run("returns the binds mapped to the default service group", func(t *testing.T) {
		i, err := binds.DefaultsForService("req-service")
		assert.NoError(t, err)
		assert.Contains(t, i.Specs, "baz:baz.default")
		assert.Contains(t, i.Specs, "bar:bar.default")
	})

	t.Run("returns the bind mode", func(t *testing.T) {
		i, err := binds.DefaultsForService("req-service")
		assert.NoError(t, err)
		assert.Equal(t, "strict", i.Mode)
	})

	t.Run("returns returns both optional and required binds", func(t *testing.T) {
		i, err := binds.DefaultsForService("opt-and-req-service")
		assert.NoError(t, err)
		assert.Equal(t, 2, len(i.Specs))
	})

}
