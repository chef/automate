package server

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestNamespace(t *testing.T) {
	examples := []struct {
		Domain string
		Name   string
	}{
		{
			Domain: "foo",
			Name:   "bar",
		},
		{
			Domain: "foo",
			Name:   "bar/baz",
		},
		{
			Domain: "foo",
			Name:   "/bar/baz",
		},
		{
			Domain: "foo",
			Name:   "/bar/baz/quux",
		},
	}

	for _, example := range examples {
		namespaced := namespace(example.Domain, example.Name)
		t.Run(namespaced, func(t *testing.T) {
			domain, name := unnamespace(namespaced)
			assert.Equal(t, example.Domain, domain)
			assert.Equal(t, example.Name, name)
		})
	}
}

func TestValidateDomain(t *testing.T) {
	t.Run("valid", func(t *testing.T) {
		err := validateDomain("valid")
		assert.NoError(t, err)
	})

	t.Run("invalid if empty", func(t *testing.T) {
		err := validateDomain("")
		assert.Error(t, err)
	})

	t.Run("invalid if contains spaces", func(t *testing.T) {
		err := validateDomain(" notvalid")
		assert.Error(t, err)

		err = validateDomain("notvalid ")
		assert.Error(t, err)

		err = validateDomain("not valid")
		assert.Error(t, err)
	})

	t.Run("invalid if contains /", func(t *testing.T) {
		err := validateDomain("not/valid")
		assert.Error(t, err)
	})
}
