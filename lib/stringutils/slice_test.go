package stringutils_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/lib/stringutils"
)

func TestSliceContains(t *testing.T) {
	assert.False(t, stringutils.SliceContains([]string{}, "item"))

	assert.True(t, stringutils.SliceContains([]string{"item"}, "item"))
	assert.False(t, stringutils.SliceContains([]string{"item"}, "item2"))

	assert.True(t, stringutils.SliceContains([]string{"item1", "item2"}, "item2"))
	assert.False(t, stringutils.SliceContains([]string{"item1", "item2"}, "item"))
	assert.False(t, stringutils.SliceContains([]string{"  item1", "item2"}, "item1"))

	assert.False(t, stringutils.SliceContains([]string{"item1", "item2"}, ""))
	assert.True(t, stringutils.SliceContains([]string{"item1", ""}, ""))
}

func TestSliceFilter(t *testing.T) {
	tests := []struct {
		in       []string
		expected []string
	}{
		{in: []string{"foo", "bar", "item"}, expected: []string{"item"}},
		{in: []string{"foo", "bar", "baz"}, expected: []string{}},
		{in: []string{}, expected: []string{}},
	}

	for _, test := range tests {
		actual := stringutils.SliceFilter(test.in, func(element string) bool {
			return len(element) > 3
		})

		assert.Equal(t, test.expected, actual)
	}
}

func TestIndexOf(t *testing.T) {
	i, err := stringutils.IndexOf([]string{}, "item")
	assert.Equal(t, -1, i)
	assert.Equal(t, stringutils.ErrNotFound, err)

	i, err = stringutils.IndexOf([]string{"item"}, "item1")
	assert.Equal(t, -1, i)
	assert.Equal(t, stringutils.ErrNotFound, err)

	i, err = stringutils.IndexOf([]string{"item"}, "item")
	assert.NoError(t, err)
	assert.Equal(t, 0, i)

	i, err = stringutils.IndexOf([]string{"item", "item"}, "item")
	assert.NoError(t, err)
	assert.Equal(t, 0, i)

	i, err = stringutils.IndexOf([]string{"item1", "item", "item"}, "item")
	assert.NoError(t, err)
	assert.Equal(t, 1, i)

	i, err = stringutils.IndexOf([]string{"item1", "item2", "item3"}, "item3")
	assert.NoError(t, err)
	assert.Equal(t, 2, i)
}
