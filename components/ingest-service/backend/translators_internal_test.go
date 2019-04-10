package backend

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestBackendInterfaceArrayToStringArray(t *testing.T) {
	cases := []struct {
		message  string
		value    []interface{}
		expected []string
	}{
		{
			message:  "expected array of strings should be transform correctly",
			value:    []interface{}{"a", "very", "normal", "array", "of", "strings"},
			expected: []string{"a", "very", "normal", "array", "of", "strings"},
		},
		{
			message: "map of interfaces should return empty string(s)",
			value: []interface{}{map[string]interface{}{
				"name": "Link",
				"age":  27,
				"weapons": []interface{}{
					"sword",
					"shield",
				},
			}},
			expected: []string{""},
		},
		{
			message:  "array of int should return empty string(s)",
			value:    []interface{}{1, 2, 3, 4, 5, 6, 7, 8, 9, 0},
			expected: []string{"", "", "", "", "", "", "", "", "", ""},
		},
		{
			message:  "array of float should return empty string(s)",
			value:    []interface{}{float32(11.11), float32(22.22)},
			expected: []string{"", ""},
		},
		{
			message:  "any kind of array should work",
			value:    []interface{}{"h", "o", "l", "a", " ", "a", "m", "i", "g", "o", "s"},
			expected: []string{"h", "o", "l", "a", " ", "a", "m", "i", "g", "o", "s"},
		},
	}

	for _, test := range cases {
		t.Run(test.message, func(t *testing.T) {
			result := interfaceArrayToStringArray(test.value)
			assert.Equal(t, test.expected, result)
		})
	}
}
