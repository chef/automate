package server

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestSafeStringFromMap(t *testing.T) {
	testData := map[string]interface{}{
		"test1": "validate",
		"test2": "",
		"test3": nil,
	}

	assert.Equal(t, "validate", SafeStringFromMap(testData, "test1"))
	assert.Equal(t, "", SafeStringFromMap(testData, "test2"))
	assert.Equal(t, "", SafeStringFromMap(testData, "test3"))
	assert.Equal(t, "", SafeStringFromMap(testData, "no-key"))
}

func TestSafeStringFromMapFloat(t *testing.T) {
	testData := map[string]interface{}{
		"test1": 27.909,
		"test2": 0.443433,
		"test3": nil,
	}

	assert.Equal(t, "2.7909E+01", SafeStringFromMapFloat(testData, "test1"))
	assert.Equal(t, "4.43433E-01", SafeStringFromMapFloat(testData, "test2"))
	assert.Equal(t, "", SafeStringFromMapFloat(testData, "test3"))
	assert.Equal(t, "", SafeStringFromMap(testData, "no-key"))
}

func TestSafeBooleanFromMap(t *testing.T) {
	testData := map[string]interface{}{
		"test1": true,
		"test2": false,
		"test3": nil,
	}

	assert.Equal(t, true, SafeBooleanFromMap(testData, "test1"))
	assert.Equal(t, false, SafeBooleanFromMap(testData, "test2"))
	assert.Equal(t, false, SafeBooleanFromMap(testData, "test3"))
	assert.Equal(t, false, SafeBooleanFromMap(testData, "no-key"))
}

func TestSafeSliceFromMap(t *testing.T) {
	testData := map[string]interface{}{
		"test1": []string{"tag1", "tag2", "tag3"},
		"test2": []string{},
		"test3": nil,
	}

	assert.Equal(t, []string{"tag1", "tag2", "tag3"}, SafeSliceFromMap(testData, "test1"))
	assert.Equal(t, []string{}, SafeSliceFromMap(testData, "test2"))
	assert.Equal(t, []string{}, SafeSliceFromMap(testData, "test3"))
	assert.Equal(t, []string{}, SafeSliceFromMap(testData, "no-key"))
}

func TestSubtractSlice(t *testing.T) {
	testData := []struct {
		slice1 []string
		slice2 []string
		output []string
	}{
		{
			[]string{"tag1", "tag2", "tag3"},
			[]string{"tag1"},
			[]string{"tag2", "tag3"},
		},
		{
			[]string{"tag1", "tag2", "tag3"},
			[]string{"tag1", "tag2"},
			[]string{"tag3"},
		},
		{
			[]string{"tag1", "tag2", "tag3"},
			[]string{"unknown"},
			[]string{"tag1", "tag2", "tag3"},
		},
		{
			[]string{"tag1", "tag2", "tag3"},
			[]string{"tag3", "unknown"},
			[]string{"tag1", "tag2"},
		},
		{
			[]string{"tag1", "tag2"},
			[]string{},
			[]string{"tag1", "tag2"},
		},
		{
			[]string{"tag1", "tag2"},
			[]string{"tag1", "tag2"},
			[]string{},
		},
		{
			[]string{},
			[]string{"tag1"},
			[]string{},
		},
		{
			[]string{},
			[]string{},
			[]string{},
		},
	}

	for _, test := range testData {
		result := SubtractSlice(test.slice1, test.slice2)
		assert.Equal(t, test.output, result)
	}
}

func TestRemoveElement(t *testing.T) {
	testData := []struct {
		slice   []string
		element string
		output  []string
	}{
		{
			[]string{"tag1", "tag2", "tag3"},
			"tag1",
			[]string{"tag2", "tag3"},
		},
		{
			[]string{"tag1", "tag2", "tag3"},
			"unknown",
			[]string{"tag1", "tag2", "tag3"},
		},
		{
			[]string{"tag1", "tag2", "tag3"},
			"",
			[]string{"tag1", "tag2", "tag3"},
		},
		{
			[]string{},
			"tag",
			[]string{},
		},
		{
			[]string{"tag1"},
			"tag1",
			[]string{},
		},
		{
			[]string{},
			"",
			[]string{},
		},
	}

	for _, test := range testData {
		result := RemoveElement(test.slice, test.element)
		assert.Equal(t, test.output, result)
	}
}

func TestUnique(t *testing.T) {
	testData := []struct {
		slice  []string
		output []string
	}{
		{
			[]string{"tag1", "tag2", "tag3"},
			[]string{"tag1", "tag2", "tag3"},
		},
		{
			[]string{"tag1", "tag1", "tag2", "tag3"},
			[]string{"tag1", "tag2", "tag3"},
		},
		{
			[]string{"tag1", "tag2", "tag3", "tag1"},
			[]string{"tag1", "tag2", "tag3"},
		},
		{
			[]string{"tag", "tag", "tag"},
			[]string{"tag"},
		},
		{
			[]string{},
			[]string{},
		},
	}

	for _, test := range testData {
		assert.Equal(t, test.output, Unique(test.slice))
	}
}
