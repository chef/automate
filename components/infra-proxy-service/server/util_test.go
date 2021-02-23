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
	assert.Equal(t, []string{"tag2", "tag3"}, SubtractSlice([]string{"tag1", "tag2", "tag3"}, []string{"tag1"}))
	assert.Equal(t, []string{"tag3"}, SubtractSlice([]string{"tag1", "tag2", "tag3"}, []string{"tag1", "tag2"}))
	assert.Equal(t, []string{"tag1", "tag2", "tag3"}, SubtractSlice([]string{"tag1", "tag2", "tag3"}, []string{"unknown"}))
	assert.Equal(t, []string{"tag1", "tag2"}, SubtractSlice([]string{"tag1", "tag2", "tag3"}, []string{"tag3", "unknown"}))
	assert.Equal(t, []string{"tag1", "tag2"}, SubtractSlice([]string{"tag1", "tag2"}, []string{}))
	assert.Equal(t, []string{}, SubtractSlice([]string{}, []string{"tag1"}))
	assert.Equal(t, []string{}, SubtractSlice([]string{"tag1", "tag2"}, []string{"tag1", "tag2"}))
	assert.Equal(t, []string{}, SubtractSlice([]string{}, []string{}))
}

func TestRemoveElement(t *testing.T) {
	testData1 := []string{"tag1", "tag2", "tag3"}
	assert.Equal(t, []string{"tag1", "tag2"}, RemoveElement(testData1, "tag3"))

	testData2 := []string{"tag1", "tag2", "tag3"}
	assert.Equal(t, testData2, RemoveElement(testData2, "unknown"))

	testData3 := []string{"tag1", "tag2", "tag3"}
	assert.Equal(t, testData3, RemoveElement(testData3, ""))

	testData4 := []string{}
	assert.Equal(t, testData4, RemoveElement(testData4, "tag1"))

	testData5 := []string{}
	assert.Equal(t, testData5, RemoveElement(testData5, ""))
}

func TestUnique(t *testing.T) {
	testData1 := []string{"tag1", "tag2", "tag3"}
	assert.Equal(t, testData1, Unique(testData1))

	testData2 := []string{"tag1", "tag1", "tag2", "tag3"}
	assert.Equal(t, []string{"tag1", "tag2", "tag3"}, Unique(testData2))

	testData3 := []string{"tag1", "tag2", "tag3", "tag1"}
	assert.Equal(t, []string{"tag1", "tag2", "tag3"}, Unique(testData3))

	testData4 := []string{"tag", "tag", "tag"}
	assert.Equal(t, []string{"tag"}, Unique(testData4))

	testData5 := []string{}
	assert.Equal(t, []string{}, Unique(testData5))
}
