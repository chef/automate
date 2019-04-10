package relaxting

import (
	"testing"

	"sort"

	"github.com/stretchr/testify/assert"
)

func TestMapKeys(t *testing.T) {
	// Test with empty map
	var map1 map[string]string
	assert.Equal(t, []string{}, MapKeys(map1))

	map1 = map[string]string{
		"key1": "value1",
		"key2": "value2",
	}
	keys := MapKeys(map1)
	sort.Strings(keys)
	assert.Equal(t, []string{"key1", "key2"}, keys)
}

func TestMapValues(t *testing.T) {
	// Test with empty map
	var map1 map[string]string
	assert.Equal(t, []string{}, MapValues(map1))

	map1 = map[string]string{
		"key1": "value1",
		"key2": "value2",
	}
	vals := MapValues(map1)
	sort.Strings(vals)
	assert.Equal(t, []string{"value1", "value2"}, vals)
}

func TestFirstOrEmpty(t *testing.T) {
	assert.Equal(t, "", firstOrEmpty([]string{}))
	assert.Equal(t, "item1", firstOrEmpty([]string{"item1", "item2"}))
}

func TestRightSplit(t *testing.T) {
	left, right := rightSplit("something", "|")
	assert.Equal(t, "something", left)
	assert.Equal(t, "", right)

	left, right = rightSplit("some|thi|ng", "|")
	assert.Equal(t, "some|thi", left)
	assert.Equal(t, "ng", right)
}

func TestRemove(t *testing.T) {
	arr := []string{"item1", "item2"}
	Remove(&arr, 1)
	assert.Equal(t, []string{"item1"}, arr)

	Remove(&arr, 1)
	assert.Equal(t, []string{"item1"}, arr)

	arr = []string{"item1", "item2", "item3"}
	Remove(&arr, 1)
	assert.Equal(t, []string{"item1", "item3"}, arr)

	Remove(&arr, 0)
	assert.Equal(t, []string{"item3"}, arr)
}

func TestDeDupSlice(t *testing.T) {
	arr := []string{"item1", "item2", "item2"}
	arr = deDupSlice(arr)
	assert.Equal(t, []string{"item1", "item2"}, arr)

	arr = []string{"item1"}
	arr = deDupSlice(arr)
	assert.Equal(t, []string{"item1"}, arr)

	arr = []string{}
	arr = deDupSlice(arr)
	assert.Equal(t, []string{}, arr)

	arr = []string{"item1", "item1", "item1", "item3", "item2", "item2"}
	arr = deDupSlice(arr)
	assert.Equal(t, []string{"item1", "item3", "item2"}, arr)
}

func TestDeDupFilters(t *testing.T) {
	filters := make(map[string][]string)
	filters["profile_id"] = []string{"prof1", "prof2"}
	deDupFilters(filters)
	assert.Equal(t, []string{"prof1", "prof2"}, filters["profile_id"])

	filters["profile_id"] = []string{"prof1", "prof2", "prof2"}
	deDupFilters(filters)
	assert.Equal(t, []string{"prof1", "prof2"}, filters["profile_id"])

	filters["profile_id"] = []string{"prof1", "prof2", "prof2", "prof1", "prof11"}
	deDupFilters(filters)
	assert.Equal(t, []string{"prof1", "prof2", "prof11"}, filters["profile_id"])
}
