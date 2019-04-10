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

//todo - get rid of this.
//func TestGetEsIndex(t *testing.T) {
//	var filters map[string][]string
//	assert.Equal(t, CompSumLatestIndexAccumulated, GetEsIndex(filters))
//
//	filters = map[string][]string{
//		"job_id": []string{"12345-6789"},
//	}
//	assert.Equal(t, true, strings.HasPrefix(GetEsIndex(filters), fmt.Sprintf("%s2017", CompDailySumIndexPrefix)))
//
//	filters = map[string][]string{
//		"end_time": []string{time.Now().UTC().Format(time.RFC3339)},
//	}
//	assert.Equal(t, CompSumLatestIndexAccumulated, GetEsIndex(filters))
//
//	filters = map[string][]string{
//		"end_time": []string{"2017-02-02T09:18:41Z"},
//	}
//	expectedEsIndex := fmt.Sprintf(fmt.Sprintf("%[1]s2017.01*,%[1]s2017.02.01*,%[1]s2017.02.02*",
//		CompDailySumIndexPrefix))
//	actualEsIndex := GetEsIndex(filters)
//	assert.Equal(t, expectedEsIndex, actualEsIndex)
//}
