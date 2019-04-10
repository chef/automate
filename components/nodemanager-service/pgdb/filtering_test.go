package pgdb

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestWhereFieldIn(t *testing.T) {
	// Test field validation with error
	arr := []string{}
	actual, err := whereFieldIn("'", arr, "j")
	assert.EqualError(t, err, "Unsupported character found in field: '")
	assert.Equal(t, "", actual)

	// Test with empty array of data
	actual, err = whereFieldIn("parent_id", arr, "j")
	assert.Equal(t, err, nil)
	assert.Equal(t, "j.parent_id IN ('')", actual)

	// Test with valid parent_ids
	arr = []string{"11111111-ac8a-4474-4e42-448c29c543e6", "22222222-ac8a-4474-4e42-448c29c543e6"}
	actual, err = whereFieldIn("parent_id", arr, "j")
	assert.Equal(t, nil, err)
	assert.Equal(t, "j.parent_id IN ('11111111-ac8a-4474-4e42-448c29c543e6','22222222-ac8a-4474-4e42-448c29c543e6')", actual)

	// Test with valid job types
	arr = []string{"exec"}
	actual, err = whereFieldIn("type", arr, "j")
	assert.Equal(t, nil, err)
	assert.Equal(t, "j.type IN ('exec')", actual)

	// Test with invalid job types
	arr = []string{"exec'; DELETE FROM nodes;"}
	actual, err = whereFieldIn("type", arr, "j")
	assert.Equal(t, nil, err)
	assert.Equal(t, "j.type IN ('exec''; DELETE FROM nodes;')", actual)
}
