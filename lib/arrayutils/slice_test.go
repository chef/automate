package arrayutils_test

import (
	"testing"

	"github.com/bmizerany/assert"
	"github.com/chef/automate/lib/arrayutils"
)

func TestGetPortsForMockServer(t *testing.T) {
	firstList := []int{1, 2, 3}
	secondList := []int{2, 3, 4}

	diff := arrayutils.SliceDifference(firstList, secondList)
	assert.Equal(t, 1, diff[0])
}
