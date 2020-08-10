package passert

import (
	"fmt"
	"testing"

	"github.com/chef/automate/lib/pcmp"
	"github.com/google/go-cmp/cmp"
	"github.com/google/go-cmp/cmp/cmpopts"
	"github.com/stretchr/testify/assert"
)

func Equal(t *testing.T, expected, actual interface{}, msgAndArgs ...interface{}) bool {
	if !cmp.Equal(expected, actual, pcmp.CompareOptsForProtos...) {
		diff := cmp.Diff(expected, actual, pcmp.CompareOptsForProtos...)
		expected, actual = pcmp.FormatUnequalValues(expected, actual)
		return assert.Fail(t, fmt.Sprintf("Not equal: \n"+
			"expected: %s\n"+
			"actual  : %s%s", expected, actual, diff), msgAndArgs...)
	}
	return true
}

// Approximates the behavior of `assert.ElementsMatch(...)` by feeding them to
// the Equal comparison with a sorting option. The sorter is just a naive
// string comparison at present.
func ElementsMatch(t *testing.T, expected, actual interface{}, msgAndArgs ...interface{}) bool {
	naiveSorter := cmpopts.SortSlices(func(x, y fmt.Stringer) bool {
		return x.String() > y.String()
	})
	compareOpts := append(pcmp.CompareOptsForProtos, naiveSorter)

	if !cmp.Equal(expected, actual, compareOpts...) {
		diff := cmp.Diff(expected, actual, pcmp.CompareOptsForProtos...)
		expected, actual = pcmp.FormatUnequalValues(expected, actual)
		return assert.Fail(t, fmt.Sprintf("Not equal: \n"+
			"expected: %s\n"+
			"actual  : %s%s", expected, actual, diff), msgAndArgs...)
	}
	return true
}
