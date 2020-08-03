package prequire

import (
	"fmt"
	"testing"

	"github.com/chef/automate/lib/pcmp"
	"github.com/google/go-cmp/cmp"
	"github.com/stretchr/testify/require"
)

func Equal(t *testing.T, expected, actual interface{}) {
	if !cmp.Equal(expected, actual, pcmp.CompareOptsForProtos...) {
		diff := cmp.Diff(expected, actual, pcmp.CompareOptsForProtos...)
		expected, actual = pcmp.FormatUnequalValues(expected, actual)
		require.Fail(t, fmt.Sprintf("Not equal: \n"+
			"expected: %s\n"+
			"actual  : %s%s", expected, actual, diff))
	}
}
