package assets

import (
	"io/ioutil"
	"testing"

	"github.com/pmezard/go-difflib/difflib"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// This checks that the bindata compiled version of assets.bindata.go matches
// the files that are currently in data/* so that we don't accidentally
// forget to generate them.
// If this test fails `make generate` should get you fixed.
func TestBindataKeys(t *testing.T) {
	assert.True(t, len(AssetNames()) > 0, "expecting asset files")

	for _, asset := range AssetNames() {
		compiled := MustAsset(asset)

		onDisk, err := ioutil.ReadFile(asset)
		require.NoErrorf(t, err, "read asset %q from disk", asset)

		diff := difflib.UnifiedDiff{
			A:        difflib.SplitLines(string(onDisk)),
			B:        difflib.SplitLines(string(compiled)),
			FromFile: asset,
			FromDate: "on disk",
			ToFile:   asset,
			ToDate:   "generated",
			Context:  3,
		}
		text, err := difflib.GetUnifiedDiffString(diff)
		require.NoError(t, err, "error generating diff")
		if text != "" {
			t.Error("expected no difference, got diff:\n" + text)
		}
	}
}
