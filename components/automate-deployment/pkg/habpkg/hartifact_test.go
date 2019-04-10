// Copyright © 2017 Chef Software

package habpkg

import (
	"encoding/json"
	"io/ioutil"
	"math/rand"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestFindHartReturnsEmptyString(t *testing.T) {
	path, _ := filepath.Abs("../../testdata/hartifactdir/")
	dir := NewHartDir(path)
	hart, err := dir.FindHart("dne", "dne")
	assert.NoError(t, err)
	assert.Nil(t, hart)
}

func TestFindHartReturnsPath(t *testing.T) {
	path, _ := filepath.Abs("../../testdata/hartifactdir/")
	dir := NewHartDir(path)
	hart, err := dir.FindHart("ssd", "local-user-service")
	require.NoError(t, err)
	assert.Equal(t, "ssd-local-user-service-0.1.0-20180202161424-x86_64-linux.hart", filepath.Base(hart.Path()))
}

func TestFindHartCanHandleNamesThatSharePrefix(t *testing.T) {
	path, _ := filepath.Abs("../../testdata/hartifactdir/")
	dir := NewHartDir(path)

	hart, err := dir.FindHart("chef", "some-service")
	require.NoError(t, err)
	assert.Nil(t, hart)

	hart, err = dir.FindHart("chef", "some-service-helper")
	require.NoError(t, err)
	assert.Equal(t, "chef-some-service-helper-0.1.0-20180202161424-x86_64-linux.hart", filepath.Base(hart.Path()))
}

func TestFindHartReturnsErrorWhenPathIncludesInvalidName(t *testing.T) {
	path, _ := filepath.Abs("../../testdata/hartifactdir/")
	badFileName := strings.Join([]string{path, "ssd-deployment-service-0not-a-valid-hart-name.hart"}, "/")
	ioutil.WriteFile(badFileName, []byte{}, 0600)
	defer os.Remove(badFileName)

	dir := NewHartDir(path)
	_, err := dir.FindHart("ssd", "deployment-service")
	assert.Error(t, err)
}

func TestFindHartReturnsErrorIfABadGlobIsGenerated(t *testing.T) {
	path, _ := filepath.Abs("../../testdata/hartifactdir/")
	dir := NewHartDir(path)
	_, err := dir.FindHart("[-]", "deployment-service")
	assert.Error(t, err)
}

func TestHartFromPathSuccess(t *testing.T) {
	h, err := HartFromPath("/some/path/ssd-deployment-service-0.1.0-20180119115432-x86_64-linux.hart")
	assert.NoError(t, err)
	assert.Equal(t, 0, h.major)
	assert.Equal(t, 1, h.minor)
	assert.Equal(t, 0, h.patch)
	assert.Equal(t, 20180119115432, h.Timestamp())
}

func TestHartFromPathFailure(t *testing.T) {
	h, err := HartFromPath("/some/path/ssd-deployment-service-0.1.0-20180119115432-x86_64-linux.blob")
	assert.Equal(t, Hart{}, h)
	assert.Error(t, err)

	h, err = HartFromPath("/some/path/ssd-deployment-service-0.1.0-20180119115432.hart")
	assert.Equal(t, Hart{}, h)
	assert.Error(t, err)

	h, err = HartFromPath("/some/path/ssd-deployment-service-0.1.0-x86_64-linux.hart")
	assert.Equal(t, Hart{}, h)
	assert.Error(t, err)

}

func TestHartStringMarshalUnmarshal(t *testing.T) {
	h, err := HartFromPath("/some/path/ssd-deployment-service-0.1.0-20180119115432-x86_64-linux.hart")
	require.NoError(t, err)
	h.origin = "ssd"
	h.name = "deployment-service"

	text, err := h.MarshalText()
	require.NoError(t, err)

	unmarshalled := Hart{}
	err = unmarshalled.UnmarshalText(text)
	require.NoError(t, err)

	assert.Equal(t, "ssd", unmarshalled.origin)
	assert.Equal(t, "deployment-service", unmarshalled.name)
	assert.Equal(t, 0, unmarshalled.major)
	assert.Equal(t, 1, unmarshalled.minor)
	assert.Equal(t, 0, unmarshalled.patch)
	assert.Equal(t, 20180119115432, unmarshalled.Timestamp())
}

func TestHartJsonMarshalUnmarshal(t *testing.T) {
	h, err := HartFromPath("/some/path/ssd-deployment-service-0.1.0-20180119115432-x86_64-linux.hart")
	require.NoError(t, err)
	h.origin = "ssd"
	h.name = "deployment-service"

	text, err := json.Marshal(&h)
	require.NoError(t, err)

	unmarshalled := Hart{}
	err = json.Unmarshal(text, &unmarshalled)
	require.NoError(t, err)

	assert.Equal(t, "ssd", unmarshalled.origin)
	assert.Equal(t, "deployment-service", unmarshalled.name)
	assert.Equal(t, 0, unmarshalled.major)
	assert.Equal(t, 1, unmarshalled.minor)
	assert.Equal(t, 0, unmarshalled.patch)
	assert.Equal(t, 20180119115432, unmarshalled.Timestamp())
}

func TestSortableHartsFromPaths(t *testing.T) {
	paths := []string{
		"/some/path/ssd-deployment-service-0.1.0-20180119115432-x86_64-linux.hart",
		"/some/path/ssd-deployment-service-0.2.2-20180119115432-x86_64-linux.hart",
		"/some/path/ssd-deployment-service-0.2.0-20180119115432-x86_64-linux.hart",
		"/some/path/ssd-deployment-service-0.2.2-20180119115437-x86_64-linux.hart",
		"/some/path/ssd-deployment-service-0.2.1-20180119115432-x86_64-linux.hart",
		"/some/path/ssd-deployment-service-1.0.0-20180119115432-x86_64-linux.hart",
	}

	for i := 0; i < 10; i++ {
		sortable, err := sortableHartsFromPaths("ssd", "deployment-service", shufflePaths(paths))
		assert.NoError(t, err)
		sort.Sort(sort.Reverse(sortable))
		assert.Equal(t,
			"/some/path/ssd-deployment-service-1.0.0-20180119115432-x86_64-linux.hart",
			sortable[0].Path())
	}
}

func shufflePaths(paths []string) []string {
	ret := make([]string, len(paths))
	perms := rand.Perm(len(paths))
	for i, v := range perms {
		ret[v] = paths[i]
	}

	return ret
}
