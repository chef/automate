package gatherlogs

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
)

func TestNameFromTime(t *testing.T) {
	someTime, _ := time.Parse(time.RFC3339, "2018-02-05T23:20:12Z")

	assert.Equal(t, "20180205T232012Z", nameFromTime(someTime))
}

func TestNewGatherer(t *testing.T) {
	assert := assert.New(t)

	stagingDir := "/data"
	tempDir := stagingDir + "/bundle"
	binPaths := map[string]string{}
	someTime, _ := time.Parse(time.RFC3339, "2018-02-05T23:20:12Z")

	g := NewGatherer(stagingDir, tempDir, "example.com", binPaths, someTime)

	assert.Equal(
		"/data/bundle",
		g.archiveRoot,
		"The directory where we build the archive tree is incorrect.",
	)
	assert.Equal(
		"example.com-20180205T232012Z.tar.gz",
		g.bundleFileName,
		"Unexpected archive file name.",
	)
	assert.Equal(
		"/data/example.com-20180205T232012Z.tar.gz",
		g.bundleFilePath,
		"Full path to the archive file looks wrong.",
	)
	assert.Equal(
		"/data/bundle/example.com/20180205T232012Z",
		g.targetDir,
		"Path to where we dump collected data looks wrong.",
	)
}
