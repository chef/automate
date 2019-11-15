package airgap

import (
	"crypto/sha256"
	"encoding/hex"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestHabBinaryDownload(t *testing.T) {
	dl := netHabDownloader{}
	buf := sha256.New()
	err := dl.DownloadHabBinary("0.56.0", "20180530234036", buf)
	require.NoError(t, err)
	assert.Equal(t, "d829ee385a7ca3cca980f5c52671643df2ee4f80104f2336884d3c565f9a2e13", hex.EncodeToString(buf.Sum(nil)))

	buf = sha256.New()
	err = dl.DownloadHabBinary("0.90.6", "20191112141314", buf)
	require.NoError(t, err)
	assert.Equal(t, "1d4cdb2165e967e7421b671512c6588cdfeb7b218f18a594bcfaf52325f8a934", hex.EncodeToString(buf.Sum(nil)))
}
