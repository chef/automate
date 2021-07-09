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
	err := dl.DownloadHabBinary("0.90.6", "20191112141314", buf)
	require.NoError(t, err)
	assert.Equal(t, "1d4cdb2165e967e7421b671512c6588cdfeb7b218f18a594bcfaf52325f8a934", hex.EncodeToString(buf.Sum(nil)))

	buf = sha256.New()
	err = dl.DownloadHabBinary("1.6.181", "20201030172917", buf)
	require.NoError(t, err)
	assert.Equal(t, "b9b96105d341613c0fcd9c34968b9966b26ee4597d6f298d8f18300190b1f799", hex.EncodeToString(buf.Sum(nil)))
}
