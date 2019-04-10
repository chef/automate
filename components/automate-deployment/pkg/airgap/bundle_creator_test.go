package airgap

import (
	"crypto/sha256"
	"encoding/hex"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestHabBinaryDownload(t *testing.T) {
	dl := bintrayHabDownloader{}
	buf := sha256.New()
	err := dl.DownloadHabBinary("0.56.0", "20180530234036", buf)
	require.NoError(t, err)
	assert.Equal(t, "d829ee385a7ca3cca980f5c52671643df2ee4f80104f2336884d3c565f9a2e13", hex.EncodeToString(buf.Sum(nil)))
}
