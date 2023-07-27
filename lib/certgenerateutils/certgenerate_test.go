package certgenerateutils

import (
	"testing"

	"github.com/bmizerany/assert"
	"github.com/chef/automate/lib/io/fileutils"
	//"github.com/chef/automate/lib/io/fileutils"
)

func TestMakeRequest(t *testing.T) {
	privateKeyFilePath, publicKeyFilePath, err := GenerateCert("54.169.203.144")
	fileutils.DeleteFile(privateKeyFilePath)
	fileutils.DeleteFile(publicKeyFilePath)
	assert.Equal(t, nil, err)
}
