package certgenerateutils

import (
	"testing"

	"github.com/bmizerany/assert"
	"github.com/chef/automate/lib/io/fileutils"
)

func TestMakeRequest(t *testing.T) {
	err := GenerateCert("54.169.203.144")
	fileutils.DeleteFile("certificate.pem")
	fileutils.DeleteFile("private_key.pem")
	assert.Equal(t, nil, err)
}
