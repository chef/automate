package stringutils_test

import (
	"testing"

	"github.com/chef/automate/lib/stringutils"
	"github.com/stretchr/testify/assert"
)

func TestGetFileName(t *testing.T) {
	assert.Equal(t, "report.toml", stringutils.GetFileName("/usr/a2ra/report.toml"))
}
