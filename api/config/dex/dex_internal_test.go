package dex

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestReplaceAttrNames(t *testing.T) {
	assert.Equal(t, "neverSeenThisComing", fix("neverSeenThisComing"))

	assert.Equal(t, "sAMAccountName", fix("samAccountName"))
	assert.Equal(t, "cn", fix("CN"))
	assert.Equal(t, "DN", fix("dn"))
	assert.Equal(t, "mail", fix("Mail"))
	assert.Equal(t, "uid", fix("uID"))
	assert.Equal(t, "gidNumber", fix("gidnumber"))
	assert.Equal(t, "memberOf", fix("memberof"))
}
