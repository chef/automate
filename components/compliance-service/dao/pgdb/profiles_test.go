package pgdb

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestParseNamespaceAndNameFromUrl(t *testing.T) {
	var namespace, name, url string
	var err error

	url = "compliance://mario/linux-patch-baseline#0.3.0"
	namespace, name, err = parseNamespaceAndNameFromUrl(url)
	assert.NoError(t, err)
	assert.Equal(t, "mario", namespace)
	assert.Equal(t, "linux-patch-baseline", name)

	url = "https://github.com/dev-sec/apache-baseline"
	namespace, name, err = parseNamespaceAndNameFromUrl(url)
	assert.NoError(t, err)
	assert.Equal(t, "dev-sec", namespace)
	assert.Equal(t, "apache-baseline", name)

	url = "https://github.com/dev-sec/linux-baseline/archive/master.tar.gz"
	namespace, name, err = parseNamespaceAndNameFromUrl(url)
	assert.NoError(t, err)
	assert.Equal(t, "dev-sec", namespace)
	assert.Equal(t, "linux-baseline", name)

	url = ""
	namespace, name, err = parseNamespaceAndNameFromUrl(url)
	assert.Error(t, err)
	assert.Equal(t, "", namespace)
	assert.Equal(t, "", name)
}
