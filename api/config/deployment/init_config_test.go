package deployment

import (
	"os"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGenerateProxySettings(t *testing.T) {
	t.Run("it trims whitespace from no_proxy entries", func(t *testing.T) {
		oldNoProxy := os.Getenv("no_proxy")
		oldHTTPProxy := os.Getenv("http_proxy")
		defer func() {
			os.Setenv("no_proxy", oldNoProxy)
			os.Setenv("http_proxy", oldHTTPProxy)
		}()
		os.Setenv("no_proxy", "domain1.local, domain2.local  ,*.cool")
		os.Setenv("http_proxy", "http://some.proxy.local")

		c := InitConfig{}
		err := generateProxySettings(&c)
		assert.NoError(t, err)
		assert.Equal(t, []string{"domain1.local", "domain2.local", "*.cool",
			"127.0.0.1/8", "127.0.0.1", "localhost", "::1/128", "::1", "localhost6", "0.0.0.0", "::0"}, c.NoProxy)
	})
}
