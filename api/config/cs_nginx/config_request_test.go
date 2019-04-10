package cs_nginx

import (
	"testing"

	"github.com/stretchr/testify/assert"

	shared "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

func TestValidateConfigRequestValid(t *testing.T) {
	c := NewConfigRequest()
	err := c.Validate()
	assert.Nil(t, err)
}

func TestValidateConfigRequestMissingRequiredRecipePath(t *testing.T) {
	c := NewConfigRequest()
	c.V1.Sys.RequiredRecipe.Enabled = w.Bool(true)
	expected := shared.NewInvalidConfigError()
	expected.AddMissingKey("cs_nginx.v1.sys.required_recipe.path")
	err := c.Validate()
	assert.Equal(t, expected, err)
}

func TestCalculateContentMD5(t *testing.T) {
	tests := map[string]struct {
		content     []byte
		expectedMD5 string
	}{
		// Expected hashes should be calculated with the
		// following ruby:
		//
		//     Digest::MD5.base64digest(STRING)
		"empty string": {[]byte{}, "1B2M2Y8AsgTpgAmY7PhCfg=="},
		"multi-line":   {[]byte("foo\nbar\n"), "9Hx1YUCHqN2Ti6Ss/yUklA=="},
	}

	for name, tt := range tests {
		t.Run(name, func(t *testing.T) {
			res := CalculateContentMD5(tt.content)
			assert.Equal(t, tt.expectedMD5, res)
		})
	}
}
