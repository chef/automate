package elastic

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestCreateCaseInsensitivePatternEmpty(t *testing.T) {
	regex := createCaseInsensitivePattern("")
	assert.Equal(t, ".*.*", regex)
}

func TestCreateCaseInsensitivePatternWord(t *testing.T) {
	regex := createCaseInsensitivePattern("bob")
	assert.Equal(t, ".*[bB][oO][bB].*", regex)
}

func TestCreateCaseInsensitivePatternWordWithNonLetters(t *testing.T) {
	regex := createCaseInsensitivePattern("chef.io")
	assert.Equal(t, ".*[cC][hH][eE][fF]\\.[iI][oO].*", regex)

	regex = createCaseInsensitivePattern("chef?io")
	assert.Equal(t, ".*[cC][hH][eE][fF]\\?[iI][oO].*", regex)

	regex = createCaseInsensitivePattern("chef-io")
	assert.Equal(t, ".*[cC][hH][eE][fF]-[iI][oO].*", regex)

	regex = createCaseInsensitivePattern("chef+io")
	assert.Equal(t, ".*[cC][hH][eE][fF]\\+[iI][oO].*", regex)

	regex = createCaseInsensitivePattern("chef$io")
	assert.Equal(t, ".*[cC][hH][eE][fF]$[iI][oO].*", regex)

	regex = createCaseInsensitivePattern("chef^io")
	assert.Equal(t, ".*[cC][hH][eE][fF]^[iI][oO].*", regex)

	regex = createCaseInsensitivePattern("chef@io")
	assert.Equal(t, ".*[cC][hH][eE][fF]@[iI][oO].*", regex)

	regex = createCaseInsensitivePattern("chef#io")
	assert.Equal(t, ".*[cC][hH][eE][fF]\\#[iI][oO].*", regex)
}

func TestCreateCaseInsensitivePatternWordWithNumber(t *testing.T) {
	regex := createCaseInsensitivePattern("chef2io")
	assert.Equal(t, ".*[cC][hH][eE][fF]2[iI][oO].*", regex)

	regex = createCaseInsensitivePattern("chef3io")
	assert.Equal(t, ".*[cC][hH][eE][fF]3[iI][oO].*", regex)

	regex = createCaseInsensitivePattern("chef10io")
	assert.Equal(t, ".*[cC][hH][eE][fF]10[iI][oO].*", regex)
}
