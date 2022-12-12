package stringutils_test

import (
	"testing"

	"github.com/chef/automate/lib/stringutils"
	"github.com/stretchr/testify/assert"
)

func TestTitle(t *testing.T) {
	assert.Equal(t, "Hello", stringutils.Title("hello"))
	assert.NotEqual(t, "hello", stringutils.Title("hello"))
	assert.Equal(t, "Hello", stringutils.Title("Hello"))
	assert.NotEqual(t, "HELLO", stringutils.Title("HELLO"))
	assert.Equal(t, "Foo Bar", stringutils.Title("foo bar"))
	assert.Equal(t, "Hello World", stringutils.Title("heLLo worLd"))
}

func TestTitleSplit(t *testing.T) {
	assert.Equal(t, "Hello World", stringutils.TitleSplit("hello_world", "_"))
	assert.Equal(t, "Helloworld", stringutils.TitleSplit("helloworld", "-"))
	assert.Equal(t, "Hello World", stringutils.TitleSplit("hello world", "_"))

	assert.Equal(t, "Hello-World", stringutils.TitleSplit("hello-world", " "))
	assert.Equal(t, "Foo Bar", stringutils.TitleSplit("foo-bar", "-"))
	assert.Equal(t, "Foo", stringutils.TitleSplit("foo", "-"))
}
