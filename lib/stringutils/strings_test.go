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
	assert.Equal(t, "Hello Chef", stringutils.TitleSplit("hello chef", "_"))

	assert.Equal(t, "Hello-World", stringutils.TitleSplit("hello-world", " "))
	assert.Equal(t, "Foo Bar", stringutils.TitleSplit("foo-bar", "-"))
	assert.Equal(t, "Foo", stringutils.TitleSplit("foo", "-"))
}

func TestTitleReplace(t *testing.T) {
	assert.Equal(t, "Hello World", stringutils.TitleReplace("hello_world", "_", " "))
	assert.Equal(t, "Hello-World", stringutils.TitleReplace("hello_world", "_", "-"))
	assert.Equal(t, "Helloworld", stringutils.TitleReplace("helloworld", "_", "-"))
	assert.Equal(t, "Helloworld", stringutils.TitleReplace("helloworld", "-", " "))
	assert.Equal(t, "Hello Chef", stringutils.TitleReplace("hello chef", "_", "-"))
}

func TestGetLastLine(t *testing.T) {
	// Test case 1: Empty input string
	input := ""
	expected := ""
	actual := stringutils.GetLastLine(input)
	assert.Equal(t, actual, expected)

	// Test case 2: Single line input
	input = "Hello, World!"
	expected = "Hello, World!"
	actual = stringutils.GetLastLine(input)
	assert.Equal(t, actual, expected)

	// Test case 3: Multiple lines input
	input = "Line 1\nLine 2\nLine 3\nLast line"
	expected = "Last line"
	actual = stringutils.GetLastLine(input)
	assert.Equal(t, actual, expected)
}
