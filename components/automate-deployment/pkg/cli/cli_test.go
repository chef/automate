package cli

import (
	"bytes"
	"fmt"
	"testing"

	"github.com/fatih/color"
	"github.com/pkg/errors"
	"github.com/stretchr/testify/assert"
)

var writeBuffer *bytes.Buffer
var readBuffer *bytes.Buffer
var errorBuffer *bytes.Buffer

func enterInput(userInput string) {
	readBuffer = bytes.NewBufferString(fmt.Sprintf("%s\n", userInput))
}

func resetWriter() FormatWriter {
	writeBuffer = new(bytes.Buffer)
	errorBuffer = new(bytes.Buffer)
	return NewWriter(writeBuffer, errorBuffer, readBuffer)
}

func output() string {
	return writeBuffer.String()
}

func errorOutput() string {
	return errorBuffer.String()
}

func TestPrintMethods(t *testing.T) {
	color.NoColor = false

	hello := "hello\nworld\n"

	w := resetWriter()
	w.Print("hello\nworld\n")
	assert.Equal(t, hello, output())

	w = resetWriter()
	w.Printf("hello\n%s\n", "world")
	assert.Equal(t, hello, output())

	w = resetWriter()
	w.Println("hello\nworld")
	assert.Equal(t, hello, output())

	w = resetWriter()
	w.Error("hello\nworld\n")
	assert.Equal(t, hello, errorOutput())

	w = resetWriter()
	w.Errorf("hello\n%s\n", "world")
	assert.Equal(t, hello, errorOutput())

	w = resetWriter()
	w.Errorln("hello\nworld")
	assert.Equal(t, hello, errorOutput())

	w = resetWriter()
	w.Title("title")
	assert.Equal(t, "\ntitle\n", output())

	w = resetWriter()
	w.Titlef("%s", "title")
	assert.Equal(t, "\ntitle\n", output())

	w = resetWriter()
	w.Body("body")
	assert.Equal(t, "  body\n", output())

	w = resetWriter()
	w.Bodyf("%s", "body")
	assert.Equal(t, "  body\n", output())

	w = resetWriter()
	w.Success("yay")
	assert.Equal(t, "\x1b[32mSuccess: \x1b[0myay\n", output())

	w = resetWriter()
	w.Successf("test %s", "yay")
	assert.Equal(t, "\x1b[32mSuccess: \x1b[0mtest yay\n", output())

	w = resetWriter()
	w.Skipped("yay")
	assert.Equal(t, "Skipped: yay\n", output())

	w = resetWriter()
	w.Skippedf("test %s", "yay")
	assert.Equal(t, "Skipped: test yay\n", output())

	w = resetWriter()
	w.Fail("oops")
	assert.Equal(t, "\x1b[31mError: \x1b[0moops\n", errorOutput())

	w = resetWriter()
	w.Failf("%s", "oops")
	assert.Equal(t, "\x1b[31mError: \x1b[0moops\n", errorOutput())

	e := fmt.Errorf("sad trombone")

	w = resetWriter()
	w.FailError(e)
	assert.Equal(t, "\x1b[31mError: \x1b[0msad trombone\n", errorOutput())

	w = resetWriter()
	w.FailWrap(e, "whoops")
	// this might be (too) sensitive to a change in how wrapped errors convert to string?
	assert.Equal(t, "\x1b[31mError: \x1b[0mwhoops: sad trombone\n", errorOutput())

	expected := "\x1b[31mError: \x1b[0mwah wah: sad trombone\n\x1b[31mCause: \x1b[0msad trombone\n"

	w = resetWriter()
	nested := errors.Wrap(e, "wah wah")
	w.FailCause(nested)
	assert.Equal(t, expected, errorOutput())

	w = resetWriter()
	w.FailWrapCause(e, "wah wah")
	assert.Equal(t, expected, errorOutput())

	w = resetWriter()
	w.HR()
	expHR := "--------------------------------------------------------------------------------\n"
	assert.Equal(t, expHR, output())

	w = resetWriter()
	w.ErrorHeader("dangit")
	expMsg := `--------------------------------------------------------------------------------
ERROR: dangit
--------------------------------------------------------------------------------

`
	assert.Equal(t, expMsg, errorOutput())
}

func TestConfirmationDialog(t *testing.T) {
	validYes := []string{"y", "Y", "yes", "Yes", "YES", "yaas"}
	validNo := []string{"n", "N", "no", "No", "NO", "nope", "nah"}
	ambiguous := []string{"yippee", "yup", "ya", "yah", "yankee", "november", "naptime", "pokemon", "spongebob"}

	for _, userResponse := range validYes {
		enterInput(userResponse)
		w := resetWriter()
		result, err := w.Confirm("should we do the thing?")
		assert.Equal(t, "should we do the thing? (y/n)\n", output())
		assert.Nil(t, err)
		assert.True(t, result, "expected true for input %s, got %v", userResponse, result)
	}

	for _, userResponse := range validNo {
		enterInput(userResponse)
		w := resetWriter()
		result, err := w.Confirm("should we do the thing?")
		assert.Equal(t, "should we do the thing? (y/n)\n", output())
		assert.Nil(t, err)
		assert.False(t, result, "expected false for input %s, got %v", userResponse, result)
	}

	for _, userResponse := range ambiguous {
		enterInput(userResponse + "\nyes\n")
		w := resetWriter()

		_, err := w.Confirm("should we do the thing?")

		expected := "should we do the thing? (y/n)\n" +
			fmt.Sprintf("I don't understand '%s'. Please type 'y' or 'n'.", userResponse) +
			"\nshould we do the thing? (y/n)\n"
		assert.Equal(t, expected, output())
		assert.Nil(t, err)
	}

}
