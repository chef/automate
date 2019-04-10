package cli

import (
	"bufio"
	"fmt"
	"io"
	"strings"
	"time"

	"github.com/briandowns/spinner"
	"github.com/fatih/color"
	"github.com/pkg/errors"
)

// BodyWriter is an interface for printers that can print indented
// sub-steps of a process.
type BodyWriter interface {
	Body(string)
	Bodyf(string, ...interface{})
}

// TitleWriter is an interface for printers that can print indented
// sub-steps of a process.
type TitleWriter interface {
	Title(string)
	Titlef(string, ...interface{})
}

// ErrorWriter is an interface for printers that print to a writer
// that the user expects errors on, such as stderr.
type ErrorWriter interface {
	Error(string)
	Errorf(string, ...interface{})
	Errorln(string)
}

// WarnWriter is an interface for printers that can printed warnings
type WarnWriter interface {
	Warn(string)
	Warnf(string, ...interface{})
	Warnln(string)
	WarnError(error)
	LongWarningln(string)
}

// FormatWriter is a writer interface that includes formatting helpers
type FormatWriter interface {
	// Writers
	Print(string)
	Printf(string, ...interface{})
	Println(string)
	Success(string)
	Successf(string, ...interface{})
	Skipped(string)
	Skippedf(string, ...interface{})
	Fail(string)
	Failf(string, ...interface{})
	FailError(error)
	FailWrap(error, string)
	FailCause(error)
	FailWrapCause(error, string)
	Failt(string, string)
	Failtf(string, string, ...interface{})

	BodyWriter
	TitleWriter
	ErrorWriter
	WarnWriter

	HR()
	ErrorHeader(string)

	Confirm(string) (bool, error)
	Prompt(string) (string, error)

	// Implement a buffered version of the io.Writer interface
	Writer() io.Writer
	BufferWriter() *bufio.Writer

	// Spinner
	StartSpinner()
	StopSpinner()
}

const hr = `--------------------------------------------------------------------------------`

// Writer is the default implementation of of the cli FormatWriter interface
type Writer struct {
	errorStream  io.Writer
	writerStream io.Writer
	readerStream io.Reader
	spinner      *spinner.Spinner
	readBuffer   *bufio.Reader
	writerBuffer *bufio.Writer
	errorBuffer  *bufio.Writer
}

// NewWriter takes and io.Writer and returns a pointer to an instance of cli.Writer.
func NewWriter(w io.Writer, e io.Writer, r io.Reader) *Writer {
	return &Writer{writerStream: w, errorStream: e, readerStream: r}
}

// Print writes the message to the writer.
func (w *Writer) Print(m string) {
	w.write(m)
}

// Printf formats the message with the format string and writes it to the writer.
func (w *Writer) Printf(m string, f ...interface{}) {
	w.write(fmt.Sprintf(m, f...))
}

// Println writes the message to the writer with a newline
func (w *Writer) Println(m string) {
	w.Printf("%s\n", m)
}

// Error writes the message to the error writer..
func (w *Writer) Error(m string) {
	w.error(m)
}

// Errorf formats the message with the format string and writes it to the error writer.
func (w *Writer) Errorf(m string, f ...interface{}) {
	w.error(fmt.Sprintf(m, f...))
}

// Errorln writes the message to the error writer with a newline
func (w *Writer) Errorln(m string) {
	w.Errorf("%s\n", m)
}

// Title outputs an un-indented line of text newlines before and
// after.
func (w *Writer) Title(text string) {
	text = fmt.Sprintf("\n%s\n", text)
	w.write(text)
}

// Titlef formats the provided string and then calls Title
func (w *Writer) Titlef(text string, a ...interface{}) {
	text = fmt.Sprintf(text, a...)
	w.Title(text)
}

// The following two functions implement the deployment.BodyWriter
// which is helpful for being able to use this everywhere.

// Body outputs an indented line of text
func (w *Writer) Body(text string) {
	w.Printf("  %s\n", text)
}

// Bodyf formats the provided string and then calls Body
func (w *Writer) Bodyf(text string, a ...interface{}) {
	w.Printf("  %s\n", fmt.Sprintf(text, a...))
}

// Success formats the message to a Success message and writes it to the writer.
func (w *Writer) Success(m string) {
	w.write(color.New(color.FgGreen).Sprint("Success: "))
	w.Println(m)
}

// Successf formats the message using the format string and writes it to the
// writer as a Success message.
func (w *Writer) Successf(m string, f ...interface{}) {
	w.Success(fmt.Sprintf(m, f...))
}

// Skipped formats the message to a Skipped message and writes it to the writer.
func (w *Writer) Skipped(m string) {
	w.write(fmt.Sprint("Skipped: "))
	w.Println(m)
}

// Skippedf formats the message using the format string and writes it to the
// writer as a Skipped message.
func (w *Writer) Skippedf(m string, f ...interface{}) {
	w.Skipped(fmt.Sprintf(m, f...))
}

// Warn formats the message as a warning and writes it to the writer.
func (w *Writer) Warn(m string) {
	w.error(color.New(color.FgYellow).Sprint("Warn: "))
	w.Errorf("%s\n", strings.Replace(m, "\n", "\n      ", -1))
}

// Warnf formats the message with the format string and writes it to the writer
// as a Warning message.
func (w *Writer) Warnf(m string, f ...interface{}) {
	w.Warn(fmt.Sprintf(m, f...))
}

// Warnln formats the messages as a warning and writes it to the
// writer, appending a newline.
func (w *Writer) Warnln(m string) {
	w.Warnf("%s\n", m)
}

// WarnError formats the error's message as a Warning message and writes it to the
// writer.
func (w *Writer) WarnError(e error) {
	w.Warn(e.Error())
}

// LongWarningln colors the output as a warning but does not append any text
func (w *Writer) LongWarningln(m string) {
	w.Println(color.New(color.FgYellow).Sprint(m))
}

// Fail formats the message as a failure and writes it to the writer.
func (w *Writer) Fail(m string) {
	w.error(color.New(color.FgRed).Sprint("Error: "))
	w.Errorln(m)
}

// Failf formats the message with the format string and writes it to the writer
// as a Failure message.
func (w *Writer) Failf(m string, f ...interface{}) {
	w.Fail(fmt.Sprintf(m, f...))
}

// FailError formats the error's message as a Failure message and writes it to the
// writer.
func (w *Writer) FailError(e error) {
	w.Fail(e.Error())
}

// FailWrap wraps the error's message and outputs it to the writer as a Failure
// message.
func (w *Writer) FailWrap(e error, m string) {
	w.Fail(errors.Wrap(e, m).Error())
}

// FailCause prints the error's message and attempts to locate a root cause.
// In the event a root cause is found that is not the error it will also
// print the cause.
func (w *Writer) FailCause(e error) {
	w.Fail(e.Error())
	cause := errors.Cause(e)
	if e != cause {
		w.error(color.New(color.FgRed).Sprint("Cause: "))
		w.Errorf("%s\n", strings.Replace(cause.Error(), "\n", "\n    ", -1))
	}
}

// FailWrapCause wraps the error's message and prints the error message. In
// the event a root cause is found that is not the error it will also
// print the cause.
func (w *Writer) FailWrapCause(e error, m string) {
	w.FailCause(errors.Wrap(e, m))
}

// Failt prints the error message with an error type header and message
func (w *Writer) Failt(errType string, m string) {
	w.error(color.New(color.FgRed).Sprintf("%s: ", errType))
	w.Errorln(m)
}

// Failtf prints the error message with an error type header and with a formatted
// message
func (w *Writer) Failtf(errType string, m string, f ...interface{}) {
	w.Failt(errType, fmt.Sprintf(m, f...))
}

// HR prints a horizontal rule (80 chars of "-")
func (w *Writer) HR() {
	w.write(hr)
	w.write("\n")
}

// ErrorHeader prints an ASCII horizontal rule, then "ERROR: " plus your text
// and then another hr, like this:
// ErrorHeader("didn't catch 'em all :'(")
// --------------------------------------------------------------------------------
// ERROR: didn't catch 'em all :'(
// --------------------------------------------------------------------------------
func (w *Writer) ErrorHeader(text string) {
	formatted := fmt.Sprintf("%s\nERROR: %s\n%s\n\n", hr, text, hr)
	w.error(formatted)
}

// Confirm asks the user if they want to take the action defined in the
// question or not.
// Ex:
// Confirm("should we do the thing?")
// > should we do the thing? (y/n)
// <waits for stdin>
func (w *Writer) Confirm(question string) (bool, error) {
	w.Printf("%s (y/n)\n", question)
	rawUserResponse, err := w.reader().ReadString('\n')
	if err != nil {
		return false, errors.Wrap(err, "failed to read user input in confirmation")
	}
	chompedResponse := strings.TrimRight(rawUserResponse, "\n")
	response := strings.ToLower(chompedResponse)
	switch response {
	case "y", "yes", "yaas", "yaass":
		return true, nil
	case "n", "no", "nope", "nah":
		return false, nil
	default:
		w.Printf("I don't understand '%s'. Please type 'y' or 'n'.\n", chompedResponse)
		return w.Confirm(question)
	}
}

func (w *Writer) Prompt(promptText string) (response string, err error) {
	w.Printf("%s: ", promptText)
	rawUserResponse, err := w.reader().ReadString('\n')
	if err != nil {
		return "", errors.Wrap(err, "failed to read user input in confirmation")
	}
	return strings.TrimRight(rawUserResponse, "\n"), nil
}

// StartSpinner starts the CLI spinner
func (w *Writer) StartSpinner() {
	if w.spinner == nil {
		w.writer().Flush()
		w.spinner = spinner.New(spinner.CharSets[3], 100*time.Millisecond)
		w.spinner.Prefix = "  "
		w.spinner.Writer = w.writerStream
	}
	w.spinner.Start()
}

// StopSpinner stops the CLI spinner
func (w *Writer) StopSpinner() {
	if w.spinner != nil {
		w.spinner.Stop()
	}
	w.spinner = nil
}

// Writer returns a pointer to the cli.Writer's io.Writer
func (w *Writer) Writer() io.Writer {
	return w.writer()
}

// BufferWriter returns a pointer to the cli.Writer's bufio.Writer
func (w *Writer) BufferWriter() *bufio.Writer {
	return w.writer()
}

// Indent takes a message and returns it with indentation.
func Indent(m string) string {
	return fmt.Sprintf("  %s", m)
}

// Body is an alias for Indent. It takes a message and returns it with indentation.
func Body(m string) string {
	return Indent(m)
}

// Writer implements the io.Writer interface
func (w *Writer) Write(p []byte) (int, error) {
	n := w.write(fmt.Sprintf("%s", p))
	return n, nil
}

func (w *Writer) write(m string) int {
	n, _ := w.writer().WriteString(m)
	if e := w.writer().Flush(); e != nil {
		return n
	}
	return n
}

func (w *Writer) error(m string) int {
	n, _ := w.errWriter().WriteString(m)
	if e := w.errWriter().Flush(); e != nil {
		return n
	}
	return n
}

// lazy-initialize a bufio.Reader to wrap input (usually os.Stdin in
// production).
//
// Whenever we read from stdin, we must re-use this bufio.Reader and nothing
// else should read from stdin because bufio.Reader will "over read" from the
// input and keep the extra stuff in its buffer. Therefore any subsequent
// attempt to read from stdin not using the same bufio.Reader will lose
// whatever data is kept in our reader's buffer.
func (w *Writer) reader() *bufio.Reader {
	if w.readBuffer == nil {
		w.readBuffer = bufio.NewReader(w.readerStream)
	}
	return w.readBuffer
}

// lazy-initialize a bufio.Writer to wrap input (usually os.Stdout in
// production).
func (w *Writer) errWriter() *bufio.Writer {
	if w.errorBuffer == nil {
		w.errorBuffer = bufio.NewWriter(w.errorStream)
	}
	return w.errorBuffer
}

// lazy-initialize a bufio.Writer to wrap input (usually os.Stdout in
// production).
func (w *Writer) writer() *bufio.Writer {
	if w.writerBuffer == nil {
		w.writerBuffer = bufio.NewWriter(w.writerStream)
	}
	return w.writerBuffer
}
