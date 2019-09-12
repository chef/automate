package runner

import (
	"fmt"
	"strings"
	"time"

	"github.com/briandowns/spinner"
)

// A Reporter is used to report runner lifecycle events
type Reporter interface {
	ReportGenerateSkip(componentName, msg string)
	ReportGenerateSkipErrored(componentName, msg string)
	ReportGenerateStart(componentName string)
	ReportGenerateSuccess(componentName string)
	ReportGenerateErrored(componentName, msg string)

	ReportVerifySkip(componentName, msg string)
	ReportVerifySkipErrored(componentName, msg string)
	ReportVerifyStart(componentName string)
	ReportVerifyFinish(componentName string)
	ReportVerifyFailure(componentName, msg string)

	ReportCleanupSkip(componentName, msg string)
	ReportCleanupSkipErrored(componentName, msg string)
	ReportCleanupStart(componentName string)
	ReportCleanupSuccess(componentName string)
	ReportCleanupErrored(componentName, msg string)

	SummarizeFailures()
}

type defaultReporter struct {
	inVerifyFailureMode bool
	s                   *spinner.Spinner
}

// NewDefaultReporter creates a reporter that pretty prints messages and does
// all the fancy progress spinner stuff
func NewDefaultReporter() Reporter {
	return &defaultReporter{
		s: spinner.New(spinner.CharSets[14], 100*time.Millisecond),
	}
}

func (r *defaultReporter) ReportGenerateSkip(componentName string, msg string) {
	r.s.Stop()
	fmt.Printf("[-] Skipping generating data for %s\n", componentName)
	fmt.Printf("%s\n", formatMessage(msg))
}
func (r *defaultReporter) ReportGenerateSkipErrored(componentName string, msg string) {
	r.s.Stop()
	fmt.Printf("[✗] Failed skipping generate for %s\n", componentName)
	fmt.Printf("%s\n", formatMessage(msg))
}
func (r *defaultReporter) ReportGenerateStart(componentName string) {
	r.s.Prefix = "["
	r.s.Suffix = fmt.Sprintf("] Generating data for %s", componentName)
	r.s.Start()
}
func (r *defaultReporter) ReportGenerateSuccess(componentName string) {
	r.s.Stop()
	fmt.Printf("[✓] Generating data for %s\n", componentName)
}
func (r *defaultReporter) ReportGenerateErrored(componentName string, msg string) {
	r.s.Stop()
	fmt.Printf("[✗] Generating data for %s\n", componentName)
	fmt.Printf("%s\n", formatMessage(msg))
}

func (r *defaultReporter) ReportVerifySkip(componentName string, msg string) {
	r.s.Stop()
	fmt.Printf("[-] Skipping verifying data for %s\n", componentName)
	fmt.Printf("%s\n", formatMessage(msg))
}
func (r *defaultReporter) ReportVerifySkipErrored(componentName string, msg string) {
	r.s.Stop()
	fmt.Printf("[✗] Failed skipping verify for %s\n", componentName)
	fmt.Printf("%s\n", formatMessage(msg))
}
func (r *defaultReporter) ReportVerifyStart(componentName string) {
	r.s.Prefix = "["
	r.s.Suffix = fmt.Sprintf("] Verifying %s", componentName)
	r.s.Start()
}
func (r *defaultReporter) ReportVerifyFinish(componentName string) {
	if !r.inVerifyFailureMode {
		r.s.Stop()
		fmt.Printf("[✓] Verifying %s\n", componentName)
	}
	r.inVerifyFailureMode = false
}
func (r *defaultReporter) ReportVerifyFailure(componentName string, msg string) {
	if !r.inVerifyFailureMode {
		r.s.Stop()
		fmt.Printf("[✗] Verifying %s\n", componentName)
		r.inVerifyFailureMode = true
	}
	fmt.Printf("%s\n", formatMessage(msg))
}

func (r *defaultReporter) ReportCleanupSkip(componentName string, msg string) {
	r.s.Stop()
	fmt.Printf("[-] Skipping cleaning up for %s\n", componentName)
	fmt.Printf("%s\n", formatMessage(msg))
}
func (r *defaultReporter) ReportCleanupSkipErrored(componentName string, msg string) {
	r.s.Stop()
	fmt.Printf("[✗] Failed skipping cleanup for %s\n", componentName)
	fmt.Printf("%s\n", formatMessage(msg))
}
func (r *defaultReporter) ReportCleanupStart(componentName string) {
	r.s.Prefix = "["
	r.s.Suffix = fmt.Sprintf("] Cleaning up %s", componentName)
	r.s.Start()
}
func (r *defaultReporter) ReportCleanupSuccess(componentName string) {
	r.s.Stop()
	fmt.Printf("[✓] Cleaning up %s\n", componentName)
}
func (r *defaultReporter) ReportCleanupErrored(componentName string, msg string) {
	r.s.Stop()
	fmt.Printf("[✗] Cleaning up %s\n", componentName)
	fmt.Printf("%s\n", formatMessage(msg))
}

func (*defaultReporter) SummarizeFailures() {}

func formatMessage(msg string) string {
	lines := strings.Split(msg, "\n")
	for i, l := range lines {
		lines[i] = fmt.Sprintf("      %s", l)
	}
	return strings.Join(lines, "\n")
}
