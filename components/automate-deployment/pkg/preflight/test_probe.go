package preflight

import (
	"context"
	"fmt"
	"io/ioutil"
	"net/http"
	"os"
	"os/exec"
	"os/user"
	"strings"
	"time"

	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/platform/sys"
)

type Reporter interface {
	ReportSuccess(string)
	ReportFailure(string)
	ReportSummary(string)
}

type TestProbe interface {
	Reporter
	File(filePath string) ([]byte, error)
	IsSymlink(filePath string) (bool, error)
	Euid() int
	AvailableDiskSpace(path string) (uint64, error)
	LookupUser(username string) (*user.User, error)
	LookPath(file string) (string, error)
	HTTPConnectivity(url string) error
}

type ConsoleReporter struct {
	summaries   []string
	hasFailures bool
}

func (*ConsoleReporter) ReportSuccess(msg string) {
	fmt.Printf(" OK | %s\n", msg)
}

func (r *ConsoleReporter) ReportFailure(msg string) {
	r.hasFailures = true
	fmt.Printf("FAIL| %s\n", msg)
}

func (r *ConsoleReporter) ReportSummary(msg string) {
	r.summaries = append(r.summaries, strings.TrimSpace(msg))
}

func (r *ConsoleReporter) HasFailures() bool {
	return r.hasFailures
}

func (c *ConsoleReporter) PrintSummaries() {
	for _, s := range c.summaries {
		fmt.Printf("%s\n\n", s)
	}
}

func NewConsoleReporter() *ConsoleReporter {
	return &ConsoleReporter{
		summaries: []string{},
	}
}

type InMemoryPrintReporter struct {
	builder     strings.Builder
	summaries   []string
	hasFailures bool
}

func NewInMemoryPrintReporter() *InMemoryPrintReporter {
	return &InMemoryPrintReporter{
		summaries: []string{},
	}
}

func (r *InMemoryPrintReporter) ReportSuccess(msg string) {
	fmt.Fprintf(&r.builder, " OK | %s\n", msg)
}

func (r *InMemoryPrintReporter) ReportFailure(msg string) {
	r.hasFailures = true
	fmt.Fprintf(&r.builder, "FAIL| %s\n", msg)
}

func (r *InMemoryPrintReporter) ReportSummary(msg string) {
	r.summaries = append(r.summaries, strings.TrimSpace(msg))
}

func (r *InMemoryPrintReporter) Out() string {
	if len(r.summaries) == 0 {
		return r.builder.String()
	}
	return fmt.Sprintf("\n%s\n%s\n", r.builder.String(), strings.Join(r.summaries, "\n\n"))
}

func (r *InMemoryPrintReporter) HasFailures() bool {
	return r.hasFailures
}

type localTestProbe struct {
	Reporter
}

func NewTestProbe(r Reporter) TestProbe {
	return &localTestProbe{
		Reporter: r,
	}
}

func (*localTestProbe) File(path string) ([]byte, error) {
	return ioutil.ReadFile(path)
}

func (*localTestProbe) IsSymlink(filePath string) (bool, error) {
	return fileutils.IsSymlink(filePath)
}

func (*localTestProbe) Euid() int {
	return os.Geteuid()
}

func (*localTestProbe) AvailableDiskSpace(path string) (uint64, error) {
	v, err := sys.SpaceAvailForPath(path)
	return v * 1024, err
}

func (*localTestProbe) LookupUser(username string) (*user.User, error) {
	return user.Lookup(username)
}

func (*localTestProbe) LookPath(file string) (string, error) {
	return exec.LookPath(file)
}

func (*localTestProbe) HTTPConnectivity(url string) error {
	req, err := http.NewRequest(http.MethodHead, url, nil)
	if err != nil {
		logrus.WithError(err).Debugf("failed to prepare request to HEAD %s", url)
		return err
	}
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()
	resp, err := http.DefaultClient.Do(req.WithContext(ctx))

	if err != nil {
		logrus.WithError(err).Debugf("failed to HEAD %s", url)
		return err
	}
	defer resp.Body.Close()
	if resp.StatusCode >= 400 {
		logrus.Debugf("HEAD %s return a non success %q", url, resp.Status)
	}

	return nil
}
