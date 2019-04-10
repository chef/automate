package client_test

import (
	"context"
	"errors"
	"fmt"
	"runtime"
	"testing"

	"github.com/stretchr/testify/assert"

	"google.golang.org/grpc"

	api "github.com/chef/automate/api/interservice/deployment"
	ds "github.com/chef/automate/components/automate-deployment/pkg/client"
)

type mockWarningWriter struct {
	warning string
}

func (m *mockWarningWriter) Warn(msg string) {
	m.warning = msg
}

func (m *mockWarningWriter) Warnf(msg string, f ...interface{}) {
	m.warning = fmt.Sprintf(msg, f...)

}

func (m *mockWarningWriter) WarnError(err error) {
	m.warning = err.Error()
}

func (m *mockWarningWriter) AssertNoWarning(t *testing.T) {
	assert.Empty(t, m.warning)
}

func (m *mockWarningWriter) AssertWarningMatches(t *testing.T, matchingStr string) {
	assert.Regexp(t, matchingStr, m.warning)
}

type mockVersionManifestClient struct {
	resp *api.ManifestVersionResponse
	err  error
}

func (m *mockVersionManifestClient) ManifestVersion(ctx context.Context, in *api.ManifestVersionRequest, opts ...grpc.CallOption) (*api.ManifestVersionResponse, error) {
	return m.resp, m.err
}

func TestNoWarningIfErrorEncountered(t *testing.T) {
	release := "20180419150000"
	client := &mockVersionManifestClient{err: errors.New("Bad stuff")}
	writer := &mockWarningWriter{}

	ds.WarnIfNotUpToDateAgainstServer(client, writer, release)

	writer.AssertNoWarning(t)
}

func TestNoWarningIfCliVersionNotReturned(t *testing.T) {
	release := "20180419150000"
	client := &mockVersionManifestClient{
		resp: &api.ManifestVersionResponse{
			CliRelease: "",
		},
	}

	writer := &mockWarningWriter{}

	ds.WarnIfNotUpToDateAgainstServer(client, writer, release)

	writer.AssertNoWarning(t)
}

func TestNoWarningIfVersionIsEqual(t *testing.T) {
	release := "20180419150000"
	client := &mockVersionManifestClient{
		resp: &api.ManifestVersionResponse{
			CliRelease: release,
		},
	}
	writer := &mockWarningWriter{}

	ds.WarnIfNotUpToDateAgainstServer(client, writer, release)

	writer.AssertNoWarning(t)
}

func TestNoWarningIfVersionIsNewer(t *testing.T) {
	release := "20180419150000"
	client := &mockVersionManifestClient{
		resp: &api.ManifestVersionResponse{
			CliRelease: release,
		},
	}
	writer := &mockWarningWriter{}

	ds.WarnIfNotUpToDateAgainstServer(client, writer, release)

	writer.AssertNoWarning(t)
}

func TestWarnsIfVersionIsOld(t *testing.T) {
	releaseOld := "20180419150000"
	releaseNew := "20180419150001"
	client := &mockVersionManifestClient{
		resp: &api.ManifestVersionResponse{
			BuildTimestamp: releaseOld,
			CliRelease:     releaseNew,
		},
	}
	writer := &mockWarningWriter{}

	ds.WarnIfNotUpToDateAgainstServer(client, writer, releaseOld)

	writer.AssertWarningMatches(t, fmt.Sprintf("installed: %s", releaseOld))
	writer.AssertWarningMatches(t, fmt.Sprintf("latest: %s", releaseNew))
	writer.AssertWarningMatches(t, fmt.Sprintf("https://packages.chef.io/files/automate/20180419150000/chef-automate_%s_amd64.zip", runtime.GOOS))
}
