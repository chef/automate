package api_test

import (
	"testing"

	"github.com/chef/automate/lib/grpc/grpctest"
	"github.com/stretchr/testify/require"

	// needed to accommodate that most of our protobuf files define their own package
	_ "github.com/chef/automate/api/external/cfgmgmt"
	_ "github.com/chef/automate/api/external/common/query"
	_ "github.com/chef/automate/api/external/common/version"
	_ "github.com/chef/automate/api/external/compliance/profiles"
	_ "github.com/chef/automate/api/external/compliance/reporting"
	_ "github.com/chef/automate/api/external/compliance/reporting/stats"
	_ "github.com/chef/automate/api/external/compliance/scanner/jobs"
	_ "github.com/chef/automate/api/external/nodes"
	_ "github.com/chef/automate/api/external/nodes/manager"
	_ "github.com/chef/automate/api/external/secrets"
	_ "github.com/chef/automate/components/automate-gateway/api/deployment"
	_ "github.com/chef/automate/components/automate-gateway/api/gateway"
	_ "github.com/chef/automate/api/external/iam/v2"
	_ "github.com/chef/automate/components/automate-gateway/api/legacy"
	_ "github.com/chef/automate/components/automate-gateway/api/license"
	_ "github.com/chef/automate/components/automate-gateway/api/notifications"
	_ "github.com/chef/automate/components/automate-gateway/api/telemetry"
)

func TestGeneratedProtobufUpToDate(t *testing.T) {
	grpctest.AssertCompiledInUpToDate(t, "automate-gateway/api")
}

// Note: currently, we only generate policy code for the gateway. As long as
// that holds, these checks are only applicable _here_.

func TestGeneratedPolicyUpToDate(t *testing.T) {
	foundProtos := grpctest.FindServiceProtos(t, "automate-gateway/api")
	t.Run("ensure the test finds files to test", func(t *testing.T) {
		require.NotEmpty(t, foundProtos)
	})

	for _, file := range foundProtos {
		t.Run(file, func(t *testing.T) {
			grpctest.AssertGeneratedPolicyUpToDate(t, file)
		})
	}
}

func TestAllProtoFilesAnnotated(t *testing.T) {
	const annotations = "chef.automate.api.iam.policy"
	foundProtos := grpctest.FindServiceProtos(t, "automate-gateway/api")
	t.Run("ensure the test finds files to test", func(t *testing.T) {
		require.NotEmpty(t, foundProtos)
	})

	for _, file := range foundProtos {
		t.Run(file, func(t *testing.T) {
			grpctest.AssertAllProtoMethodsAnnotated(t, file, annotations)
		})
	}
}
