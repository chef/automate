package api_test

import (
	"testing"

	"github.com/chef/automate/lib/grpc/grpctest"

	// needed to accommodate that most of our protobuf files define their own package
	_ "github.com/chef/automate/api/external/cfgmgmt"
	_ "github.com/chef/automate/api/external/common/query"
	_ "github.com/chef/automate/api/external/common/version"
	_ "github.com/chef/automate/api/external/nodes"
	_ "github.com/chef/automate/api/external/nodes/manager"
	_ "github.com/chef/automate/api/external/secrets"
	_ "github.com/chef/automate/components/automate-gateway/api/auth/tokens"
	_ "github.com/chef/automate/components/automate-gateway/api/auth/users"
	_ "github.com/chef/automate/components/automate-gateway/api/authz"
	_ "github.com/chef/automate/components/automate-gateway/api/compliance/profiles"
	_ "github.com/chef/automate/components/automate-gateway/api/compliance/reporting"
	_ "github.com/chef/automate/components/automate-gateway/api/compliance/reporting/stats"
	_ "github.com/chef/automate/components/automate-gateway/api/compliance/scanner/jobs"
	_ "github.com/chef/automate/components/automate-gateway/api/deployment"
	_ "github.com/chef/automate/components/automate-gateway/api/event_feed"
	_ "github.com/chef/automate/components/automate-gateway/api/gateway"
	_ "github.com/chef/automate/components/automate-gateway/api/iam/v2"
	_ "github.com/chef/automate/components/automate-gateway/api/legacy"
	_ "github.com/chef/automate/components/automate-gateway/api/license"
	_ "github.com/chef/automate/components/automate-gateway/api/notifications"
	_ "github.com/chef/automate/components/automate-gateway/api/telemetry"
)

func TestGeneratedProtobufUpToDate(t *testing.T) {
	grpctest.AssertCompiledInUpToDate(t, "components/automate-gateway/api")
}

// Note: currently, we only generate policy code for the gateway. As long as
// that holds, these checks are only applicable _here_.

func TestGeneratedPolicyUpToDate(t *testing.T) {
	for _, file := range grpctest.FindServiceProtos(t, "components/automate-gateway/api") {
		t.Run(file, func(t *testing.T) {
			grpctest.AssertGeneratedPolicyUpToDate(t, file)
		})
	}
}

func TestAllProtoFilesAnnotated(t *testing.T) {
	const v1Annotations = "chef.automate.api.policy"
	const v2Annotations = "chef.automate.api.iam.policy"
	for _, file := range grpctest.FindServiceProtos(t, "components/automate-gateway/api") {
		t.Run(file, func(t *testing.T) {
			grpctest.AssertAllProtoMethodsAnnotated(t, file, v1Annotations)
			grpctest.AssertAllProtoMethodsAnnotated(t, file, v2Annotations)
		})
	}
}
