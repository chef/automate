package deployment_test

import (
	"testing"

	"github.com/chef/automate/lib/grpc/grpctest"
)

func TestGeneratedProtobufUpToDate(t *testing.T) {
	grpctest.AssertCompiledInUpToDate(t, "api/interservice/deployment")
}
