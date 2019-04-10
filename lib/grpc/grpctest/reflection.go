package grpctest

import (
	"testing"

	"github.com/jhump/protoreflect/grpcreflect"
	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc"
)

// AssertReflection checks if the passed *grpc.Server instance supports
// reflection. It thereby checks that all the used proto files (e.g. for adding
// validation rules, or policy info) can be retrieved through this service.
// This is a requirement for being able to use fullstackdev/grpcurl with the
// exposed service.
func AssertReflection(t *testing.T, serv *grpc.Server) {
	t.Helper()

	sds, err := grpcreflect.LoadServiceDescriptors(serv)
	if err != nil {
		t.Fatalf("reflect server API: %s", err)
	}

	foundAny := false
	for name := range sds {
		if name == "grpc.reflection.v1alpha.ServerReflection" {
			continue // skip reflection, it's not defined in any of our .proto files
		}
		t.Logf("found %s", name)
		foundAny = true
	}

	assert.True(t, foundAny)
}
