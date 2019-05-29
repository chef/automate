package grpctest

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

// this is manually crafted from
// https://github.com/grpc/grpc-go/blob/f0a1202acdc/codes/codes.go#L148-L166
// because I haven't found any better method
var codeToString = map[codes.Code]string{
	codes.OK:                 "OK",
	codes.Canceled:           "Canceled",
	codes.Unknown:            "Unknown",
	codes.InvalidArgument:    "InvalidArgument",
	codes.DeadlineExceeded:   "DeadlineExceeded",
	codes.NotFound:           "NotFound",
	codes.AlreadyExists:      "AlreadyExists",
	codes.PermissionDenied:   "PermissionDenied",
	codes.ResourceExhausted:  "ResourceExhausted",
	codes.FailedPrecondition: "FailedPrecondition",
	codes.Aborted:            "Aborted",
	codes.OutOfRange:         "OutOfRange",
	codes.Unimplemented:      "Unimplemented",
	codes.Internal:           "Internal",
	codes.Unavailable:        "Unavailable",
	codes.DataLoss:           "DataLoss",
	codes.Unauthenticated:    "Unauthenticated",
}

// AssertCode is an assertion helper that unwraps the passed error argument,
// and compares its code to the expected code argument. On mismatch, it displays
// the textual code representation. Fails the test if the passed error is nil.
// Returns true if the assertion was met (same as the well-known testify
// methods).
func AssertCode(t *testing.T, expected codes.Code, err error) bool {
	t.Helper()

	if assert.Error(t, err) {
		s := status.Convert(err)
		actual := s.Code()
		return assert.Equal(t, codeToString[expected], codeToString[actual], "expected status codes to match")
	}
	return false
}
