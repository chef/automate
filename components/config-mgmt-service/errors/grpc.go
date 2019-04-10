package errors

import (
	"fmt"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"
)

// NewGrpcErrorWithErr creates and/or transforms a provided error into a gRPC error
//
// If the provided `Error` is a known Error within this package we will
// leverage its details to convert it into a GrpcError. If it is not, we
// will call directly the method `Error()` that will return a `string`
// that we will use as the Standard Error Message.
func GrpcErrorFromErr(code codes.Code, err error) error {
	switch e := err.(type) {
	case StandardError:
		return GrpcError(code, e.ToString())
	case BackendError:
		return GrpcError(code, e.StandardError.ToString())
	default:
		// To any other error we will call its method
		// `Error()` and pass it as the error message
		return GrpcError(code, err.Error())
	}
}

// NewGrpcErrorWithMessage creates and/or transforms a provided error into a gRPC error
func GrpcError(code codes.Code, message string) error {
	return status.Error(code, message)
}

// NewGrpcErrorWithMessagef creates and/or transforms a provided error into a gRPC error
func GrpcErrorf(c codes.Code, format string, a ...interface{}) error {
	return status.Error(c, fmt.Sprintf(format, a...))
}
