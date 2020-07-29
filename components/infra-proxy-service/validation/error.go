package validation

import (
	"fmt"

	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/status"

	"google.golang.org/genproto/googleapis/rpc/errdetails"
)

// NewError creates new validation error for field violoations.
func NewError(violations []*FieldViolation) error {
	errViolations := make([]*errdetails.BadRequest_FieldViolation, len(violations))
	newStatus := status.New(codes.InvalidArgument, "invalid argument") // TODO: need better message

	for i, v := range violations {
		errViolations[i] = &errdetails.BadRequest_FieldViolation{
			Field:       v.Field,
			Description: v.Description,
		}
	}

	badRequestErr := &errdetails.BadRequest{
		FieldViolations: errViolations,
	}

	newStatus, err := newStatus.WithDetails(badRequestErr)
	if err != nil {
		// If this errored, it will always error
		// here, so better panic so we can figure
		// out why than have this silently passing.
		panic(fmt.Sprintf("Unexpected error attaching metadata: %v", err))
	}
	return newStatus.Err()
}

// NewPlainError creates new validation error.
func NewPlainError(fv *FieldViolation) error {
	return status.Error(codes.InvalidArgument, fv.Description)
}
