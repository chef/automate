package errors

import "fmt"

// BackendError type
//
// Whenever there is an error in the Backend, use
// this error handler to standardize all errors
// reported to the users and/or logging to the systems log
//
// Example:
// ```
//  statusResult, found := searchResult.Aggregations.Terms("status_counts")
//  if !found {
//    return errors.NewBackendError("Aggregation term 'status_counts' not found")
//  }
// ```
type BackendError struct {
	*StandardError
}

// NewBackendError - create a new backend error
func NewBackendError(format string, args ...interface{}) *BackendError {
	return &BackendError{StandardError: New(Backend, fmt.Sprint(format, args))}
}
