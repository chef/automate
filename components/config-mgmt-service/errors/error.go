//
//  Author:: Salim Afiune <afiune@chef.io>
//  Copyright:: Copyright 2017, Chef Software Inc.
//

package errors

import (
	"encoding/json"
	"fmt"
)

// ErrorType will be our enum to identity types of errors
type ErrorType int

// Add multiple error types that will be associated with an integer,
// then make sure to update the `typeString` map below to do the
// translation to String
const (
	GrpcClient ErrorType = iota
	Backend
	InvalidParameter
	NodeNotFound
	NodeAttributeNotFound
	RunNotFound
	ActionNotFound
)

var typeString = map[ErrorType]string{
	GrpcClient:            "GrpcClient",
	Backend:               "Backend",
	InvalidParameter:      "InvalidParameter",
	NodeNotFound:          "NodeNotFound",
	NodeAttributeNotFound: "NodeAttributeNotFound",
	RunNotFound:           "RunNotFound",
	ActionNotFound:        "ActionNotFound",
}

// StandardError type
//
// For any standard error (that is not either a RestClient nor a Backend), use
// this error handler to standardize all errors reported to the users and/or
// logging to the systems log
//
// Example: When there is an invalid `filter` parameter
// ```
//  errors.New(
//    errors.InvalidParameter,
//    fmt.Sprintf("Invalid filter '%s' (format: key:value)", filter),
//  )
// ```
type StandardError struct {
	Type    ErrorType `json:"-"` // Avoid displaying the type to the user
	Message string    `json:"message"`
}

// New creates a new StandardError struct
func New(eType ErrorType, msg string) *StandardError {
	return &StandardError{eType, msg}
}

// Implementation of the `Error` interface
func (e StandardError) Error() string {
	return e.ToString()
}

// TypeString converts our enum ErrorType into a readable string
func (e StandardError) TypeString() string {
	return typeString[e.Type]
}

// ToJSON converts the StandardError into JSON format
func (e StandardError) ToJSON() ([]byte, error) {
	return json.Marshal(e)
}

// ToString converts the StandardError into String format
func (e StandardError) ToString() string {
	return fmt.Sprintf(
		"type=%s msg=\"%s\"",
		e.TypeString(),
		e.Message,
	)
}
