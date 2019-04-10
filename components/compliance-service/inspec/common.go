package inspec

import (
	"bytes"
	"encoding/json"
)

//PrettyPrintJSON does just what one would imagine.  Use your imagination.
func PrettyPrintJSON(b []byte) ([]byte, error) {
	var out bytes.Buffer
	err := json.Indent(&out, b, "", "    ")
	return out.Bytes(), err
}

// NewInspecError creates an Error
func NewInspecError(typ string, message string) *Error {
	return &Error{
		Type:    typ,
		Message: message,
	}
}
