package osnodevalidator

import "fmt"

// OSNodeValidator interface definition
type OSNodeValidator interface {
	Run() error
}

// OSValidator struct definition
type OSValidator struct {
}

// NewOSNodeValidator returns the new Open Search node validator
func NewOSNodeValidator() (OSNodeValidator, error) {
	return OSValidator{}, nil
}

// Run method performs all open search node validations
func (osValidator OSValidator) Run() error {
	fmt.Println("Running Open Search Validator")
	return nil
}
