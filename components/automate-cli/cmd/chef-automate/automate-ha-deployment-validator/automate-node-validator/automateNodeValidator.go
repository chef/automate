package automatenodevalidator

import "fmt"

// AutomateNodeValidator interface definition
type AutomateNodeValidator interface {
	Run() error
}

// AutomateValidator struct definition
type AutomateValidator struct {
}

// NewAutomateNodeValidator returns the new Automate node validator
func NewAutomateNodeValidator() (AutomateNodeValidator, error) {
	return AutomateValidator{}, nil
}

// Run method performs all automate node validations
func (aValidator AutomateValidator) Run() error {
	fmt.Println("Running Automate Validator")
	return nil
}
