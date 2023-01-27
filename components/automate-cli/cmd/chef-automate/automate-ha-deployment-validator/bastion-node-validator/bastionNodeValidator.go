package bastionnodevalidator

import "fmt"

// BastionNodeValidator interface definition
type BastionNodeValidator interface {
	Run() error
}

// BastionValidator struct definition
type BastionValidator struct {
}

// NewBastionNodeValidator returns the new bastion node validator
func NewBastionNodeValidator() (BastionNodeValidator, error) {
	return BastionValidator{}, nil
}

// Run method performs all bastion node validations
func (bValidator BastionValidator) Run() error {
	fmt.Println("Running Bastion Validator")
	return nil
}
