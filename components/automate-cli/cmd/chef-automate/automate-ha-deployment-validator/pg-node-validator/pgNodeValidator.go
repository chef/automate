package pgnodevalidator

import "fmt"

// PGNodeValidator interface definition
type PGNodeValidator interface {
	Run() error
}

// PGValidator struct definition
type PGValidator struct {
}

// NewPGNodeValidator returns the new PG node validator
func NewPGNodeValidator() (PGNodeValidator, error) {
	return PGValidator{}, nil
}

// Run method performs all postgres node validations
func (pgValidator PGValidator) Run() error {
	fmt.Println("Running Postgres Validator")
	return nil
}
