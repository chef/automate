package csnodevalidator

import "fmt"

// CSNodeValidator interface definition
type CSNodeValidator interface {
	Run() error
}

// CSValidator struct definition
type CSValidator struct {
}

// NewCSNodeValidator returns the new CS node validator
func NewCSNodeValidator() (CSNodeValidator, error) {
	return CSValidator{}, nil
}

// Run method performs all chef server node validations
func (csValidator CSValidator) Run() error {
	fmt.Println("Running Chef Server Validator")
	return nil
}
