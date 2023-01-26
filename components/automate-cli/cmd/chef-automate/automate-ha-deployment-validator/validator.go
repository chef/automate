package automatehadeploymentvalidator

import "fmt"

// HADeploymentValidator interface definition
type HADeploymentValidator interface {
	RunHAValidator() error
}

// NewHADeploymentValidator returns the new HA deployment validator
func NewHADeploymentValidator() (HADeploymentValidator, error) {
	return &HAValidator{}, nil
}

// HAValidator struct definition
type HAValidator struct {
}

// RunHAValidator run the infra validation
func (validator *HAValidator) RunHAValidator() error {
	fmt.Println("Running HA Validator")
	return nil
}
