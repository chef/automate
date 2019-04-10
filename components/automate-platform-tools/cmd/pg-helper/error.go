package main

// ExitError encodes an error cause and exit code
type ExitError interface {
	error
	ExitCode() int
}

type exitError struct {
	message  string
	exitCode int
}

func (err *exitError) Error() string {
	return err.message
}

func (err *exitError) ExitCode() int {
	return err.exitCode
}

// NewExitError creates an ExitError
func NewExitError(exitCode int, message string) error {
	return &exitError{
		message:  message,
		exitCode: exitCode,
	}
}
