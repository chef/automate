package main

import (
	"errors"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestExecCommand(t *testing.T) {
	tests := []struct {
		TestName       string
		Input          string
		ExpectedOutput string
		ExpectedError  error
	}{
		{"Valid Command", "echo Hello", "Hello\n", nil},
		{"Invalid Command", "ech Hello", "", errors.New("")},
	}
	for _, e := range tests {
		t.Run(e.TestName, func(t *testing.T) {
			output, err := execCommand(e.Input)
			if e.ExpectedError != nil {
				assert.Error(t, err)
			} else {
				assert.NoError(t, err)
				assert.Equal(t, e.ExpectedOutput, output)
			}
		})
	}
}
