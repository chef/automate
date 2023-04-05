// Copyright © 2017 Chef Software

package main

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func Test_printInfo(t *testing.T) {
	t.Run("Passed ", func(t *testing.T) {
		automate := &AutomateHAInfraDetails{}
		tmpl, err := printInfo(automate)
		require.NoError(t, err)
		require.NotNil(t, tmpl)
	})

	t.Run("Failed to parse", func(t *testing.T) {
		automate := &AutomateHAInfraDetails{}
		infoCommandTemp = "Hello, {{Name}}"
		tmpl, err := printInfo(automate)
		require.Error(t, err)
		require.Nil(t, tmpl)
	})

	t.Run("Failed to execute tmpl", func(t *testing.T) {
		automate := &AutomateHAInfraDetails{}
		infoCommandTemp = "Hello, {{.Name}}"
		tmpl, err := printInfo(automate)
		require.Error(t, err)
		require.Nil(t, tmpl)
	})
}
