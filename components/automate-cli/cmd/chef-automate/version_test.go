// Copyright © 2017 Chef Software

package main

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func Test_extractVersion(t *testing.T) {
	t.Run("Returns OK", func(t *testing.T) {
		versionString := "PostgreSQL 13.5 on x86_64-pc-linux-gnu, compiled by gcc.real (GCC) 9.4.0, 64-bit"
		pattern := `PostgreSQL (\d+\.\d+)`

		versionNumber, err := extractVersion(versionString, pattern)
		assert.Equal(t, nil, err)
		assert.Equal(t, "13.5", versionNumber)
	})
	t.Run("Error1", func(t *testing.T) {
		versionString := "Invalid version string"
		pattern := `PostgreSQL (\d+\.\d+)`

		versionNumber, err := extractVersion(versionString, pattern)
		assert.NotNil(t, err)
		assert.Equal(t, "no version string found", err.Error())
		assert.Empty(t, versionNumber)
	})
	t.Run("Error2", func(t *testing.T) {
		versionString := "PostgreSQL 13.5 on x86_64-pc-linux-gnu, compiled by gcc.real (GCC) 9.4.0, 64-bit"
		pattern := `[`

		versionNumber, err := extractVersion(versionString, pattern)
		assert.NotNil(t, err)
		assert.Empty(t, versionNumber)
	})

}
