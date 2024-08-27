package main

import (
	"testing"

	"github.com/stretchr/testify/require"
)

func Test_compareVersions(t *testing.T) {
	v2 := "4.12.69"

	t.Run("Test 1", func(t *testing.T) {
		isNew := compareVersions("4.13.1", v2)
		require.True(t, isNew)
	})

	t.Run("Test 2", func(t *testing.T) {
		isNew := compareVersions("20211201164433", v2)
		require.False(t, isNew)
	})

	t.Run("Test 3", func(t *testing.T) {
		isNew := compareVersions("4.9.9", v2)
		require.False(t, isNew)
	})

	t.Run("Test 4", func(t *testing.T) {
		isNew := compareVersions("5.0.0", v2)
		require.True(t, isNew)
	})
	t.Run("Test 5", func(t *testing.T) {
		isNew := compareVersions("4.12.123", v2)
		require.True(t, isNew)
	})

}
