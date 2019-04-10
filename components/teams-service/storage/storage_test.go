package storage

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestIsTeamDeletable(t *testing.T) {
	t.Run("when a team is deletable", func(t *testing.T) {
		assert.Equal(t, IsTeamDeletable("totally deletable"), true)
	})

	t.Run("when a team is not deletable", func(t *testing.T) {
		assert.Equal(t, IsTeamDeletable(NonDeletableTeams[0]), false)
	})
}
