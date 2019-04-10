package pgw

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestDropTablesQuery(t *testing.T) {
	t.Run("with cascade", func(t *testing.T) {
		q := NewDropTablesQuery()
		q.Tables = []string{"foo", "bar_baz", "qux"}
		q.Cascade = true

		assert.Equal(t, "DROP TABLE IF EXISTS foo, bar_baz, qux CASCADE", q.String())
	})

	t.Run("without cascade", func(t *testing.T) {
		q := NewDropTablesQuery()
		q.Tables = []string{"foo", "bar_baz", "qux"}
		q.Cascade = false

		assert.Equal(t, "DROP TABLE IF EXISTS foo, bar_baz, qux ", q.String())
	})
}
