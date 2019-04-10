package pgw

import (
	"fmt"
	"strings"
)

// DropTablesQuery represents the available options for an ALTER ROLE query
type DropTablesQuery struct {
	Tables  []string
	Cascade bool
}

// NewDropTablesQuery returns a pointer to a new AlterRoleQuery
func NewDropTablesQuery() *DropTablesQuery {
	return &DropTablesQuery{
		Tables: []string{},
	}
}

// String returns the query as SQL
func (q *DropTablesQuery) String() string {
	cascade := ""
	if q.Cascade {
		cascade = "CASCADE"
	}

	return fmt.Sprintf("DROP TABLE IF EXISTS %s %s", strings.Join(q.Tables, ", "), cascade)
}
