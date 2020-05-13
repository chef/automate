// +build !prod

package postgres

import (
	"context"
)

func (p *postgres) Reset(ctx context.Context) error {
	_, err := p.db.ExecContext(ctx, "TRUNCATE table servers CASCADE;")
	return err
}
