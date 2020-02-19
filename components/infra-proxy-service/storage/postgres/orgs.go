package postgres

import (
	"context"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/infra-proxy-service/storage"
	uuid "github.com/chef/automate/lib/uuid4"
)

// StoreOrg saves a org to the DB.
func (p *postgres) StoreOrg(ctx context.Context, name string, adminUser string, adminKey string, serverID string, projects []string) (storage.Org, error) {
	return p.insertOrg(ctx, name, adminUser, adminKey, serverID, projects)
}

func (p *postgres) insertOrg(ctx context.Context,
	name string, adminUser string, adminKey string, serverID string, projects []string) (storage.Org, error) {

	var org storage.Org
	err := p.db.QueryRowContext(ctx,
		`INSERT INTO orgs (id, name, admin_user, admin_key, server_id, created_at, updated_at)
		VALUES (uuid_generate_v4(), $1, $2, $3, $4, now(), now())
		RETURNING id, name, admin_user, admin_key, server_id, created_at, updated_at`,
		name, adminUser, adminKey, serverID).
		Scan(&org.ID, &org.Name, &org.AdminUser, &org.AdminKey, &org.ServerId, &org.CreatedAt, &org.UpdatedAt)
	if err != nil {
		return storage.Org{}, p.processError(err)
	}

	return org, nil
}

// GetOrg fetches a org by id.
func (p *postgres) GetOrg(ctx context.Context, orgID uuid.UUID) (storage.Org, error) {
	return p.getOrg(ctx, p.db, orgID)
}

func (p *postgres) getOrg(ctx context.Context, q querier, orgID uuid.UUID) (storage.Org, error) {
	var org storage.Org
	err := q.QueryRowContext(ctx,
		`SELECT o.id, o.name, o.admin_user, o.admin_key, o.server_id, o.updated_at, o.created_at
		FROM orgs o
		WHERE o.id = $1`, orgID).
		Scan(&org.ID, &org.Name, &org.AdminUser, &org.AdminKey, &org.ServerId, &org.CreatedAt, &org.UpdatedAt)
	if err != nil {
		return storage.Org{}, p.processError(err)
	}
	return org, nil
}

// GetOrgByName fetches a org by name.
// TODO: Replace serverID with server name
// Prefer LEFT OUTER join to in order to append server detail in response
func (p *postgres) GetOrgByName(ctx context.Context, orgName string, serverID uuid.UUID) (storage.Org, error) {
	var org storage.Org
	err := p.db.QueryRowContext(ctx,
		`SELECT o.id, o.name, o.admin_user, o.admin_key, o.server_id, o.updated_at, o.created_at
		FROM orgs o
		WHERE o.name = $1 AND o.server_id = $2`,
		orgName, serverID).
		Scan(&org.ID, &org.Name, &org.AdminUser, &org.AdminKey, &org.ServerId, &org.CreatedAt, &org.UpdatedAt)
	if err != nil {
		return storage.Org{}, p.processError(err)
	}
	return org, nil
}

// DeleteOrg deletes a org from the DB.
func (p *postgres) DeleteOrg(ctx context.Context, orgID uuid.UUID) (storage.Org, error) {
	var org storage.Org
	err := p.db.QueryRowContext(ctx,
		`DELETE FROM orgs WHERE id = $1
		RETURNING id, name, admin_user, admin_key, server_id, created_at, updated_at`, orgID).
		Scan(&org.ID, &org.Name, &org.AdminUser, &org.AdminKey, &org.ServerId, &org.CreatedAt, &org.UpdatedAt)
	if err != nil {
		return storage.Org{}, p.processError(err)
	}

	return org, nil
}

// EditOrg does a full update on a database org.
func (p *postgres) EditOrg(ctx context.Context, org storage.Org) (storage.Org, error) {
	var o storage.Org

	err := p.db.QueryRowContext(ctx,
		`UPDATE orgs
		SET name = $2, admin_user = $3, admin_key = $4, server_id = $5, updated_at = now()
		WHERE id = $1
		RETURNING id, name, admin_user, admin_key, server_id, created_at, updated_at`,
		org.ID, org.Name, org.AdminUser, org.AdminKey, org.ServerId).
		Scan(&o.ID, &o.Name, &o.AdminUser, &o.AdminKey, &o.ServerId, &o.CreatedAt, &o.UpdatedAt)
	if err != nil {
		return storage.Org{}, p.processError(err)
	}

	return o, nil
}

// GetOrgs fetches orgs from the DB as an array.
func (p *postgres) GetOrgs(ctx context.Context, serverID uuid.UUID) ([]storage.Org, error) {

	var orgs []storage.Org
	// TODO eventually these should be ordered
	rows, err := p.db.QueryContext(ctx,
		`SELECT o.id, o.name, o.admin_user, o.admin_key, o.server_id, o.updated_at, o.created_at
		FROM orgs o
		WHERE o.server_id = $1`, serverID)
	if err != nil {
		return []storage.Org{}, p.processError(err)
	}
	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	for rows.Next() {
		org := storage.Org{}
		if err := rows.Scan(&org.ID, &org.Name, &org.AdminUser, &org.AdminKey, &org.ServerId,
			&org.CreatedAt, &org.UpdatedAt); err != nil {
			return nil, err // TODO: don't fail it all? handle this more gracefully?
		}
		orgs = append(orgs, org)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return orgs, nil
}
