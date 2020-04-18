package postgres

import (
	"context"

	"github.com/pkg/errors"

	"github.com/chef/automate/components/infra-proxy-service/storage"
)

// StoreServer saves a server to the DB.
func (p *postgres) StoreServer(ctx context.Context, id string, name string, fqdn string, ipAddress string) (storage.Server, error) {
	return p.insertServer(ctx, id, name, fqdn, ipAddress)
}

func (p *postgres) insertServer(ctx context.Context,
	id string, name string, fqdn string, ipAddress string) (storage.Server, error) {

	var server storage.Server
	err := p.db.QueryRowContext(ctx,
		`INSERT INTO servers (id, name, fqdn, ip_address, created_at, updated_at)
		VALUES ($1, $2, $3, $4, now(), now())
		RETURNING id, name, fqdn, ip_address, created_at, updated_at`,
		id, name, fqdn, ipAddress).
		Scan(&server.ID, &server.Name, &server.Fqdn, &server.IPAddress, &server.CreatedAt, &server.UpdatedAt)
	if err != nil {
		return storage.Server{}, p.processError(err)
	}

	return server, nil
}

// GetServer fetches a server by ID.
func (p *postgres) GetServer(ctx context.Context, id string) (storage.Server, error) {
	return p.getServer(ctx, p.db, id)
}

func (p *postgres) getServer(ctx context.Context, q querier, id string) (storage.Server, error) {
	var s storage.Server
	err := q.QueryRowContext(ctx,
		`SELECT s.id, s.name, s.fqdn, s.ip_address, s.updated_at, s.created_at,
		COALESCE((SELECT count(*) FROM orgs o WHERE o.server_id = s.id), 0) AS orgs_count
		FROM servers s
		WHERE s.id = $1`, id).
		Scan(&s.ID, &s.Name, &s.Fqdn, &s.IPAddress, &s.CreatedAt, &s.UpdatedAt, &s.OrgsCount)
	if err != nil {
		return storage.Server{}, p.processError(err)
	}
	return s, nil
}

// DeleteServer deletes a server from the DB.
func (p *postgres) DeleteServer(ctx context.Context, id string) (storage.Server, error) {
	var s storage.Server
	err := p.db.QueryRowContext(ctx,
		`DELETE FROM servers WHERE id = $1
		RETURNING id, name, fqdn, ip_address, created_at, updated_at`, id).
		Scan(&s.ID, &s.Name, &s.Fqdn, &s.IPAddress, &s.CreatedAt, &s.UpdatedAt)
	if err != nil {
		return storage.Server{}, p.processError(err)
	}

	return s, nil
}

// EditServer does a full update on a database server.
func (p *postgres) EditServer(ctx context.Context, id string, name string, fqdn string, ipAddress string) (storage.Server, error) {
	var s storage.Server

	err := p.db.QueryRowContext(ctx,
		`UPDATE servers
		SET name = $2, fqdn = $3, ip_address = $4, updated_at = now()
		WHERE id = $1
		RETURNING id, name, fqdn, ip_address, created_at, updated_at`,
		id, name, fqdn, ipAddress).
		Scan(&s.ID, &s.Name, &s.Fqdn, &s.IPAddress, &s.CreatedAt, &s.UpdatedAt)
	if err != nil {
		return storage.Server{}, p.processError(err)
	}

	return s, nil
}

// GetServers fetches servers from the DB as an array.
func (p *postgres) GetServers(ctx context.Context) ([]storage.Server, error) {

	var servers []storage.Server
	// TODO eventually these should be ordered
	rows, err := p.db.QueryContext(ctx,
		`SELECT s.id, s.name, s.fqdn, s.ip_address, s.updated_at, s.created_at,
		(SELECT count(*) FROM orgs o WHERE o.server_id = s.id) AS orgs_count
		FROM servers s`)

	if err != nil {
		return []storage.Server{}, p.processError(err)
	}
	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	for rows.Next() {
		server := storage.Server{}
		if err := rows.Scan(&server.ID, &server.Name, &server.Fqdn, &server.IPAddress,
			&server.CreatedAt, &server.UpdatedAt, &server.OrgsCount); err != nil {
			return nil, err // TODO: don't fail it all? handle this more gracefully?
		}
		servers = append(servers, server)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return servers, nil
}
