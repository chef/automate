package postgres

import (
	"context"

	// adapter for database/sql
	"github.com/pkg/errors"

	"github.com/chef/automate/components/infra-proxy-service/storage"
	uuid "github.com/chef/automate/lib/uuid4"
)

// StoreServer saves a server to the DB.
func (p *postgres) StoreServer(ctx context.Context, name string, description string, fqdn string, ipAddress string) (storage.Server, error) {
	return p.insertServer(ctx, name, description, fqdn, ipAddress)
}

func (p *postgres) insertServer(ctx context.Context,
	name string, description string, fqdn string, ipAddress string) (storage.Server, error) {

	var server storage.Server
	err := p.db.QueryRowContext(ctx,
		`INSERT INTO servers (id, name, description, fqdn, ip_address, created_at, updated_at)
		VALUES (uuid_generate_v4(), $1, $2, $3, $4, now(), now())
		RETURNING id, name, description, fqdn, ip_address, created_at, updated_at`,
		name, description, fqdn, ipAddress).
		Scan(&server.ID, &server.Name, &server.Description, &server.Fqdn, &server.IpAddress, &server.CreatedAt, &server.UpdatedAt)
	if err != nil {
		return storage.Server{}, p.processError(err)
	}

	return server, nil
}

// GetServer fetches a server by id.
func (p *postgres) GetServer(ctx context.Context, serverID uuid.UUID) (storage.Server, error) {
	return p.getServer(ctx, p.db, serverID)
}

func (p *postgres) getServer(ctx context.Context, q querier, serverID uuid.UUID) (storage.Server, error) {
	var s storage.Server
	err := q.QueryRowContext(ctx,
		`SELECT s.id, s.name, s.description, s.fqdn, s.ip_address, s.updated_at, s.created_at,
		COALESCE((SELECT count(*) FROM orgs o WHERE o.server_id = s.id), 0) AS orgs_count
		FROM servers s
		WHERE s.id = $1`, serverID).
		Scan(&s.ID, &s.Name, &s.Description, &s.Fqdn, &s.IpAddress, &s.CreatedAt, &s.UpdatedAt, &s.OrgsCount)
	if err != nil {
		return storage.Server{}, p.processError(err)
	}
	return s, nil
}

// GetServerByName fetches a server by name.
func (p *postgres) GetServerByName(ctx context.Context, serverName string) (storage.Server, error) {
	var s storage.Server
	err := p.db.QueryRowContext(ctx,
		`SELECT s.id, s.name, s.description, s.fqdn, s.ip_address, s.updated_at, s.created_at,
		(SELECT count(*) FROM orgs o WHERE o.server_id = s.id) AS orgs_count
		FROM servers s
		WHERE s.name = $1`,
		serverName).
		Scan(&s.ID, &s.Name, &s.Description, &s.Fqdn, &s.IpAddress, &s.CreatedAt, &s.UpdatedAt, &s.OrgsCount)
	if err != nil {
		return storage.Server{}, p.processError(err)
	}
	return s, nil
}

// DeleteServer deletes a server from the DB.
func (p *postgres) DeleteServer(ctx context.Context, serverID uuid.UUID) (storage.Server, error) {
	var s storage.Server
	err := p.db.QueryRowContext(ctx,
		`DELETE FROM servers WHERE id = $1
		RETURNING id, name, description, fqdn, ip_address, created_at, updated_at`, serverID).
		Scan(&s.ID, &s.Name, &s.Description, &s.Fqdn, &s.IpAddress, &s.CreatedAt, &s.UpdatedAt)
	if err != nil {
		return storage.Server{}, p.processError(err)
	}

	return s, nil
}

// EditServer does a full update on a database server.
func (p *postgres) EditServer(ctx context.Context, server storage.Server) (storage.Server, error) {
	var s storage.Server

	err := p.db.QueryRowContext(ctx,
		`UPDATE servers
		SET name = $2, description =$3, fqdn = $4, ip_address = $5, updated_at = now()
		WHERE id = $1
		RETURNING id, name, description, fqdn, ip_address, created_at, updated_at`,
		server.ID, server.Name, server.Description, server.Fqdn, server.IpAddress).
		Scan(&s.ID, &s.Name, &s.Description, &s.Fqdn, &s.IpAddress, &s.CreatedAt, &s.UpdatedAt)
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
		`SELECT s.id, s.name, s.description, s.fqdn, s.ip_address, s.updated_at, s.created_at,
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
		if err := rows.Scan(&server.ID, &server.Name, &server.Description, &server.Fqdn, &server.IpAddress,
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
