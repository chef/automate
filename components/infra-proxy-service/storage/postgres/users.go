package postgres

import (
	"context"

	"github.com/chef/automate/components/infra-proxy-service/storage"
)

// InsertUser saves a user to the DB.
func (p *postgres) InsertUser(ctx context.Context, id, serverID, infraServerUsername, credentialID, Connector, automateUserID string, IsServerAdmin bool) (storage.User, error) {
	var user storage.User
	err := p.db.QueryRowContext(ctx,
		`INSERT INTO users (id, server_id, infra_server_username, credential_id, connector, automate_user_id,is_server_admin,created_at,updated_at)
		VALUES ($1, $2, $3, $4, $5, $6, $7, now(), now())
		RETURNING id, server_id, infra_server_username, credential_id, connector, automate_user_id, is_server_admin, created_at, updated_at`,
		id, serverID, infraServerUsername, credentialID, Connector, automateUserID, IsServerAdmin).
		Scan(&user.ID, &user.ServerID, &user.InfraServerUsername, &user.CredentialID, &user.Connector, &user.AutomateUserID, &user.IsServerAdmin, &user.CreatedAt, &user.UpdatedAt)
	if err != nil {
		return storage.User{}, p.processError(err)
	}

	return user, nil
}

// GetUser fetches a user by ID.
func (p *postgres) GetUser(ctx context.Context, id string) (storage.User, error) {
	return p.getUser(ctx, p.db, id)
}

func (p *postgres) getUser(ctx context.Context, q querier, id string) (storage.User, error) {
	var user storage.User
	err := q.QueryRowContext(ctx,
		`SELECT u.id, u.server_id, u.infra_server_username, u.credential_id, u.connector,u.automate_user_id, u.is_server_admin,u.created_at, u.updated_at,
		FROM users u
		WHERE s.id = $1`, id).
		Scan(&user.ID, &user.ServerID, &user.InfraServerUsername, &user.CredentialID, &user.Connector, &user.AutomateUserID, &user.IsServerAdmin, &user.CreatedAt, &user.UpdatedAt)
	if err != nil {
		return storage.User{}, p.processError(err)
	}
	return user, nil
}

// EditUser does a full update on a database server
func (p *postgres) EditUser(ctx context.Context, id, serverID, infraServerUsername, credentialID, Connector, automateUserID string, IsServerAdmin bool) (storage.User, error) {
	var user storage.User

	err := p.db.QueryRowContext(ctx,
		`UPDATE users
		SET server_id = $2, infra_server_username = $3, credential_id = $4, connector = $5, automate_user_id = $6,is_server_admin= $7, created_at = now(),updated_at= now()
		WHERE id = $1
		RETURNING id, server_id, infra_server_username, credential_id, connector, automate_user_id, is_server_admin, created_at, updated_at`,
		id, serverID, infraServerUsername, credentialID, Connector, automateUserID, IsServerAdmin).
		Scan(&user.ID, &user.ServerID, &user.InfraServerUsername, &user.CredentialID, &user.Connector, &user.AutomateUserID, &user.IsServerAdmin, &user.CreatedAt, &user.UpdatedAt)
	if err != nil {
		return storage.User{}, p.processError(err)
	}

	return user, nil
}

// DeleteServer deletes a user from the DB.
func (p *postgres) DeleteUser(ctx context.Context, id string) (storage.User, error) {
	var user storage.User
	err := p.db.QueryRowContext(ctx,
		`DELETE FROM users WHERE id = $1
		RETURNING id, server_id, infra_server_username, credential_id, connector, automate_user_id, is_server_admin, created_at, updated_at`, id).
		Scan(&user.ID, &user.ServerID, &user.InfraServerUsername, &user.CredentialID, &user.Connector, &user.AutomateUserID, &user.IsServerAdmin, &user.CreatedAt, &user.UpdatedAt)
	if err != nil {
		return storage.User{}, p.processError(err)
	}

	return user, nil
}
