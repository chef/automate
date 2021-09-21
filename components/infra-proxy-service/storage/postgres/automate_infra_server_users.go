package postgres

import (
	"context"
	"time"

	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/pkg/errors"
)

// InsertUser saves a user to the DB.
func (p *postgres) InsertUser(ctx context.Context, id, serverId, infraServerUsername, credentialId, connector, automateUserId string, isServerAdmin bool) (storage.User, error) {
	var user storage.User
	nowTime := time.Now()
	err := p.db.QueryRowContext(ctx,
		`INSERT INTO users (
		id, server_id, 
		infra_server_username, 
		credential_id, 
		connector, 
		automate_user_id,
		is_server_admin,
		created_at,updated_at)
		VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $8)
		RETURNING id, server_id, infra_server_username, credential_id, connector, automate_user_id, is_server_admin, created_at, updated_at`,
		id, serverId, infraServerUsername, credentialId, connector, automateUserId, isServerAdmin, nowTime).
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
		`SELECT 
		u.id, u.server_id, 
		u.infra_server_username, 
		u.credential_id, 
		u.connector,
		u.automate_user_id, 
		u.is_server_admin,
		u.created_at, u.updated_at
		FROM users u
		WHERE u.id = $1`, id).
		Scan(&user.ID, &user.ServerID, &user.InfraServerUsername, &user.CredentialID, &user.Connector, &user.AutomateUserID, &user.IsServerAdmin, &user.CreatedAt, &user.UpdatedAt)
	if err != nil {
		return storage.User{}, p.processError(err)
	}
	return user, nil
}

// EditUser does a full update on a database server
func (p *postgres) EditUser(ctx context.Context, id, serverId, infraServerUsername, credentialId, connector, automateUserId string, isServerAdmin bool) (storage.User, error) {
	var user storage.User
	nowTime := time.Now()
	err := p.db.QueryRowContext(ctx,
		`UPDATE users SET 
		server_id = $2, 
		infra_server_username = $3, 
		credential_id = $4, 
		connector = $5, 
		automate_user_id = $6,
		is_server_admin= $7, 
		updated_at= $8
		WHERE id = $1
		RETURNING id, server_id, infra_server_username, credential_id, connector, automate_user_id, is_server_admin, created_at, updated_at`,
		id, serverId, infraServerUsername, credentialId, connector, automateUserId, isServerAdmin, nowTime).
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
		RETURNING id, server_id, infra_server_username, credential_id, connector, automate_user_id, is_server_admin, created_at, updated_at`,
		id).
		Scan(&user.ID, &user.ServerID, &user.InfraServerUsername, &user.CredentialID, &user.Connector, &user.AutomateUserID, &user.IsServerAdmin, &user.CreatedAt, &user.UpdatedAt)
	if err != nil {
		return storage.User{}, p.processError(err)
	}

	return user, nil
}

//GetAutomateInfraServerUsers: fetches server users from the DB as an array.
func (p *postgres) GetAutomateInfraServerUsers(ctx context.Context, serverId string) ([]storage.User, error) {
	var users []storage.User
	rows, err := p.db.QueryContext(ctx,
		`SELECT 
		u.id, u.server_id, 
		u.infra_server_username, 
		u.credential_id, 
		u.connector,
		u.automate_user_id, 
		u.is_server_admin,
		u.created_at, u.updated_at
		FROM users u
		WHERE u.server_id = $1`, serverId)
	if err != nil {
		return []storage.User{}, p.processError(err)
	}
	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("failed to close db rows: %s", err.Error())
		}
	}()

	for rows.Next() {
		user := storage.User{}
		if err := rows.Scan(&user.ID, &user.ServerID, &user.InfraServerUsername, &user.CredentialID, &user.Connector,
			&user.AutomateUserID, &user.IsServerAdmin, &user.CreatedAt, &user.UpdatedAt); err != nil {
			return nil, err // TODO: continue or return??
		}
		users = append(users, user)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return users, nil
}
