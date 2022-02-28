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
		u.connector,
		u.automate_user_id, 
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
			return nil, err
		}
		users = append(users, user)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return users, nil
}

//GetAutomateOrgUsers: .Fetches all the users for a org
func (p *postgres) GetAutomateOrgUsers(ctx context.Context, orgId string) ([]storage.OrgUser, error) {
	var orgUsers []storage.OrgUser
	rows, err := p.db.QueryContext(ctx,
		`select ou.org_id,ou.is_admin,u.infra_server_username  from org_users ou join users u on u.id=ou.user_id  join orgs o on o.id=ou.org_id where org_id=$1`, orgId)
	if err != nil {
		return []storage.OrgUser{}, p.processError(err)
	}
	defer func() {
		if err := rows.Close(); err != nil {
			p.logger.Warnf("Failed to close database rows for org users: %s", err.Error())
		}
	}()

	for rows.Next() {
		orgUser := storage.OrgUser{}
		if err := rows.Scan(&orgUser.OrgId, &orgUser.IsAdmin, &orgUser.InfraServerUsername); err != nil {
			return nil, err
		}
		orgUsers = append(orgUsers, orgUser)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return orgUsers, nil
}

func (p *postgres) GetUsers(ctx context.Context, serverID string) ([]storage.User, error) {
	return p.getUsers(ctx, p.db, serverID)
}

func (p *postgres) getUsers(ctx context.Context, q querier, serverID string) ([]storage.User, error) {
	var users []storage.User
	rows, err := p.db.QueryContext(ctx, `SELECT u.id, u.server_id, u.infra_server_username, u.credential_id, u.connector,
		u.automate_user_id, u.is_server_admin,u.created_at, u.updated_at FROM users u
		WHERE u.server_id = $1`, serverID)
	if err != nil {
		return nil, p.processError(err)
	}

	for rows.Next() {
		user := storage.User{}
		if err := rows.Scan(&user.ID, &user.ServerID, &user.InfraServerUsername, &user.CredentialID, &user.Connector,
			&user.AutomateUserID, &user.IsServerAdmin, &user.CreatedAt, &user.UpdatedAt); err != nil {
			return nil, err
		}
		users = append(users, user)

	}
	return users, nil
}

// StoreOrgUserAssociation: Inserts an entry of org users
func (p *postgres) StoreOrgUserAssociation(ctx context.Context, serverID, orgID, username string, isAdmin bool) (storage.OrgUser, error) {
	return p.insertOrgUserAssociation(ctx, serverID, orgID, username, isAdmin)
}

func (p *postgres) insertOrgUserAssociation(ctx context.Context, serverID, orgID, username string, isAdmin bool) (storage.OrgUser, error) {
	var orgUser storage.OrgUser
	nowTime := time.Now()
	err := p.db.QueryRowContext(ctx,
		`INSERT INTO org_users (
		org_id, user_id, 
		is_admin, 
		created_at,updated_at)
		VALUES ($1, SELECT id from users where infra_server_username=$2 and server_id=$3, $4, $5, $5)
		RETURNING id, org_id, user_id, is_admin, created_at, updated_at`,
		orgID, username, serverID, isAdmin, nowTime).
		Scan(&orgUser.ID, &orgUser.OrgId, &orgUser.UserID, &orgUser.IsAdmin, &orgUser.CreatedAt, &orgUser.UpdatedAt)
	if err != nil {
		return storage.OrgUser{}, p.processError(err)
	}
	return orgUser, nil
}

// EditOrgUserAssociation: Updates an entry of org users
func (p *postgres) EditOrgUserAssociation(ctx context.Context, serverID, orgID, username string, isAdmin bool) (storage.OrgUser, error) {
	return p.updateOrgUserAssociation(ctx, serverID, orgID, username, isAdmin)
}

func (p *postgres) updateOrgUserAssociation(ctx context.Context, serverID, orgID, username string, isAdmin bool) (storage.OrgUser, error) {
	var orgUser storage.OrgUser
	nowTime := time.Now()
	err := p.db.QueryRowContext(ctx,
		`UPDATE org_users SET 
		is_admin = $2, 
		updated_at= $5
		WHERE org_id=$1 and user_id = SELECT id FROM users u WHERE u.infra_server_username=$3 and u.server_id=$4
		RETURNING id, org_id, user_id, is_admin, created_at, updated_at`,
		orgID, isAdmin, username, serverID, nowTime).
		Scan(&orgUser.ID, &orgUser.OrgId, &orgUser.UserID, &orgUser.IsAdmin, &orgUser.CreatedAt, &orgUser.UpdatedAt)
	if err != nil {
		return storage.OrgUser{}, p.processError(err)
	}

	return orgUser, nil
}

// DeleteOrgUserAssociation: Deletes an entry from org users
func (p *postgres) DeleteOrgUserAssociation(ctx context.Context, serverID, orgID, username string, isAdmin bool) (storage.OrgUser, error) {
	return p.deleteOrgUserAssociation(ctx, serverID, orgID, username, isAdmin)
}

func (p *postgres) deleteOrgUserAssociation(ctx context.Context, serverID, orgID, username string, isAdmin bool) (storage.OrgUser, error) {
	var orgUser storage.OrgUser
	err := p.db.QueryRowContext(ctx,
		`DELETE FROM org_users 
		WHERE org_id=$1 and user_id = SELECT id FROM users u WHERE u.infra_server_username=$2 and u.server_id=$3
		RETURNING id, org_id, user_id, is_admin, created_at, updated_at`,
		orgID, username, serverID).
		Scan(&orgUser.ID, &orgUser.OrgId, &orgUser.UserID, &orgUser.IsAdmin, &orgUser.CreatedAt, &orgUser.UpdatedAt)
	if err != nil {
		return storage.OrgUser{}, p.processError(err)
	}

	return orgUser, nil
}
