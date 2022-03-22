package postgres

import (
	"context"
	"time"

	"github.com/chef/automate/components/infra-proxy-service/storage"
	"github.com/pkg/errors"
)

// InsertUser saves a user to the DB.
func (p *postgres) InsertUser(ctx context.Context, user storage.User) (storage.User, error) {
	var returnUser storage.User
	nowTime := time.Now()
	err := p.db.QueryRowContext(ctx,
		`INSERT INTO users (
		server_id, 
		infra_server_username, 
		connector, 
		automate_user_id,
		first_name,
		last_name,
		email,
		middle_name,
		display_name,
		created_at,updated_at)
		VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $10 )
		RETURNING id, server_id, infra_server_username, connector, automate_user_id, first_name, last_name, email, middle_name, display_name, created_at, updated_at`,
		user.ServerID, user.InfraServerUsername, user.Connector, user.AutomateUserID, user.FirstName, user.LastName, user.Email, user.MiddleName, user.DisplayName, nowTime).
		Scan(&returnUser.ID, &returnUser.ServerID, &returnUser.InfraServerUsername, &returnUser.Connector, &returnUser.AutomateUserID, &returnUser.FirstName, &returnUser.LastName, &returnUser.Email, &returnUser.MiddleName, &returnUser.DisplayName, &returnUser.CreatedAt, &returnUser.UpdatedAt)
	if err != nil {
		return storage.User{}, p.processError(err)
	}

	return returnUser, nil
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
		u.connector,
		u.automate_user_id, 
		u.first_name,
		u.last_name,
		u.email,
		u.middle_name,
		u.display_name,
		u.created_at, u.updated_at
		FROM users u
		WHERE u.id = $1`, id).
		Scan(&user.ID, &user.ServerID, &user.InfraServerUsername, &user.Connector, &user.AutomateUserID, &user.FirstName, &user.LastName, &user.Email, &user.MiddleName, &user.DisplayName, &user.CreatedAt, &user.UpdatedAt)
	if err != nil {
		return storage.User{}, p.processError(err)
	}
	return user, nil
}

// EditUser does a full update on a database server
func (p *postgres) EditUser(ctx context.Context, user storage.User) (storage.User, error) {
	var returnUser storage.User
	nowTime := time.Now()
	err := p.db.QueryRowContext(ctx,
		`UPDATE users SET 
		server_id = $3, 
		infra_server_username = $4, 
		connector = $5, 
		automate_user_id = $6,
		first_name = $7,
		last_name = $8,
		email = $9,
		middle_name = $10,
		display_name = $11,
		updated_at= $12
		WHERE server_id = $1 AND infra_server_username = $2
		RETURNING id, server_id, infra_server_username, connector, automate_user_id, first_name, last_name, email, middle_name, display_name, created_at, updated_at`,
		user.ServerID, user.InfraServerUsername, user.ServerID, user.InfraServerUsername, user.Connector, user.AutomateUserID, user.FirstName, user.LastName, user.Email, user.MiddleName, user.DisplayName, nowTime).
		Scan(&returnUser.ID, &returnUser.ServerID, &returnUser.InfraServerUsername, &returnUser.Connector, &returnUser.AutomateUserID, &returnUser.FirstName, &returnUser.LastName, &returnUser.Email, &returnUser.MiddleName, &returnUser.DisplayName, &returnUser.CreatedAt, &returnUser.UpdatedAt)
	if err != nil {
		return storage.User{}, p.processError(err)
	}

	return returnUser, nil
}

// DeleteServer deletes a user from the DB.
func (p *postgres) DeleteUser(ctx context.Context, user storage.User) (storage.User, error) {
	var returnUser storage.User
	err := p.db.QueryRowContext(ctx,
		`DELETE FROM users WHERE server_id = $1 AND infra_server_username = $2
		RETURNING id, server_id, infra_server_username, connector, automate_user_id, first_name, last_name, email, middle_name, display_name, created_at, updated_at`,
		user.ServerID, user.InfraServerUsername).
		Scan(&returnUser.ID, &returnUser.ServerID, &returnUser.InfraServerUsername, &returnUser.Connector, &returnUser.AutomateUserID, &returnUser.FirstName, &returnUser.LastName, &returnUser.Email, &returnUser.MiddleName, &returnUser.DisplayName, &returnUser.CreatedAt, &returnUser.UpdatedAt)
	if err != nil {
		return storage.User{}, p.processError(err)
	}

	return returnUser, nil
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
		u.first_name,
		u.last_name,
		u.email,
		u.middle_name,
		u.display_name,
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
		if err := rows.Scan(&user.ID, &user.ServerID, &user.InfraServerUsername, &user.Connector,
			&user.AutomateUserID, &user.FirstName, &user.LastName, &user.Email, &user.MiddleName, &user.DisplayName, &user.CreatedAt, &user.UpdatedAt); err != nil {
			return nil, err
		}
		users = append(users, user)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return users, nil
}

//GetAutomateInfraOrgUsers: .Fetches all the users for a org
func (p *postgres) GetAutomateInfraOrgUsers(ctx context.Context, serverId, orgId string) ([]storage.OrgUser, error) {
	var orgUsers []storage.OrgUser
	rows, err := p.db.QueryContext(ctx,

		`select ou.user_id,u.server_id,u.infra_server_username,u.connector,
			u.automate_user_id,u.first_name,u.last_name,u.email,u.middle_name,u.display_name,
			ou.org_id,ou.is_admin
			from users u 
			join org_users ou on u.id=ou.user_id
			join orgs o on o.id=ou.org_id
			and ou.org_id=$1 and u.server_id=$2;`, orgId, serverId)
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
		if err := rows.Scan(&orgUser.UserID, &orgUser.ServerID, &orgUser.InfraServerUsername,
			&orgUser.Connector, &orgUser.AutomateUserID, &orgUser.FirstName, &orgUser.LastName,
			&orgUser.EmailID, &orgUser.MiddleName, &orgUser.DisplayName, &orgUser.OrgID, &orgUser.IsAdmin); err != nil {
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
		if err := rows.Scan(&user.ID, &user.ServerID, &user.InfraServerUsername, &user.Connector,
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
		VALUES ($1, (SELECT id from users where infra_server_username=$2 and server_id=$3), $4, $5, $5)
		RETURNING id, org_id, user_id, is_admin, created_at, updated_at`,
		orgID, username, serverID, isAdmin, nowTime).
		Scan(&orgUser.ID, &orgUser.OrgID, &orgUser.UserID, &orgUser.IsAdmin, &orgUser.CreatedAt, &orgUser.UpdatedAt)
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
		is_admin = $1, 
		updated_at= $2
		WHERE org_id=$3 and user_id = (SELECT id FROM users u WHERE u.infra_server_username=$4 and u.server_id=$5)
		RETURNING id, org_id, user_id, is_admin, created_at, updated_at`,
		isAdmin, nowTime, orgID, username, serverID).
		Scan(&orgUser.ID, &orgUser.OrgID, &orgUser.UserID, &orgUser.IsAdmin, &orgUser.CreatedAt, &orgUser.UpdatedAt)
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
		WHERE org_id=$1 and user_id = (SELECT id FROM users u WHERE u.infra_server_username=$2 and u.server_id=$3)
		RETURNING id, org_id, user_id, is_admin, created_at, updated_at`,
		orgID, username, serverID).
		Scan(&orgUser.ID, &orgUser.OrgID, &orgUser.UserID, &orgUser.IsAdmin, &orgUser.CreatedAt, &orgUser.UpdatedAt)
	if err != nil {
		return storage.OrgUser{}, p.processError(err)
	}
	return orgUser, nil
}
