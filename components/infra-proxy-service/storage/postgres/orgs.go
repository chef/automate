package postgres

import (
	"context"

	"github.com/lib/pq"
	"github.com/pkg/errors"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/infra-proxy-service/storage"

	"github.com/chef/automate/lib/grpc/auth_context"
)

// StoreOrg saves an org to the DB.
func (p *postgres) StoreOrg(ctx context.Context, id string, name string, adminUser string, credentialID string, serverID string, projects []string) (storage.Org, error) {
	return p.insertOrg(ctx, id, name, adminUser, credentialID, serverID, projects)
}

func (p *postgres) insertOrg(ctx context.Context,
	id string, name string, adminUser string, credentialID string, serverID string, projects []string) (storage.Org, error) {

	// ensure we do not pass null projects to db and break the "not null"
	if len(projects) == 0 {
		projects = []string{}
	}

	// Adding the subjects if missing if gRPC calls from internal service level
	subjects := auth_context.FromContext(auth_context.FromIncomingMetadata(ctx)).Subjects
	if len(subjects) == 0 {
		subjects = []string{"tls:service:compliance-service:internal"}
	}
	_, err := p.authzClient.ValidateProjectAssignment(ctx, &authz.ValidateProjectAssignmentReq{
		Subjects:    subjects,
		OldProjects: []string{},
		NewProjects: projects,
	})
	if err != nil {
		// return error unaltered because it's already a GRPC status code
		return storage.Org{}, err
	}

	var org storage.Org
	err = p.db.QueryRowContext(ctx,
		`INSERT INTO orgs (id, name, admin_user, credential_id, server_id, projects, created_at, updated_at)
		VALUES ($1, $2, $3, $4, $5, $6, now(), now())
		RETURNING id, name, admin_user, credential_id, server_id, projects, created_at, updated_at`,
		id, name, adminUser, credentialID, serverID, pq.Array(projects)).
		Scan(&org.ID, &org.Name, &org.AdminUser, &org.CredentialID, &org.ServerID, pq.Array(&org.Projects), &org.CreatedAt, &org.UpdatedAt)
	if err != nil {
		return storage.Org{}, p.processError(err)
	}

	return org, nil
}

// GetOrg fetches an org by ID.
func (p *postgres) GetOrg(ctx context.Context, orgID string, serverID string) (storage.Org, error) {
	return p.getOrg(ctx, p.db, orgID, serverID)
}

func (p *postgres) getOrg(ctx context.Context, q querier, orgID string, serverID string) (storage.Org, error) {
	var org storage.Org
	err := q.QueryRowContext(ctx,
		`SELECT o.id, o.name, o.admin_user, o.credential_id, o.server_id, o.projects, o.updated_at, o.created_at
		FROM orgs o
		WHERE o.id = $1 AND o.server_id = $2`, orgID, serverID).
		Scan(&org.ID, &org.Name, &org.AdminUser, &org.CredentialID, &org.ServerID, pq.Array(&org.Projects), &org.CreatedAt, &org.UpdatedAt)
	if err != nil {
		return storage.Org{}, p.processError(err)
	}
	return org, nil
}

// DeleteOrg deletes an org from the DB.
func (p *postgres) DeleteOrg(ctx context.Context, orgID string, serverID string) (storage.Org, error) {
	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return storage.Org{}, p.processError(err)
	}

	var org storage.Org
	err = p.db.QueryRowContext(ctx,
		`DELETE FROM orgs WHERE id = $1 AND server_id = $2 AND projects_match(projects, $3::TEXT[])
		RETURNING id, name, admin_user, credential_id, server_id, projects, created_at, updated_at`, orgID, serverID, pq.Array(projectsFilter)).
		Scan(&org.ID, &org.Name, &org.AdminUser, &org.CredentialID, &org.ServerID, pq.Array(&org.Projects), &org.CreatedAt, &org.UpdatedAt)
	if err != nil {
		return storage.Org{}, p.processError(err)
	}

	return org, nil
}

// EditOrg does a full update on a database org.
func (p *postgres) EditOrg(ctx context.Context, id string, name string, adminUser string, serverID string, projects []string) (storage.Org, error) {
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()

	// ensure we do not pass null projects to db
	if projects == nil {
		projects = []string{}
	}

	tx, err := p.db.BeginTx(ctx, nil)
	if err != nil {
		return storage.Org{}, p.processError(err)
	}

	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return storage.Org{}, p.processError(err)
	}

	var oldProjects []string

	err = tx.QueryRowContext(ctx,
		`SELECT projects FROM orgs
		WHERE id = $1 AND server_id = $2 AND projects_match(projects, $3::TEXT[])
		FOR UPDATE;`,
		id, serverID, pq.Array(projectsFilter)).
		Scan(pq.Array(&oldProjects))
	if err != nil {
		return storage.Org{}, p.processError(err)
	}

	_, err = p.authzClient.ValidateProjectAssignment(ctx, &authz.ValidateProjectAssignmentReq{
		Subjects:        auth_context.FromContext(auth_context.FromIncomingMetadata(ctx)).Subjects,
		OldProjects:     oldProjects,
		NewProjects:     projects,
		IsUpdateRequest: true,
	})
	if err != nil {
		// return error unaltered because it's already a GRPC status code
		return storage.Org{}, err
	}

	var o storage.Org
	err = tx.QueryRowContext(ctx,
		`UPDATE orgs
		SET name = $2, admin_user = $3, projects = $4, updated_at = now()
		WHERE id = $1 AND server_id = $5 AND projects_match(projects, $6::TEXT[])
		RETURNING id, name, admin_user, credential_id, server_id, projects, created_at, updated_at;`,
		id, name, adminUser, pq.Array(projects), serverID, pq.Array(projectsFilter)).
		Scan(&o.ID, &o.Name, &o.AdminUser, &o.CredentialID, &o.ServerID, pq.Array(&o.Projects), &o.CreatedAt, &o.UpdatedAt)
	if err != nil {
		return storage.Org{}, p.processError(err)
	}

	if err := tx.Commit(); err != nil {
		return storage.Org{}, p.processError(err)
	}

	return o, nil
}

// GetOrgs fetches orgs from the DB as an array.
func (p *postgres) GetOrgs(ctx context.Context, serverID string) ([]storage.Org, error) {
	projectsFilter, err := ProjectsListFromContext(ctx)
	if err != nil {
		return []storage.Org{}, p.processError(err)
	}

	var orgs []storage.Org
	rows, err := p.db.QueryContext(ctx,
		`SELECT o.id, o.name, o.admin_user, o.credential_id, o.server_id, o.projects
		FROM orgs o
		WHERE o.server_id = $1 AND projects_match(o.projects, $2::TEXT[])`,
		serverID, pq.Array(projectsFilter))
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
		if err := rows.Scan(&org.ID, &org.Name, &org.AdminUser, &org.CredentialID, &org.ServerID, pq.Array(&org.Projects)); err != nil {
			return nil, err // TODO: don't fail it all? handle this more gracefully?
		}
		orgs = append(orgs, org)
	}
	if err := rows.Err(); err != nil {
		return nil, errors.Wrap(err, "error retrieving result rows")
	}
	return orgs, nil
}

func (p *postgres) TouchOrg(ctx context.Context, id string, serverID string) (storage.Org, error) {
	var o storage.Org
	err := p.db.QueryRowContext(ctx,
		`UPDATE orgs
		SET updated_at = now()
		WHERE id = $1 AND server_id = $2
		RETURNING id, name, admin_user, credential_id, server_id, projects, created_at, updated_at;`,
		id, serverID).
		Scan(&o.ID, &o.Name, &o.AdminUser, &o.CredentialID, &o.ServerID, pq.Array(&o.Projects), &o.CreatedAt, &o.UpdatedAt)
	if err != nil {
		return storage.Org{}, p.processError(err)
	}
	return o, nil
}

// ProjectsListFromContext returns the project list from the context.
// In the case that the project list was ["*"], we return an empty list,
// since we do not wish to filter on projects.
func ProjectsListFromContext(ctx context.Context) ([]string, error) {
	projectsFilter, err := auth_context.ProjectsFromIncomingContext(ctx)
	if err != nil {
		return nil, err
	}
	if auth_context.AllProjectsRequested(projectsFilter) {
		projectsFilter = []string{}
	}
	return projectsFilter, nil
}
