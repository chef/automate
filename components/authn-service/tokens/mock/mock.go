// +build !prod

package mock

import (
	"context"
	"fmt"
	"time"

	"go.uber.org/zap"

	pg "github.com/chef/automate/components/authn-service/tokens/pg"
	tokens "github.com/chef/automate/components/authn-service/tokens/types"
	tutil "github.com/chef/automate/components/authn-service/tokens/util"
	"github.com/chef/automate/lib/tls/certs"
	uuid "github.com/chef/automate/lib/uuid4"
)

// Config is used for configuring mock adapters
type Config struct {
	Tokens []*tokens.Token `json:"tokens"`
}

type mock struct {
	tokens []*tokens.Token
}

// Open initializes the mock adapter
func (cfg *Config) Open(_ *certs.ServiceCerts, logger *zap.Logger) (tokens.Storage, error) {
	return &mock{tokens: cfg.Tokens}, nil
}

func (m *mock) GetTokens(ctx context.Context) ([]*tokens.Token, error) {
	tokensToRet := []*tokens.Token{}
	for _, tok := range m.tokens {
		if projectsIntersect(ctx, tok) {
			tokensToRet = append(tokensToRet, tok)
		}
	}
	return tokensToRet, nil
}

func (m *mock) GetTokenIDWithValue(ctx context.Context, value string) (string, error) {
	for i, t := range m.tokens {
		if (t.Value == value) && t.Active && projectsIntersect(ctx, t) {
			return m.tokens[i].ID, nil
		}
	}
	return "", &tokens.NotFoundError{}
}

func (m *mock) GetToken(ctx context.Context, id string) (*tokens.Token, error) {
	for i, t := range m.tokens {
		if t.ID == id && projectsIntersect(ctx, t) {
			return m.tokens[i], nil
		}
	}
	return nil, &tokens.NotFoundError{}
}

func (m *mock) CreateToken(_ context.Context, id, description string,
	active bool, projects []string) (*tokens.Token, error) {

	if id == "" {
		id = uuid.Must(uuid.NewV4()).String()
	}
	return m.createTokenWithValue(mockToken(id), description, active, id, projects)
}

func (m *mock) CreateTokenWithValue(_ context.Context,
	id, value, description string, active bool, projects []string) (*tokens.Token, error) {
	if id == "" {
		id = uuid.Must(uuid.NewV4()).String()
	}
	if err := tutil.IsValidToken(value); err != nil {
		return nil, err
	}
	return m.createTokenWithValue(value, description, active, id, projects)
}

func (m *mock) CreateLegacyTokenWithValue(_ context.Context, value string) (*tokens.Token, error) {
	if err := tutil.IsValidLegacyToken(value); err != nil {
		return nil, err
	}
	return m.createTokenWithValue(value, tokens.LegacyTokenDescription,
		true, uuid.Must(uuid.NewV4()).String(), []string{})
}

func (m *mock) createTokenWithValue(value string,
	description string, active bool, id string, projects []string) (*tokens.Token, error) {

	if len(projects) == 0 {
		projects = []string{}
	}

	now := time.Now().UTC()

	tNew := tokens.Token{
		Description: description,
		Value:       value,
		Active:      active,
		Created:     now,
		Updated:     now,
		ID:          id,
		Projects:    projects,
	}

	m.tokens = append(m.tokens, &tNew)
	return &tNew, nil
}

func (m *mock) DeleteToken(ctx context.Context, id string) error {
	found := false
	newTokens := []*tokens.Token{}
	for i, t := range m.tokens {
		if t.ID == id && projectsIntersect(ctx, t) {
			found = true
			continue
		}
		newTokens = append(newTokens, m.tokens[i])
	}
	m.tokens = newTokens
	if !found {
		return &tokens.NotFoundError{}
	}
	return nil
}

func (m *mock) UpdateToken(ctx context.Context,
	id, description string,
	active bool,
	projects []string) (*tokens.Token, error) {

	t, err := m.GetToken(ctx, id)
	if err != nil {
		return nil, err
	}
	if t == nil {
		return nil, &tokens.NotFoundError{} // not deleted, because not found
	}

	now := time.Now().UTC()
	var newToken *tokens.Token
	newTokens := []*tokens.Token{}
	for _, t := range m.tokens {
		t := t
		if t.ID == id {
			t.Active = active
			t.Updated = now
			if description != "" {
				t.Description = description
			}
			if len(projects) != 0 {
				t.Projects = projects
			} else if projects == nil {
				t.Projects = []string{}
			}
			newToken = t
		}
		newTokens = append(newTokens, t)
	}
	m.tokens = newTokens
	return newToken, nil
}

func mockToken(id string) string {
	return fmt.Sprintf("%v-token", id)
}

func projectsIntersect(ctx context.Context, token *tokens.Token) bool {
	projectsFilter, err := pg.ProjectsListFromContext(ctx)
	if err != nil {
		return false
	}

	if len(projectsFilter) == 0 {
		return true
	}

	tokenProjects := token.Projects
	if len(tokenProjects) == 0 {
		tokenProjects = []string{"(unassigned)"}
	}

	for _, projectFilter := range projectsFilter {
		for _, project := range tokenProjects {
			if projectFilter == project {
				return true
			}
		}
	}
	return false
}

func (m *mock) Reset() {
	m.tokens = []*tokens.Token{}
}
