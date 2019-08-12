package server

import (
	"context"
	"encoding/json"
	"fmt"
	"os"

	"github.com/pkg/errors"
	"go.uber.org/zap"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/grpclog"
	"google.golang.org/grpc/status"

	authz "github.com/chef/automate/api/interservice/authz/common"
	teams "github.com/chef/automate/api/interservice/teams/v1"
	"github.com/chef/automate/components/local-user-service/password"
	"github.com/chef/automate/components/local-user-service/users"
	"github.com/chef/automate/lib/grpc/health"
	"github.com/chef/automate/lib/grpc/secureconn"
	wrap "github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/stringutils"
	"github.com/chef/automate/lib/tls/certs"
	uuid "github.com/chef/automate/lib/uuid4"
)

// Config holds the server's configuration options.
type Config struct {
	A1UserData      string
	A1UserRolesData string
	Logger          *zap.Logger
	Users           UsersConfig
	ServiceCerts    *certs.ServiceCerts
	AuthzAddress    string
}

// Server is the top level object.
type Server struct {
	users           users.Adapter
	logger          *zap.Logger
	validator       *password.Validator
	connFactory     *secureconn.Factory
	health          *health.Service
	teamsClient     teams.TeamsV1Client
	a1UserData      string
	a1UserRolesData string
	authzClient     authz.SubjectPurgeClient
}

// NewServer constructs a server from the provided config.
func NewServer(ctx context.Context, c Config) (*Server, error) {
	return newServer(ctx, c)
}

func newServer(ctx context.Context, c Config) (*Server, error) {
	var usrs users.Adapter
	var err error

	// Users shouldn't see this, but gives you a clearer error message if you
	// don't configure things correctly in testing.
	if c.ServiceCerts == nil {
		return nil, fmt.Errorf("config is missing required TLS settings")
	}

	factory := secureconn.NewFactory(*c.ServiceCerts)

	if c.Users != nil {
		usrs, err = c.Users.Open(c.Logger, c.ServiceCerts)
		if err != nil {
			return nil, errors.Wrap(err, "initialize users adapter")
		}
	} else {
		return nil, errors.New("no users adapter configured")
	}

	val, err := password.NewValidator()
	if err != nil {
		return nil, errors.Wrap(err, "initialize password validator")
	}

	authzConn, err := factory.DialContext(ctx, "authz-service", c.AuthzAddress)
	if err != nil {
		return nil, errors.Wrapf(err, "failed to dial authz-service at (%s)", c.AuthzAddress)
	}

	s := &Server{
		logger:          c.Logger,
		users:           usrs,
		validator:       val,
		connFactory:     secureconn.NewFactory(*c.ServiceCerts),
		teamsClient:     teams.NewTeamsV1Client(authzConn),
		a1UserData:      c.A1UserData,
		a1UserRolesData: c.A1UserRolesData,
		authzClient:     authz.NewSubjectPurgeClient(authzConn),
		health:          health.NewService(),
	}

	// make grpc-go log through zap
	grpclog.SetLoggerV2(wrap.WrapZapGRPC(s.logger))

	return s, nil
}

func (s *Server) MigrateA1Users(ctx context.Context) error {
	f, err := os.Open(s.a1UserData)
	if err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		return errors.Wrapf(err, "read a1 user data file %q", s.a1UserData)
	}
	data := []a1User{}
	err = json.NewDecoder(f).Decode(&data)
	if err != nil {
		return errors.Wrapf(err, "decode a1 user data file %q", s.a1UserData)
	}

	for _, a1User := range data {
		if a1User.HashedPass == "" || a1User.Name == "" {
			s.logger.Error(fmt.Sprintf("incomplete record %v", a1User))
			return errors.New("failed to import all A1 users")
		}

		id, err := uuid.NewV4()
		if err != nil {
			return errors.Wrapf(err, "create ID for user %q", a1User.Name)
		}
		u := users.UserWithHashedPass{
			ShowUser: users.ShowUser{
				ID:    id.String(),
				Name:  a1User.DisplayName(),
				Email: a1User.Name,
			},
			HashedPass: []byte(a1User.HashedPass),
		}
		_, err = s.users.CreateUserWithHashedPass(ctx, u)
		if err != nil {
			if _, ok := err.(*users.AlreadyExistsError); ok {
				s.logger.Info(fmt.Sprintf("A1 user %q already exists (skipping)", u.Name))
				continue
			}
			return errors.Wrapf(err, "create migrated user %q", u.Name)
		}
		s.logger.Info(fmt.Sprintf("migrated A1 user %q", u.Name))
	}
	s.logger.Info("A1 users migration finished")
	err = os.Rename(s.a1UserData, s.a1UserData+".done")
	return errors.Wrapf(err, "rename A1 users data file %q", s.a1UserData)
}

func (s *Server) MigrateA1UserRoles(ctx context.Context) error {
	f, err := os.Open(s.a1UserRolesData)
	if err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		return errors.Wrapf(err, "read a1 user roles data file %q", s.a1UserRolesData)
	}
	data := []a1UserRole{}
	err = json.NewDecoder(f).Decode(&data)
	if err != nil {
		return errors.Wrapf(err, "decode a1 user roles data file %q", s.a1UserRolesData)
	}

	var usersToAdd []string
	for _, role := range data {
		s.logger.Info(fmt.Sprintf("migrating user role %v", role))
		if role.Name == "builder" {
			s.logger.Info(fmt.Sprintf("skipping builder user (%v)", role))
			continue
		}

		var migrated bool
		if stringutils.SliceContains(role.Roles, "admin") {
			usersToAdd = append(usersToAdd, role.Name)
			migrated = true
		}
		if !migrated {
			s.logger.Info(fmt.Sprintf("user %q has no admin role, skipping", role.Name))
		}
	}

	if len(usersToAdd) == 0 {
		s.logger.Info("no user roles to migrate")
		return s.cleanupA1UserRolesData()
	}

	adminsID, err := s.ensureAdminsTeamExists(ctx)
	if err != nil {
		return errors.Wrap(err, "failed to ensure admins team existence")
	}

	userIDsToAdd := make([]string, len(usersToAdd))
	for i, name := range usersToAdd {
		u, err := s.users.GetUser(ctx, name)
		if err != nil {
			return errors.Wrapf(err, "failed to lookup user %q", name)
		}
		userIDsToAdd[i] = u.ID
	}
	_, err = s.teamsClient.AddUsers(ctx, &teams.AddUsersReq{Id: adminsID, UserIds: userIDsToAdd})
	if err != nil {
		return errors.Wrap(err, "failed to add users to admins team")
	}

	s.logger.Info("A1 user roles migration finished")
	return s.cleanupA1UserRolesData()
}

func (s *Server) cleanupA1UserRolesData() error {
	err := os.Rename(s.a1UserRolesData, s.a1UserRolesData+".done")
	return errors.Wrapf(err, "rename A1 user roles data file %q", s.a1UserRolesData)
}

func (s *Server) ensureAdminsTeamExists(ctx context.Context) (string, error) {
	resp, err := s.teamsClient.GetTeamByName(ctx, &teams.GetTeamByNameReq{Name: "admins"})
	if err != nil {
		if status.Convert(err).Code() != codes.NotFound {
			return "", errors.Wrap(err, "failed to fetch team \"admins\"")
		}
		// create "admins" team
		resp, err := s.teamsClient.CreateTeam(ctx, &teams.CreateTeamReq{Name: "admins", Description: "TODO"})
		if err != nil {
			return "", errors.Wrap(err, "failed to create team \"admins\"")
		}
		return resp.Team.Id, nil
	}

	return resp.Team.Id, nil
}

type a1User struct {
	Name       string `json:"name"`
	FirstName  string `json:"first_name"`
	LastName   string `json:"last_name"`
	HashedPass string `json:"hashed_pass"`
}

type a1UserRole struct {
	Name  string   `json:"name"`
	Roles []string `json:"roles"`
}

func (a a1User) DisplayName() string {
	switch {
	case a.FirstName != "" && a.LastName != "":
		return fmt.Sprintf("%s %s", a.FirstName, a.LastName)
	case a.FirstName != "":
		return a.FirstName
	case a.LastName != "":
		return a.LastName
	default:
		return a.Name
	}
}
