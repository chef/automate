package dex

import (
	"context"
	"errors"
	"net"
	"testing"

	"github.com/dexidp/dex/api"
	"github.com/kylelemons/godebug/pretty"
	"go.uber.org/zap"
	"golang.org/x/crypto/bcrypt"

	"github.com/chef/automate/components/local-user-service/users"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/test/helpers"
)

type dexAPI struct {
	usrs []*api.Password
}

func TestDexUsersAdapter(t *testing.T) {
	serviceCerts := helpers.LoadDevCerts(t, "local-user-service")

	// We need to load the dex certs for the dex server because
	// local user service checks the name the server gives back
	// matches automate-dex, which is in the automate-dex cert.
	dexCerts := helpers.LoadDevCerts(t, "automate-dex")
	dexConnFactory := secureconn.NewFactory(*dexCerts)

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	dexGRPC := dexAPI{
		usrs: []*api.Password{
			{
				Username: "alice",
				Email:    "alice@chef.io",
				UserId:   "alice",
				Hash:     []byte(`alice_password`),
			},
			{
				Username: "bob",
				Email:    "bob@chef.io",
				UserId:   "bob",
				Hash:     []byte(`bob_password`),
			},
		},
	}

	list, err := net.Listen("tcp", "127.0.0.1:0") // :0 chooses (free) port randomly
	if err != nil {
		t.Fatal(err)
	}
	grpcHost := list.Addr().String() // here we find out which port was chosen

	s := dexConnFactory.NewServer()
	api.RegisterDexServer(s, &dexGRPC)
	defer s.Stop()
	// spawn a goroutine
	go s.Serve(list)
	// setup dex adapter to connect to grpcHost
	adapterConfig := &Config{
		GRPCHost: grpcHost,
	}

	logger, err := zap.NewDevelopment()
	if err != nil {
		t.Fatal(err)
	}

	adp, err := adapterConfig.Open(logger, serviceCerts)
	if err != nil {
		t.Fatalf("opening adapter: %s", err)
	}

	t.Run("list users", func(t *testing.T) {
		usrs, err := adp.GetUsers(ctx)
		if err != nil {
			t.Error(err)
		}
		if i := len(usrs); i != 2 {
			t.Errorf("expected two users, got %d", i)
		}
	})

	t.Run("get user", func(t *testing.T) {
		fetchedUser, err := adp.GetUser(ctx, "alice@chef.io")
		if err != nil {
			t.Error(err)
		}

		expectedUser := users.ShowUser{
			ID:    "alice",
			Email: "alice@chef.io",
			Name:  "alice",
		}

		if diff := pretty.Compare(expectedUser, fetchedUser); diff != "" {
			t.Errorf("updated user: got!=want: %s", diff)
		}
	})

	t.Run("fail to get nonexistent user", func(t *testing.T) {
		fetchedUser, err := adp.GetUser(ctx, "nobody")

		if err == nil {
			t.Errorf("expected 'user not found', got %s", err)
		}

		if fetchedUser != nil {
			t.Errorf("expected nil, got %s", fetchedUser)
		}
	})

	t.Run("get password", func(t *testing.T) {
		fetchedUser, err := adp.GetPassword(ctx, "alice@chef.io")
		if err != nil {
			t.Error(err)
		}

		expectedUser := users.UserWithHashedPass{
			ShowUser: users.ShowUser{
				ID:    "alice",
				Email: "alice@chef.io",
				Name:  "alice",
			},
			HashedPass: []byte(`alice_password`),
		}

		if diff := pretty.Compare(expectedUser, fetchedUser); diff != "" {
			t.Errorf("updated user: got!=want: %s", diff)
		}
	})

	t.Run("fail to get nonexistent password", func(t *testing.T) {
		fetchedUser, err := adp.GetPassword(ctx, "nobody")

		if err == nil {
			t.Errorf("expected 'user not found', got %s", err)
		}

		if fetchedUser != nil {
			t.Errorf("expected nil, got %s", fetchedUser)
		}
	})

	t.Run("add user", func(t *testing.T) {
		user := users.User{
			ID:       "abc",
			Email:    "test@email.com",
			Name:     "test jr.",
			Password: "testing!!",
		}

		createdUser, err := adp.CreateUser(ctx, user)
		if err != nil {
			t.Fatal(err)
		}

		expectedUser := users.ShowUser{
			ID:    "abc",
			Email: "test@email.com",
			Name:  "test jr.",
		}

		if diff := pretty.Compare(expectedUser, createdUser); diff != "" {
			t.Errorf("updated user: got!=want: %s", diff)
		}
	})

	t.Run("fail to add user that already exists", func(t *testing.T) {
		user := users.User{
			ID:       "ecila",
			Email:    "alice@chef.io",
			Name:     "Ecila",
			Password: "someWord",
		}

		createdUser, err := adp.CreateUser(ctx, user)
		if err == nil {
			t.Errorf("expected 'User already exists', got %s", err)
		}

		if createdUser != nil {
			t.Errorf("expected nil, got %s", createdUser)
		}
	})

	t.Run("add user with hashed password", func(t *testing.T) {
		user := users.UserWithHashedPass{
			ShowUser: users.ShowUser{
				ID:    "abc123",
				Email: "test@email.com",
				Name:  "test jr.",
			},
			HashedPass: []byte("$2a$12$m0PNEpj3mKKJpNmUFgabz.g9apd5STp18TZlHpcoSE2.rJytUibii"),
		}

		createdUser, err := adp.CreateUserWithHashedPass(ctx, user)
		if err != nil {
			t.Fatal(err)
		}

		expectedUser := users.ShowUser{
			ID:    "abc123",
			Email: "test@email.com",
			Name:  "test jr.",
		}

		if diff := pretty.Compare(expectedUser, createdUser); diff != "" {
			t.Errorf("updated user: got!=want: %s", diff)
		}
	})

	t.Run("update user's password", func(t *testing.T) {
		pass, _ := adp.HashPassword("someWord")
		// update a seed user
		user := users.UserWithHashedPass{
			ShowUser: users.ShowUser{
				ID:    "alice",
				Email: "alice@chef.io",
				Name:  "alice",
			},
			HashedPass: pass,
		}

		updatedUser, err := adp.UpdateUser(ctx, user)
		if err != nil {
			t.Fatal(err)
		}

		expectedUser := users.ShowUser{
			ID:    "alice",
			Email: "alice@chef.io",
			Name:  "alice",
		}

		if diff := pretty.Compare(expectedUser, updatedUser); diff != "" {
			t.Errorf("updated user: got!=want: %s", diff)
		}
	})

	t.Run("delete user", func(t *testing.T) {
		email := "bob@chef.io"

		deleted, err := adp.DeleteUser(ctx, email)
		if err != nil {
			t.Fatal(err)
		}

		if !deleted {
			t.Errorf("expected true, got false")
		}
	})

	t.Run("bcrypt hash password", func(t *testing.T) {
		password := "supercalifragilisticexpialidocious"

		hashedPassword, err := adp.HashPassword(password)
		if err != nil {
			t.Fatal(err)
		}

		// confirm password hashed properly
		pwCheck := []byte(password)
		err = bcrypt.CompareHashAndPassword(hashedPassword, pwCheck)
		if err != nil {
			t.Errorf("expected nil, got %s", err)
		}
	})

	t.Run("fails to bcrypt hash empty password", func(t *testing.T) {
		password := ""

		_, err := adp.HashPassword(password)
		if err == nil {
			t.Errorf("expected error, got nil")
		}
	})

	t.Run("validate user password, not verified", func(t *testing.T) {
		email := "bob@chef.io"
		password := "supercalifragilisticexpialidocious"

		ok, err := adp.ValidatePassword(ctx, email, password)
		if err != nil {
			t.Fatal(err)
		}
		if ok {
			t.Error("expected not ok, got ok")
		}
	})
	t.Run("validate user password, verified", func(t *testing.T) {
		email := "alice@chef.io"
		password := "supercalifragilisticexpialidocious"

		ok, err := adp.ValidatePassword(ctx, email, password)
		if err != nil {
			t.Fatal(err)
		}
		if !ok {
			t.Error("expected ok, got not ok")
		}
	})
	t.Run("validate user password, not found", func(t *testing.T) {
		email := "catherine@chef.io"
		password := "supercalifragilisticexpialidocious"

		ok, err := adp.ValidatePassword(ctx, email, password)
		if err != nil {
			t.Error("expected err == nil, got not nil")
		}
		if ok {
			t.Error("expected not ok, got ok")
		}
	})
}

// mock implementations to test against
func (d *dexAPI) ListPasswords(ctx context.Context, req *api.ListPasswordReq) (*api.ListPasswordResp, error) {
	resp := api.ListPasswordResp{
		Passwords: d.usrs,
	}
	return &resp, nil
}

func (d *dexAPI) CreatePassword(ctx context.Context, req *api.CreatePasswordReq) (*api.CreatePasswordResp, error) {
	found := false
	for _, p := range d.usrs {
		if p.Email == req.Password.Email {
			found = true
		}
	}
	resp := api.CreatePasswordResp{
		AlreadyExists: found,
	}
	return &resp, nil
}

func (d *dexAPI) UpdatePassword(ctx context.Context, req *api.UpdatePasswordReq) (*api.UpdatePasswordResp, error) {
	notFound := true
	for _, p := range d.usrs {
		if p.Email == req.Email {
			notFound = false
		}
	}
	resp := api.UpdatePasswordResp{
		NotFound: notFound,
	}
	return &resp, nil
}

func (d *dexAPI) DeletePassword(ctx context.Context, req *api.DeletePasswordReq) (*api.DeletePasswordResp, error) {
	notFound := true
	for _, p := range d.usrs {
		if p.Email == req.Email {
			notFound = false
		}
	}
	resp := api.DeletePasswordResp{
		NotFound: notFound,
	}
	return &resp, nil
}

func (d *dexAPI) VerifyPassword(ctx context.Context, req *api.VerifyPasswordReq) (*api.VerifyPasswordResp, error) {
	notFound := true
	for _, p := range d.usrs {
		if p.Email == req.Email {
			notFound = false
		}
	}
	resp := api.VerifyPasswordResp{
		NotFound: notFound,
		Verified: req.Email == "alice@chef.io",
	}
	return &resp, nil
}

// We don't care for clients or refresh tokens at all, so just stub these out
func (d *dexAPI) CreateClient(context.Context, *api.CreateClientReq) (*api.CreateClientResp, error) {
	return nil, errors.New("not implemented")
}

func (d *dexAPI) UpdateClient(context.Context, *api.UpdateClientReq) (*api.UpdateClientResp, error) {
	return nil, errors.New("not implemented")
}

func (d *dexAPI) DeleteClient(context.Context, *api.DeleteClientReq) (*api.DeleteClientResp, error) {
	return nil, errors.New("not implemented")
}

func (d *dexAPI) GetVersion(context.Context, *api.VersionReq) (*api.VersionResp, error) {
	return nil, errors.New("not implemented")
}

func (d *dexAPI) ListRefresh(context.Context, *api.ListRefreshReq) (*api.ListRefreshResp, error) {
	return nil, errors.New("not implemented")
}

func (d *dexAPI) RevokeRefresh(context.Context, *api.RevokeRefreshReq) (*api.RevokeRefreshResp, error) {
	return nil, errors.New("not implemented")
}
