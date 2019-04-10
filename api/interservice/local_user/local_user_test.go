package local_user_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/api/interservice/local_user"
	"github.com/chef/automate/lib/grpc/grpctest"
	uuid "github.com/chef/automate/lib/uuid4"
)

func TestGeneratedProtobufUpToDate(t *testing.T) {
	grpctest.AssertCompiledInUpToDate(t, "api/interservice/local_user/")
}

func TestValidation(t *testing.T) {
	negativeCases := map[string]interface{}{
		"empty create req": &local_user.CreateUserReq{},
		"invalid email in create req": &local_user.CreateUserReq{
			Name:     "alice",
			Email:    "!",
			Password: "letmein!",
		},
		"empty name in create req": &local_user.CreateUserReq{
			Name:     "",
			Email:    "e@o",
			Password: "letmein!",
		},
		"empty update": &local_user.UpdateUserReq{},
		"empty name in update": &local_user.UpdateUserReq{
			Name:     "",
			Email:    "alice_+-foo2000@aol.com",
			Password: "letmein!",
		},
		// what the API calls email is our username, so, this is a bit confusing
		/* tflechtner - leaving here but commented out, because when we do the migration resulting in only
		   valid user names, we should reinstate this test.
			"invalid username": &local_user.Email{
				Email: "!@?.com",
			},
		*/
		"empty username": &local_user.Email{
			Email: "",
		},
	}
	positiveCases := map[string]interface{}{
		"complete create": &local_user.CreateUserReq{
			Name:     "alice schmidt",
			Email:    "alice_+-foo2000@aol.com",
			Password: "letmein!",
		},
		"complete update": &local_user.UpdateUserReq{
			Id:       uuid.Must(uuid.NewV4()).String(),
			Name:     "alice schmidt",
			Email:    "alice_+-foo2000@aol.com",
			Password: "letmein!",
		},
		"update without password": &local_user.UpdateUserReq{
			Id:       uuid.Must(uuid.NewV4()).String(),
			Name:     "alice",
			Email:    "alice_+-foo2000@aol.com",
			Password: "", // password is optional for UpdateUserReq
		},
		"OK username": &local_user.Email{
			Email: "alice_+-foo2000@aol.com",
		},
	}

	classes := map[bool]map[string]interface{}{
		true:  positiveCases,
		false: negativeCases,
	}

	for expectedSuccess, cases := range classes {
		for name, tc := range cases {
			t.Run(name, func(t *testing.T) {
				r, ok := tc.(interface {
					Validate() error
				})
				require.True(t, ok)
				err := r.Validate()
				if expectedSuccess {
					assert.NoError(t, err)
				} else {
					assert.Error(t, err)
				}
			})
		}
	}
}
