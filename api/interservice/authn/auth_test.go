package authn_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/lib/grpc/grpctest"
)

func TestGeneratedProtobufUpToDate(t *testing.T) {
	grpctest.AssertCompiledInUpToDate(t, "interservice/authn/")
}

func TestValidationCreateTokenID(t *testing.T) {
	negativeCases := map[string]*authn.CreateTokenReq{
		"with uppercase characters": &authn.CreateTokenReq{
			Id:       "TestID",
			Name:     "name",
			Projects: []string{"project1", "project2"},
		},
		"with non-dash characters": &authn.CreateTokenReq{
			Id:       "test#space",
			Name:     "name",
			Projects: []string{"project1", "project2"},
		},
		"with spaces": &authn.CreateTokenReq{
			Id:       "test space",
			Name:     "name",
			Projects: []string{"project1", "project2"},
		},
		"whitespace projects list": &authn.CreateTokenReq{
			Id:       "valid-id",
			Name:     "name",
			Projects: []string{"     ", "test"},
		},
		"repeated projects in list": &authn.CreateTokenReq{
			Id:       "valid-id",
			Name:     "name",
			Projects: []string{"repeat", "repeat"},
		},
		"project has invalid characters": &authn.CreateTokenReq{
			Id:       "valid-id",
			Name:     "name",
			Projects: []string{"valid", "wrong~"},
		},
		"project has spaces": &authn.CreateTokenReq{
			Id:       "valid-id",
			Name:     "name",
			Projects: []string{"valid", "wrong space"},
		},
		"project is too long": &authn.CreateTokenReq{
			Id:       "valid-id",
			Name:     "name",
			Projects: []string{"much-too-long-longest-word-in-english-pneumonoultramicroscopicsilicovolcanoconiosis", "valid"},
		},
		"with no characters": &authn.CreateTokenReq{
			Id:       "",
			Name:     "name",
			Projects: []string{"project1", "project2"},
		},
	}
	positiveCases := map[string]*authn.CreateTokenReq{
		"projects are missing": &authn.CreateTokenReq{
			Id:   "valid-id",
			Name: "name",
		},
		"underscore in IDs": &authn.CreateTokenReq{
			Id:       "valid_id",
			Name:     "name",
			Projects: []string{"project_1", "project_2"},
		},
		"with ID all lowercase": &authn.CreateTokenReq{
			Id:       "test",
			Name:     "name",
			Projects: []string{"project1", "project2"},
		},
		"with ID that has dashes": &authn.CreateTokenReq{
			Id:       "-test-with-dashes-",
			Name:     "name",
			Projects: []string{"project1", "project2"},
		},
		"with ID that has dashes and numbers": &authn.CreateTokenReq{
			Id:       "1-test-with-1-and-dashes-0",
			Name:     "name",
			Projects: []string{"project1", "project2"},
		},
		"with ID that has only numbers": &authn.CreateTokenReq{
			Id:       "1235",
			Name:     "name",
			Projects: []string{"project1", "project2"},
		},
		"with a single project": &authn.CreateTokenReq{
			Id:       "1235",
			Name:     "name",
			Projects: []string{"project1"},
		},
		"projects are empty": &authn.CreateTokenReq{
			Id:       "valid-id",
			Name:     "name",
			Projects: []string{},
		},
	}

	classes := map[bool]map[string]*authn.CreateTokenReq{
		true:  positiveCases,
		false: negativeCases,
	}

	for expectedSuccess, cases := range classes {
		for name, tc := range cases {
			t.Run(name, func(t *testing.T) {
				err := tc.Validate()
				if expectedSuccess {
					assert.NoError(t, err)
				} else {
					assert.Error(t, err)
				}
			})
		}
	}
}
