package v2_test

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/require"

	v2 "github.com/chef/automate/api/interservice/authz/v2"
)

func TestIsAuthorizedValidation_Action(t *testing.T) {
	cases := map[bool][]string{ // valid/invalid -> input
		true: []string{
			"a:b:c",
			"sOME:cAmeL:cAse",
		},
		false: []string{
			"A:B:C",
			"Titlecase:Not:Allowed",
			"titlecase:Not:Allowed",
			"titlecase:not:Allowed",
			"Titlecase:Not:allowed",
			"Titlecase:not:allowed",
			"too:short",
			"no:wildcards:*",
			"no:*:*",
			"no:*",
			"no^:bug:here",
		},
	}

	for expectedValid, ts := range cases {
		t.Run(fmt.Sprintf("%v", expectedValid), func(t *testing.T) {
			for _, tc := range ts {
				t.Run(tc, func(t *testing.T) {
					req := validRequest()
					req.Action = tc
					err := req.Validate()
					if expectedValid {
						require.NoError(t, err)
					} else {
						require.Error(t, err)
					}
				})
			}
		})
	}
}

func validRequest() *v2.IsAuthorizedReq {
	return &v2.IsAuthorizedReq{
		Subjects: []string{"user:local:alice"},
		Action:   "iam:user:list",
		Resource: "auth:users",
	}
}
