package v2_test

import (
	"testing"

	storage "github.com/chef/automate/components/authz-service/storage/v2"
)

func TestNewProject(t *testing.T) {
	for name, tc := range map[string]struct {
		id, name  string
		typeVal   storage.Type
		expectErr bool
	}{
		"empty name": {
			id:        "my-id",
			expectErr: true,
		},
		"all whitespace name": {
			id:        "my-id",
			name:      "   ",
			expectErr: true,
		},
		"empty id": {
			name:      "my-name",
			expectErr: true,
		},
		"all whitespace id": {
			id:        "   ",
			name:      "my-name",
			expectErr: true,
		},
		"non-empty id and name": {
			id:   "my-id",
			name: "my-name",
		},
	} {
		t.Run(name, func(t *testing.T) {
			if _, err := storage.NewProject(
				tc.id, tc.name, tc.typeVal, 0); tc.expectErr != (err != nil) {
				t.Fail()
			}
		})
	}
}
