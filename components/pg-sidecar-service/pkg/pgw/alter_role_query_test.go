package pgw

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestAlterQueryRole(t *testing.T) {
	t.Run("without role", func(t *testing.T) {
		q := &AlterRoleQuery{}
		assert.Equal(t, "", q.String())
	})

	t.Run("with password", func(t *testing.T) {
		q := &AlterRoleQuery{
			Role: "foo",
			Password: AlterRoleQueryPassword{
				Value: "bar",
			},
		}

		assert.Equal(t, `ALTER ROLE "foo" WITH PASSWORD 'bar'`, q.String())
	})

	t.Run("with unencrypted password", func(t *testing.T) {
		q := &AlterRoleQuery{
			Role: "foo",
			Password: AlterRoleQueryPassword{
				Value:       "bar",
				Unencrypted: true,
			},
		}

		assert.Equal(t, `ALTER ROLE "foo" WITH UNENCRYPTED PASSWORD 'bar'`, q.String())
	})

	t.Run("with connection limit", func(t *testing.T) {
		q := &AlterRoleQuery{
			Role:            "foo",
			ConnectionLimit: 300,
		}

		assert.Equal(t, `ALTER ROLE "foo" WITH CONNECTION LIMIT 300`, q.String())
	})

	t.Run("truthy options", func(t *testing.T) {
		q := &AlterRoleQuery{
			Role:        "foo",
			Superuser:   true,
			CreateDB:    true,
			CreateRole:  true,
			CreateUser:  true,
			Inherit:     true,
			Login:       true,
			Replication: true,
		}

		assert.Equal(t, `ALTER ROLE "foo" WITH SUPERUSER CREATEDB CREATEROLE CREATEUSER INHERIT LOGIN REPLICATION`, q.String())
	})

	t.Run("falsey options", func(t *testing.T) {
		q := &AlterRoleQuery{
			Role:          "foo",
			NoSuperuser:   true,
			NoCreateDB:    true,
			NoCreateRole:  true,
			NoCreateUser:  true,
			NoInherit:     true,
			NoLogin:       true,
			NoReplication: true,
		}

		assert.Equal(t, `ALTER ROLE "foo" WITH NOSUPERUSER NOCREATEDB NOCREATEROLE NOCREATEUSER NOINHERIT NOLOGIN NOREPLICATION`, q.String())
	})
}
