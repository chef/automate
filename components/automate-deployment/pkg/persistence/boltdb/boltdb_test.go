package boltdb

import (
	"errors"
	"io/ioutil"
	"os"
	"testing"

	"github.com/boltdb/bolt"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/persistence"
)

type mockVersion struct {
	initialize      func(tx *bolt.Tx) error
	readDeployment  func(tx *bolt.Tx) (*deployment.Deployment, error)
	writeDeployment func(*bolt.Tx, *deployment.Deployment) error
	cleanup         func(*bolt.Tx) error
	name            func() string
}

func (m *mockVersion) Initialize(tx *bolt.Tx) error {
	if m.initialize != nil {
		return m.initialize(tx)
	}
	return nil
}

func (m *mockVersion) ReadDeployment(tx *bolt.Tx) (*deployment.Deployment, error) {
	if m.readDeployment != nil {
		return m.readDeployment(tx)
	}
	return nil, nil
}

func (m *mockVersion) WriteDeployment(tx *bolt.Tx, d *deployment.Deployment) error {
	if m.writeDeployment != nil {
		return m.writeDeployment(tx, d)
	}
	return nil
}

func (m *mockVersion) Cleanup(tx *bolt.Tx) error {
	if m.cleanup != nil {
		return m.cleanup(tx)
	}
	return nil
}

func (m *mockVersion) Name() string {
	if m.name != nil {
		return m.name()
	}
	return "mock"
}

func TestBoltDBDeploymentStoreInitialization(t *testing.T) {
	t.Run("Initialized set to true on successful initialization", func(t *testing.T) {
		WithDatabase(t, func(t *testing.T, db *bolt.DB) {
			store := NewDeploymentStore(db)
			store.v = &mockVersion{}
			err := store.Initialize()
			assert.NoError(t, err)
			assert.True(t, store.initialized)
		})
	})
	t.Run("Initialized set to false on failed initialization", func(t *testing.T) {
		WithDatabase(t, func(t *testing.T, db *bolt.DB) {
			store := NewDeploymentStore(db)
			store.v = &mockVersion{
				initialize: func(tx *bolt.Tx) error {
					return errors.New("Fail")
				},
			}
			err := store.Initialize()
			assert.Error(t, err)
			assert.False(t, store.initialized)
		})
	})
	t.Run("Cannot call UpdateDeployment when not initialized", func(t *testing.T) {
		WithDatabase(t, func(t *testing.T, db *bolt.DB) {
			store := NewDeploymentStore(db)
			store.v = &mockVersion{
				readDeployment: func(*bolt.Tx) (*deployment.Deployment, error) {
					assert.FailNow(t, "Uninitialized store used")
					return nil, nil
				},
			}
			_, err := store.UpdateDeployment(func(*deployment.Deployment) error { return nil })
			assert.Equal(t, persistence.ErrNotInitialized, err)
		})
	})
	t.Run("Cannot call GetDeployment when not initialized", func(t *testing.T) {
		WithDatabase(t, func(t *testing.T, db *bolt.DB) {
			store := NewDeploymentStore(db)
			store.v = &mockVersion{
				readDeployment: func(*bolt.Tx) (*deployment.Deployment, error) {
					assert.FailNow(t, "Uninitialized store used")
					return nil, nil
				},
			}
			_, err := store.GetDeployment()
			assert.Equal(t, persistence.ErrNotInitialized, err)
		})
	})
}

func TestSchemaUpdates(t *testing.T) {
	t.Run("no updates happen if the latest deployment version if found", func(t *testing.T) {
		WithDatabase(t, func(t *testing.T, db *bolt.DB) {
			store := NewDeploymentStore(db)
			store.v = &mockVersion{
				readDeployment: func(*bolt.Tx) (*deployment.Deployment, error) {
					return &deployment.Deployment{Deployed: false}, nil
				},
				cleanup: func(*bolt.Tx) error {
					assert.FailNow(t, "The current version was cleaned up")
					return nil
				},
			}
			store.upgradeable = []UpgradeableVersion{
				&mockVersion{
					readDeployment: func(*bolt.Tx) (*deployment.Deployment, error) {
						assert.FailNow(t, "Old version read when new version was found")
						return &deployment.Deployment{Deployed: false}, nil
					},
					cleanup: func(*bolt.Tx) error {
						assert.FailNow(t, "Old version called cleanup when it shouldn't have been modified")
						return nil
					},
				},
			}
			err := store.Initialize()
			assert.NoError(t, err)
		})
	})

	t.Run("the deployment is rewritten if it is not up to date using the most up to date version", func(t *testing.T) {
		WithDatabase(t, func(t *testing.T, db *bolt.DB) {
			store := NewDeploymentStore(db)
			store.v = &mockVersion{
				readDeployment: func(*bolt.Tx) (*deployment.Deployment, error) {
					return nil, persistence.ErrDoesNotExist
				},
				writeDeployment: func(_ *bolt.Tx, d *deployment.Deployment) error {
					assert.Equal(t, &deployment.Deployment{Deployed: true}, d)
					return nil
				},
				cleanup: func(*bolt.Tx) error {
					assert.FailNow(t, "The current version was cleaned up")
					return nil
				},
			}
			store.upgradeable = []UpgradeableVersion{
				&mockVersion{
					readDeployment: func(*bolt.Tx) (*deployment.Deployment, error) {
						return &deployment.Deployment{Deployed: true}, nil
					},
				},
				&mockVersion{
					readDeployment: func(*bolt.Tx) (*deployment.Deployment, error) {
						assert.FailNow(t, "Old version read when newer version was found")
						return nil, persistence.ErrDoesNotExist
					},
				},
			}
			err := store.Initialize()
			assert.NoError(t, err)
		})
	})

	t.Run("multiple old versions can be iterated", func(t *testing.T) {
		WithDatabase(t, func(t *testing.T, db *bolt.DB) {
			store := NewDeploymentStore(db)
			store.v = &mockVersion{
				readDeployment: func(*bolt.Tx) (*deployment.Deployment, error) {
					return nil, persistence.ErrDoesNotExist
				},
				writeDeployment: func(_ *bolt.Tx, d *deployment.Deployment) error {
					assert.Equal(t, &deployment.Deployment{Deployed: true}, d)
					return nil
				},
				cleanup: func(*bolt.Tx) error {
					assert.FailNow(t, "The current version was cleaned up")
					return nil
				},
			}
			store.upgradeable = []UpgradeableVersion{
				&mockVersion{
					readDeployment: func(*bolt.Tx) (*deployment.Deployment, error) {
						return nil, persistence.ErrDoesNotExist
					},
				},
				&mockVersion{
					readDeployment: func(*bolt.Tx) (*deployment.Deployment, error) {
						return &deployment.Deployment{Deployed: true}, nil
					},
				},
			}
			err := store.Initialize()
			assert.NoError(t, err)
		})
	})
}

func TestUpdateDeployment(t *testing.T) {
	t.Run("Can update the value that exists", func(t *testing.T) {
		WithDatabase(t, func(t *testing.T, db *bolt.DB) {
			store := NewDeploymentStore(db)
			store.v = &mockVersion{
				readDeployment: func(*bolt.Tx) (*deployment.Deployment, error) {
					return &deployment.Deployment{Deployed: false}, nil
				},
				writeDeployment: func(_ *bolt.Tx, dd *deployment.Deployment) error {
					assert.Equal(t, &deployment.Deployment{Deployed: true}, dd)
					return nil
				},
			}
			err := store.Initialize()
			require.NoError(t, err)

			dd, err := store.UpdateDeployment(func(d *deployment.Deployment) error {
				d.Deployed = true
				return nil
			})
			assert.NoError(t, err)
			assert.Equal(t, &deployment.Deployment{Deployed: true}, dd)
		})
	})
	t.Run("Can update the value when no deployment exists", func(t *testing.T) {
		WithDatabase(t, func(t *testing.T, db *bolt.DB) {
			store := NewDeploymentStore(db)
			store.v = &mockVersion{
				readDeployment: func(*bolt.Tx) (*deployment.Deployment, error) {
					return nil, persistence.ErrDoesNotExist
				},
				writeDeployment: func(_ *bolt.Tx, dd *deployment.Deployment) error {
					assert.Equal(t, &deployment.Deployment{Deployed: true}, dd)
					return nil
				},
			}
			err := store.Initialize()
			require.NoError(t, err)

			dd, err := store.UpdateDeployment(func(d *deployment.Deployment) error {
				d.Deployed = true
				return nil
			})
			assert.NoError(t, err)
			assert.Equal(t, &deployment.Deployment{Deployed: true}, dd)
		})
	})
	t.Run("Errors in v.ReadDeployment are passed back", func(t *testing.T) {
		WithDatabase(t, func(t *testing.T, db *bolt.DB) {
			myerr := errors.New("my error")
			store := NewDeploymentStore(db)
			err := store.Initialize()
			require.NoError(t, err)

			store.v = &mockVersion{
				readDeployment: func(*bolt.Tx) (*deployment.Deployment, error) {
					return nil, myerr
				},
				writeDeployment: func(*bolt.Tx, *deployment.Deployment) error {
					return nil
				},
			}
			_, err = store.UpdateDeployment(func(d *deployment.Deployment) error {
				d.Deployed = true
				return nil
			})
			assert.Equal(t, myerr, err)
		})
	})
	t.Run("Errors in v.WriteDeployment are passed back", func(t *testing.T) {
		WithDatabase(t, func(t *testing.T, db *bolt.DB) {
			myerr := errors.New("my error")
			store := NewDeploymentStore(db)
			store.v = &mockVersion{
				readDeployment: func(*bolt.Tx) (*deployment.Deployment, error) {
					return &deployment.Deployment{Deployed: true}, nil
				},
				writeDeployment: func(*bolt.Tx, *deployment.Deployment) error {
					return myerr
				},
			}
			err := store.Initialize()
			require.NoError(t, err)

			_, err = store.UpdateDeployment(func(d *deployment.Deployment) error {
				d.Deployed = true
				return nil
			})
			assert.Equal(t, myerr, err)
		})
	})
	t.Run("Passes back the error in callback", func(t *testing.T) {
		WithDatabase(t, func(t *testing.T, db *bolt.DB) {
			myerr := errors.New("my error")
			store := NewDeploymentStore(db)
			store.v = &mockVersion{
				readDeployment: func(*bolt.Tx) (*deployment.Deployment, error) {
					return &deployment.Deployment{Deployed: true}, nil
				},
				writeDeployment: func(*bolt.Tx, *deployment.Deployment) error {
					assert.FailNow(t, "Should not have updated the database")
					return nil
				},
			}
			err := store.Initialize()
			require.NoError(t, err)

			_, err = store.UpdateDeployment(func(d *deployment.Deployment) error {
				return myerr
			})
			assert.Equal(t, myerr, err)
		})
	})
}

func TestGetDeployment(t *testing.T) {
	t.Run("Can get a value", func(t *testing.T) {
		WithDatabase(t, func(t *testing.T, db *bolt.DB) {
			store := NewDeploymentStore(db)
			store.v = &mockVersion{
				readDeployment: func(*bolt.Tx) (*deployment.Deployment, error) {
					return &deployment.Deployment{Deployed: true}, nil
				},
				writeDeployment: func(*bolt.Tx, *deployment.Deployment) error {
					assert.FailNow(t, "Should not be called")
					return nil
				},
			}
			err := store.Initialize()
			require.NoError(t, err)

			dd, err := store.GetDeployment()
			assert.NoError(t, err)
			assert.Equal(t, &deployment.Deployment{Deployed: true}, dd)
		})
	})
	t.Run("Errors are passed back", func(t *testing.T) {
		WithDatabase(t, func(t *testing.T, db *bolt.DB) {
			myerr := errors.New("my error")
			store := NewDeploymentStore(db)
			err := store.Initialize()
			require.NoError(t, err)

			store.v = &mockVersion{
				readDeployment: func(*bolt.Tx) (*deployment.Deployment, error) {
					return nil, myerr
				},
				writeDeployment: func(*bolt.Tx, *deployment.Deployment) error {
					assert.FailNow(t, "Should not be called")
					return nil
				},
			}
			_, err = store.GetDeployment()
			assert.Equal(t, myerr, err)
		})
	})
}

func WithDatabase(t *testing.T, f func(*testing.T, *bolt.DB)) {
	path := tempfile()
	database, err := bolt.Open(path, 0600, nil)
	defer os.Remove(path)
	defer database.Close()
	if err != nil {
		t.Fatal(err)
	}
	f(t, database)
}

func tempfile() string {
	f, err := ioutil.TempFile("", "a2-deployment-")
	if err != nil {
		panic(err)
	}
	if err := f.Close(); err != nil {
		panic(err)
	}
	if err := os.Remove(f.Name()); err != nil {
		panic(err)
	}
	return f.Name()
}
