package v1

import (
	"testing"
	"time"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"

	"github.com/boltdb/bolt"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/manifest"
	"github.com/chef/automate/components/automate-deployment/pkg/persistence"
	. "github.com/chef/automate/components/automate-deployment/pkg/persistence/boltdb/internal/test"
)

func buildDeploymentFromPublicFields(in *deployment.Deployment) *deployment.Deployment {
	cfg := &dc.AutomateConfig{}
	d, _ := deployment.RestoreDeploymentFromUserOverrideConfig(cfg)

	d.ID = in.ID
	d.Deployed = in.Deployed
	d.LastAction = in.LastAction
	d.ExpectedServices = in.ExpectedServices
	d.CreatedAt = in.CreatedAt
	d.CurrentReleaseManifest = in.CurrentReleaseManifest

	return d
}

func buildManifest(t *testing.T) *manifest.A2 {
	packages, err := habpkg.FromStrings("foo/bar\nbaz/quux")
	if err != nil {
		t.Fatal(err)
	}

	hart, err := habpkg.HartFromPath("/some/path/ssd-deployment-service-0.1.0-20180119115432-x86_64-linux.hart")
	hart.WithOrigin("ssd")
	hart.WithName("deployment-service")
	if err != nil {
		t.Fatal(err)
	}

	return &manifest.A2{
		Build:         "build",
		BuildSHA:      "buildsha",
		Packages:      packages,
		HartOverrides: []habpkg.Hart{hart},
	}
}

func TestRoundTrip(t *testing.T) {
	t.Run("can roundtrip full deployment", func(t *testing.T) {
		impl := New()

		ts := time.Now()
		WithDatabase(t, func(t *testing.T, db *bolt.DB) {
			db.Update(func(tx *bolt.Tx) error {
				err := impl.Initialize(tx)
				require.NoError(t, err)
				d := buildDeploymentFromPublicFields(&deployment.Deployment{
					ID:         "myid",
					Deployed:   true,
					LastAction: "lastaction",
					ExpectedServices: []*deployment.Service{
						Postgresql(t),
						AutomateElasticsearch(t),
						EsSideCar(t),
					},
					CreatedAt:              ts,
					CurrentReleaseManifest: buildManifest(t),
				})
				err = impl.WriteDeployment(tx, d)
				assert.NoError(t, err)
				return err
			})
			db.View(func(tx *bolt.Tx) error {
				d, err := impl.ReadDeployment(tx)
				require.NoError(t, err)
				assert.Equal(t, "myid", d.ID)
				assert.Equal(t, true, d.Deployed)
				assert.Equal(t, "lastaction", d.LastAction)
				assert.Equal(t, ts.UTC(), d.CreatedAt)

				assert.Equal(t, Postgresql(t), d.ExpectedServices[0])
				assert.Equal(t, AutomateElasticsearch(t), d.ExpectedServices[1])
				assert.Equal(t, EsSideCar(t), d.ExpectedServices[2])
				assert.Equal(t, buildManifest(t), d.CurrentReleaseManifest)

				return nil
			})
		})
	})

	t.Run("can roundtrip deployment with nil config", func(t *testing.T) {
		impl := New()

		ts := time.Now()
		WithDatabase(t, func(t *testing.T, db *bolt.DB) {
			db.Update(func(tx *bolt.Tx) error {
				err := impl.Initialize(tx)
				require.NoError(t, err)
				d := &deployment.Deployment{
					ID:         "myid",
					Deployed:   true,
					LastAction: "lastaction",
					ExpectedServices: []*deployment.Service{
						Postgresql(t),
						AutomateElasticsearch(t),
						EsSideCar(t),
					},
					CreatedAt:              ts,
					CurrentReleaseManifest: buildManifest(t),
				}
				err = impl.WriteDeployment(tx, d)
				assert.NoError(t, err)
				return err
			})

			db.View(func(tx *bolt.Tx) error {
				d, err := impl.ReadDeployment(tx)
				require.NoError(t, err)
				assert.Equal(t, "myid", d.ID)
				assert.Equal(t, true, d.Deployed)
				assert.Equal(t, "lastaction", d.LastAction)
				assert.Equal(t, ts.UTC(), d.CreatedAt)

				assert.Equal(t, Postgresql(t), d.ExpectedServices[0])
				assert.Equal(t, AutomateElasticsearch(t), d.ExpectedServices[1])
				assert.Equal(t, EsSideCar(t), d.ExpectedServices[2])
				assert.Equal(t, buildManifest(t), d.CurrentReleaseManifest)
				assert.Nil(t, d.Config)
				return nil
			})
		})
	})
}

func TestInitializeRead(t *testing.T) {
	// A read after initialization must return ErrDoesNotExist
	impl := New()
	WithDatabase(t, func(t *testing.T, db *bolt.DB) {
		db.Update(func(tx *bolt.Tx) error {
			err := impl.Initialize(tx)
			assert.NoError(t, err)
			return nil
		})
		db.View(func(tx *bolt.Tx) error {
			_, err := impl.ReadDeployment(tx)
			assert.Equal(t, persistence.ErrDoesNotExist, err)
			return nil
		})
	})
}

func TestCleanup(t *testing.T) {
	impl := New()
	WithDatabase(t, func(t *testing.T, db *bolt.DB) {
		db.Update(func(tx *bolt.Tx) error {
			err := impl.Initialize(tx)
			assert.NoError(t, err)
			d := buildDeploymentFromPublicFields(&deployment.Deployment{
				ID:         "myid",
				Deployed:   true,
				LastAction: "lastaction",
				ExpectedServices: []*deployment.Service{
					Postgresql(t),
					AutomateElasticsearch(t),
					EsSideCar(t),
				},
			})
			err = impl.WriteDeployment(tx, d)
			assert.NoError(t, err)
			return err
		})
		db.Update(func(tx *bolt.Tx) error {
			return impl.Cleanup(tx)
		})
		db.View(func(tx *bolt.Tx) error {
			_, err := impl.ReadDeployment(tx)
			require.Equal(t, persistence.ErrDoesNotExist, err)
			return nil
		})
	})
}
