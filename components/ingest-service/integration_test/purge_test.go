package integration_test

import (
	"context"
	"testing"
	"time"

	"github.com/stretchr/testify/require"
	"github.com/teambition/rrule-go"

	"github.com/chef/automate/api/interservice/data_lifecycle"
	iBackend "github.com/chef/automate/components/ingest-service/backend"
)

// TestPurge tests the Purge server
func TestPurge(t *testing.T) {
	t.Run("Configure", func(t *testing.T) {
		var (
			ctx  = context.Background()
			rec1 *rrule.RRule
			rec2 *rrule.RRule
			err  error
			res  *data_lifecycle.ShowResponse
		)

		rec1, err = rrule.NewRRule(rrule.ROption{
			Freq:     rrule.HOURLY,
			Interval: 1,
			Dtstart:  time.Now(),
		})
		require.NoError(t, err)

		rec2, err = rrule.NewRRule(rrule.ROption{
			Freq:     rrule.DAILY,
			Interval: 1,
			Dtstart:  time.Now(),
		})
		require.NoError(t, err)

		cases := []struct {
			description string
			enabled     bool
			recurrence  string
			update      *data_lifecycle.PolicyUpdate
		}{
			{description: "enabled one week retention with hourly recurrence",
				enabled:    true,
				recurrence: rec1.String(),
				update: &data_lifecycle.PolicyUpdate{
					Es: []*data_lifecycle.EsPolicyUpdate{
						{
							PolicyName:    "converge-history",
							OlderThanDays: 7,
						},
						{
							PolicyName:    "actions",
							OlderThanDays: 7,
						},
					},
				},
			},
			{description: "disabled two week retention with daily recurrence",
				enabled:    false,
				recurrence: rec2.String(),
				update: &data_lifecycle.PolicyUpdate{
					Es: []*data_lifecycle.EsPolicyUpdate{
						{
							PolicyName:    "converge-history",
							OlderThanDays: 14,
						},
						{
							PolicyName:    "actions",
							OlderThanDays: 14,
						},
					},
				},
			},
		}

		for _, c := range cases {
			_, err := suite.PurgeServer.Configure(ctx, &data_lifecycle.ConfigureRequest{
				Enabled:      c.enabled,
				Recurrence:   c.recurrence,
				PolicyUpdate: c.update,
			})

			require.NoError(t, err)

			res, err = suite.PurgeServer.Show(ctx, &data_lifecycle.ShowRequest{})
			require.Equal(t, res.Enabled, c.enabled)
			require.Equal(t, res.Recurrence, c.recurrence)

			for _, s := range c.update.Es {
				found := false
				for _, p := range res.EsPolicies {
					if p.Name == s.PolicyName {
						found = true
						require.Equal(t, p.OlderThanDays, s.OlderThanDays)
						break
					}
				}
				require.Truef(t, found, "unable to find matching policy for %s", s.PolicyName)
			}
		}
	})

	t.Run("Configure with unknown policy", func(t *testing.T) {
		var (
			recurrence *rrule.RRule
			err        error
		)

		recurrence, err = rrule.NewRRule(rrule.ROption{
			Freq:     rrule.DAILY,
			Interval: 1,
			Dtstart:  time.Now(),
		})
		require.NoError(t, err)

		_, err = suite.PurgeServer.Configure(context.Background(), &data_lifecycle.ConfigureRequest{
			Enabled:    true,
			Recurrence: recurrence.String(),
			PolicyUpdate: &data_lifecycle.PolicyUpdate{
				Es: []*data_lifecycle.EsPolicyUpdate{
					{
						PolicyName: "not-a-valid-policy",
					},
				},
			},
		})

		require.Error(t, err)
		require.Contains(t, err.Error(), "not a valid ES policy")
	})

	t.Run("Run", func(t *testing.T) {
		defer suite.DeleteAllDocuments()

		var (
			ctx        = context.Background()
			err        error
			recurrence *rrule.RRule
		)

		actions := []iBackend.InternalChefAction{
			iBackend.InternalChefAction{
				RemoteHostname:   "chef-server.org",
				OrganizationName: "org1",
				Projects:         []string{"org1"},
				RecordedAt:       time.Now(), // always today's index
			},
			iBackend.InternalChefAction{
				RemoteHostname:   "chef-server.org",
				OrganizationName: "org2",
				Projects:         []string{"org2"},
				RecordedAt:       time.Now().Add(time.Hour * -24), // always yesterday's index
			},
			iBackend.InternalChefAction{
				RemoteHostname:   "chef-server.org",
				OrganizationName: "org1",
				Projects:         []string{"org1"},
				RecordedAt:       time.Now().Add(time.Hour * -32),
			},
			iBackend.InternalChefAction{
				RemoteHostname:   "chef-server.org",
				OrganizationName: "org2",
				Projects:         []string{"org2"},
				RecordedAt:       time.Now().Add(time.Hour * -64),
			},
		}

		recurrence, err = rrule.NewRRule(rrule.ROption{
			Freq:     rrule.HOURLY,
			Interval: 2,
			Dtstart:  time.Now(),
		})
		require.NoError(t, err)

		suite.IngestActions(actions)
		suite.RefreshIndices(suite.Indices()...)
		waitForModificationsToApply()

		actualActions, err := suite.GetActions(5)
		require.NoError(t, err)
		require.Equal(t, 4, len(actualActions), "wrong number of actions retrieved")

		_, err = suite.PurgeServer.Configure(ctx, &data_lifecycle.ConfigureRequest{
			Enabled:    true,
			Recurrence: recurrence.String(),
			PolicyUpdate: &data_lifecycle.PolicyUpdate{
				Es: []*data_lifecycle.EsPolicyUpdate{
					{
						PolicyName:    "actions",
						OlderThanDays: 1,
					},
				},
			},
		})
		require.NoError(t, err)
		waitForModificationsToApply()

		_, err = suite.PurgeServer.Run(ctx, &data_lifecycle.RunRequest{})
		require.NoError(t, err)
		suite.RefreshIndices(suite.Indices()...)
		waitForModificationsToApply()
		actualActions, err = suite.GetActions(5)
		require.NoError(t, err)
		require.Equal(t, 1, len(actualActions), "wrong number of actions retrieved")
	})
}
