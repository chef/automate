package integration_test

import (
	"testing"
	"time"

	"context"

	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/api/interservice/event_feed"
	"github.com/chef/automate/components/event-feed-service/pkg/persistence"
	"github.com/chef/automate/components/event-feed-service/pkg/server"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"github.com/teambition/rrule-go"
)

// TestPurgeServer tests the data lifecycle purge Server
func TestPurgeServer(t *testing.T) {
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
			{description: "enabled with hourly recurrence",
				enabled:    true,
				recurrence: rec1.String(),
				update: &data_lifecycle.PolicyUpdate{
					Es: []*data_lifecycle.EsPolicyUpdate{
						{
							PolicyName: server.PurgeFeedPolicyName,
						},
					},
				},
			},
			{description: "disabled with daily recurrence",
				enabled:    false,
				recurrence: rec2.String(),
				update: &data_lifecycle.PolicyUpdate{
					Es: []*data_lifecycle.EsPolicyUpdate{
						{
							PolicyName: server.PurgeFeedPolicyName,
						},
					},
				},
			},
		}

		for _, c := range cases {
			_, err := testSuite.purgeClient.Configure(ctx, &data_lifecycle.ConfigureRequest{
				Enabled:      c.enabled,
				Recurrence:   c.recurrence,
				PolicyUpdate: c.update,
			})

			require.NoError(t, err)

			res, err = testSuite.purgeClient.Show(ctx, &data_lifecycle.ShowRequest{})
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

		_, err = testSuite.purgeClient.Configure(context.Background(), &data_lifecycle.ConfigureRequest{
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
		cases := []struct {
			description       string
			expectedRemaining int64
			before            func()
			after             func()
		}{
			{description: "purge should delete 900 docs and leave 100 docs",
				expectedRemaining: 100,
				after:             func() { GetTestSuite().DeleteAllDocuments(); return },
				before: func() {
					for _, entry := range createEntries(time.Now().UTC(), 100, int(time.Hour)*-24) {
						testSuite.feedBackend.CreateFeedEntry(entry)
					}

					testSuite.RefreshIndices(persistence.IndexNameFeeds)
				},
			},
			{
				description:       "no docs in index... purge should delete 0 docs",
				expectedRemaining: 0,
				after:             func() { GetTestSuite().DeleteAllDocuments(); return },
			},
		}

		for _, test := range cases {
			t.Run(test.description, func(t *testing.T) {
				if a := test.after; a != nil {
					defer a()
				}
				if b := test.before; b != nil {
					b()
				}

				recurrence, err := rrule.NewRRule(rrule.ROption{
					Freq:     rrule.HOURLY,
					Interval: 2,
					Dtstart:  time.Now(),
				})
				require.NoError(t, err)

				_, err = testSuite.purgeClient.Configure(context.Background(), &data_lifecycle.ConfigureRequest{
					Enabled:    true,
					Recurrence: recurrence.String(),
					PolicyUpdate: &data_lifecycle.PolicyUpdate{
						Es: []*data_lifecycle.EsPolicyUpdate{
							{
								PolicyName:    server.PurgeFeedPolicyName,
								OlderThanDays: 1,
							},
						},
					},
				})
				require.NoError(t, err)

				_, err = testSuite.purgeClient.Run(context.Background(), &data_lifecycle.RunRequest{})
				require.NoError(t, err)

				testSuite.RefreshIndices(persistence.IndexNameFeeds)

				counts, err := testSuite.feedClient.GetFeedSummary(context.Background(), &event_feed.FeedSummaryRequest{CountCategory: "event-type"})
				require.NoError(t, err)

				assert.Equal(t, test.expectedRemaining, counts.TotalEntries,
					"Expected number of post-purge documents remaining does not match results %d != %d", test.expectedRemaining, counts.TotalEntries)
			})
		}
	})
}
