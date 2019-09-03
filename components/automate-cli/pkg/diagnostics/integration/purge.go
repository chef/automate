package integration

import (
	"context"
	"fmt"

	"github.com/pkg/errors"
	"github.com/stretchr/testify/require"

	dc "github.com/chef/automate/api/config/deployment"
	"github.com/chef/automate/api/interservice/data_lifecycle"
	"github.com/chef/automate/components/automate-cli/pkg/client"
	"github.com/chef/automate/components/automate-cli/pkg/diagnostics"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/version"
)

// EventFeedPurgeDiagnostic create the diagnostic for event-feed purging
func EventFeedPurgeDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "event-feed-purge",
		Tags: diagnostics.Tags{"event-feed", "purge"},
		Generate: func(t diagnostics.TestContext) error {
			client, cleanup, err := newPurgeClientFor("event-feed-service")
			if err != nil {
				return err
			}
			defer cleanup() // nolint errcheck

			res, err := client.Show(context.Background(), &data_lifecycle.ShowRequest{})
			if err != nil {
				return errors.Wrap(err, "unable to determine event-feed purge configuration")
			}

			t.SetValue("event-feed-purge-initial-config", res)

			_, err = client.Configure(context.Background(), &data_lifecycle.ConfigureRequest{
				Enabled: true,
				PolicyUpdate: &data_lifecycle.PolicyUpdate{
					Es: []*data_lifecycle.EsPolicyUpdate{
						{
							Disabled:      true,
							PolicyName:    "feed",
							OlderThanDays: 123,
						},
					},
				},
			})

			if err != nil {
				return errors.Wrap(err, "failed to update event-feed purge policies")
			}

			return nil
		},

		Verify: func(t diagnostics.VerificationTestContext) {
			client, cleanup, err := newPurgeClientFor("event-feed-service")
			require.NoError(t, err)
			defer cleanup() // nolint errcheck

			res, err := client.Show(context.Background(), &data_lifecycle.ShowRequest{})
			require.NoError(t, err)

			require.Equal(t, 1, len(res.EsPolicies), "there should be one event-feed policies")

			for _, p := range res.EsPolicies {
				require.Truef(t, p.Disabled, "%s event-feed purge policy should be disabled", p.Name)
				require.Equal(t, int32(123), p.OlderThanDays, "older than days for %s ingest purge policy should be 123, not %d", p.Name, p.OlderThanDays)
			}
		},

		Cleanup: func(t diagnostics.TestContext) error {
			show := data_lifecycle.ShowResponse{}
			err := t.GetValue("event-feed-purge-initial-config", &show)
			if err != nil {
				return errors.Wrap(err, "failed to get initial purge config")
			}

			client, cleanup, err := newPurgeClientFor("event-feed-service")
			if err != nil {
				return errors.Wrap(err, "failed to create event-feed-service client")
			}
			defer cleanup() // nolint errcheck

			req := &data_lifecycle.ConfigureRequest{
				Enabled: true,
				PolicyUpdate: &data_lifecycle.PolicyUpdate{
					Es: []*data_lifecycle.EsPolicyUpdate{},
				},
			}

			for _, p := range show.EsPolicies {
				req.PolicyUpdate.Es = append(req.PolicyUpdate.Es, &data_lifecycle.EsPolicyUpdate{
					Disabled:      p.Disabled,
					PolicyName:    p.Name,
					OlderThanDays: p.OlderThanDays,
				})
			}

			_, err = client.Configure(context.Background(), req)
			return err
		},
	}
}

// CompliancePurgeDiagnostic create the diagnostic for compliance purging
func CompliancePurgeDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "compliance-purge",
		Tags: diagnostics.Tags{"compliance", "purge"},
		Generate: func(t diagnostics.TestContext) error {
			client, cleanup, err := newPurgeClientFor("compliance-service")
			if err != nil {
				return err
			}
			defer cleanup() // nolint errcheck

			res, err := client.Show(context.Background(), &data_lifecycle.ShowRequest{})
			if err != nil {
				return errors.Wrap(err, "unable to determine compliance purge configuration")
			}

			t.SetValue("compliance-purge-initial-config", res)

			_, err = client.Configure(context.Background(), &data_lifecycle.ConfigureRequest{
				Enabled: true,
				PolicyUpdate: &data_lifecycle.PolicyUpdate{
					Es: []*data_lifecycle.EsPolicyUpdate{
						{
							Disabled:      true,
							PolicyName:    "compliance-reports",
							OlderThanDays: 456,
						},
						{
							Disabled:      true,
							PolicyName:    "compliance-scans",
							OlderThanDays: 456,
						},
					},
				},
			})

			if err != nil {
				return errors.Wrap(err, "failed to update compliance purge policies")
			}

			return nil
		},

		Verify: func(t diagnostics.VerificationTestContext) {
			client, cleanup, err := newPurgeClientFor("compliance-service")
			require.NoError(t, err)
			defer cleanup() // nolint errcheck

			res, err := client.Show(context.Background(), &data_lifecycle.ShowRequest{})
			require.NoError(t, err)

			require.Equal(t, 2, len(res.EsPolicies), "there should be two compliance policies")

			for _, p := range res.EsPolicies {
				require.Truef(t, p.Disabled, "%s compliance purge policy should be disabled", p.Name)
				require.Equal(t, int32(456), p.OlderThanDays, "older than days for %s ingest purge policy should be 456, not %d", p.Name, p.OlderThanDays)
			}
		},

		Cleanup: func(t diagnostics.TestContext) error {
			show := data_lifecycle.ShowResponse{}
			err := t.GetValue("compliance-purge-initial-config", &show)
			if err != nil {
				return errors.Wrap(err, "failed to get initial purge config")
			}

			client, cleanup, err := newPurgeClientFor("compliance-service")
			if err != nil {
				return errors.Wrap(err, "failed to create compliance-service purge client")
			}
			defer cleanup() // nolint errcheck

			req := &data_lifecycle.ConfigureRequest{
				Enabled: true,
				PolicyUpdate: &data_lifecycle.PolicyUpdate{
					Es: []*data_lifecycle.EsPolicyUpdate{},
				},
			}

			for _, p := range show.EsPolicies {
				req.PolicyUpdate.Es = append(req.PolicyUpdate.Es, &data_lifecycle.EsPolicyUpdate{
					Disabled:      p.Disabled,
					PolicyName:    p.Name,
					OlderThanDays: p.OlderThanDays,
				})
			}

			_, err = client.Configure(context.Background(), req)
			return err
		},
	}
}

// IngestPurgeDiagnostic create the diagnostic for ingest purging
func IngestPurgeDiagnostic() diagnostics.Diagnostic {
	return diagnostics.Diagnostic{
		Name: "ingest-purge",
		Tags: diagnostics.Tags{"ingest", "purge"},
		Generate: func(t diagnostics.TestContext) error {
			client, cleanup, err := newPurgeClientFor("ingest-service")
			if err != nil {
				return err
			}
			defer cleanup() // nolint errcheck

			res, err := client.Show(context.Background(), &data_lifecycle.ShowRequest{})
			if err != nil {
				return errors.Wrap(err, "unable to determine ingest purge configuration")
			}

			t.SetValue("ingest-purge-initial-config", res)

			_, err = client.Configure(context.Background(), &data_lifecycle.ConfigureRequest{
				Enabled: true,
				PolicyUpdate: &data_lifecycle.PolicyUpdate{
					Es: []*data_lifecycle.EsPolicyUpdate{
						{
							Disabled:      true,
							PolicyName:    "actions",
							OlderThanDays: 789,
						},
						{
							Disabled:      true,
							PolicyName:    "converge-history",
							OlderThanDays: 789,
						},
					},
				},
			})

			if err != nil {
				return errors.Wrap(err, "failed to update ingest purge policies")
			}

			return nil
		},

		Verify: func(t diagnostics.VerificationTestContext) {
			client, cleanup, err := newPurgeClientFor("ingest-service")
			require.NoError(t, err)
			defer cleanup() // nolint errcheck

			res, err := client.Show(context.Background(), &data_lifecycle.ShowRequest{})
			require.NoError(t, err)

			require.Equal(t, 2, len(res.EsPolicies), "there should be two ingest policies")

			for _, p := range res.EsPolicies {
				require.Truef(t, p.Disabled, "%s ingest purge policy should be disabled", p.Name)
				require.Equal(t, int32(789), p.OlderThanDays, "older than days for %s ingest purge policy should be 789, not %d", p.Name, p.OlderThanDays)
			}
		},

		Cleanup: func(t diagnostics.TestContext) error {
			show := data_lifecycle.ShowResponse{}
			err := t.GetValue("ingest-purge-initial-config", &show)
			if err != nil {
				return errors.Wrap(err, "failed to get initial purge config")
			}

			client, cleanup, err := newPurgeClientFor("ingest-service")
			if err != nil {
				return errors.Wrap(err, "failed to create ingest-service client")
			}
			defer cleanup() // nolint errcheck

			req := &data_lifecycle.ConfigureRequest{
				Enabled: true,
				PolicyUpdate: &data_lifecycle.PolicyUpdate{
					Es: []*data_lifecycle.EsPolicyUpdate{},
				},
			}

			for _, p := range show.EsPolicies {
				req.PolicyUpdate.Es = append(req.PolicyUpdate.Es, &data_lifecycle.EsPolicyUpdate{
					Disabled:      p.Disabled,
					PolicyName:    p.Name,
					OlderThanDays: p.OlderThanDays,
				})
			}

			_, err = client.Configure(context.Background(), req)
			return err
		},
	}
}

func newPurgeClientFor(service string) (data_lifecycle.PurgeClient, func() error, error) {
	var cleanup func() error

	certs, err := client.LoadLocalCerts()
	if err != nil {
		return nil, cleanup, err
	}
	factory := secureconn.NewFactory(*certs, secureconn.WithVersionInfo(
		version.Version,
		version.GitSHA,
	))

	// There's probably some magical reflection way to get this from the a2
	// service config but since we know which services implement it we can
	// make an address book.
	host := "127.0.0.1"
	var port int32

	switch service {
	case "ingest-service":
		port = dc.DefaultAutomateConfig().GetIngest().GetV1().GetSys().GetService().GetPort().GetValue()
	case "compliance-service":
		port = dc.DefaultAutomateConfig().GetCompliance().GetV1().GetSys().GetService().GetPort().GetValue()
	case "event-feed-service":
		port = dc.DefaultAutomateConfig().GetEventFeedService().GetV1().GetSys().GetService().GetPort().GetValue()
	default:
		return nil, cleanup, errors.Errorf("unknown purge service: %s", service)
	}

	con, err := factory.Dial(service, fmt.Sprintf("%s:%d", host, port))
	if err != nil {
		return nil, cleanup, errors.Wrap(err, "failed to dial service")
	}

	return data_lifecycle.NewPurgeClient(con), con.Close, nil
}

func init() {
	diagnostics.RegisterDiagnostic(IngestPurgeDiagnostic())
	diagnostics.RegisterDiagnostic(CompliancePurgeDiagnostic())
	diagnostics.RegisterDiagnostic(EventFeedPurgeDiagnostic())
}
