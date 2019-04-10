package segmentio

import (
	analytics "gopkg.in/segmentio/analytics-go.v3"

	"github.com/chef/automate/components/trial-license-service/pkg/recorder"
	"github.com/chef/automate/lib/logger"
)

// This is added as "form" key to the segment.io identify record
const trialLicenseForm = "Trial License Service"

type client struct {
	c analytics.Client
}

type logWrap struct {
	l logger.Logger
}

func (lw logWrap) Logf(format string, args ...interface{}) {
	lw.l.Infof(format, args)
}
func (lw *logWrap) Errorf(format string, args ...interface{}) {
	lw.l.Errorf(format, args)
}

// New returns a segmentio-backed recorder.Client
func New(writeKey string, l logger.Logger) (recorder.Client, error) {
	cfg := analytics.Config{
		Logger: &logWrap{l: l},
	}

	cl, err := analytics.NewWithConfig(writeKey, cfg)
	return &client{c: cl}, err
}

// Note: Enqueue() will not send the request right away: that'll happen in
// batches, either when the max batch size (default: 250) has been reached, or
// at certain intervals (default: 5s), depending on what comes first.
func (c *client) Record(licenseID, customerID, name, email, automateVersion, deploymentID string,
	gdprAgree bool) error {
	return c.c.Enqueue(analytics.Identify{
		UserId: customerID,
		Traits: analytics.NewTraits().
			SetName(name).
			SetEmail(email).
			Set("license_id", licenseID).
			Set("deployment_id", deploymentID).
			Set("automate_version", automateVersion).
			Set("form", trialLicenseForm).
			Set("gdpr_agree", gdprAgree),
	})
}
