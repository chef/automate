package segmentio

import (
	"bytes"
	"encoding/json"
	"errors"
	"testing"

	uuid "github.com/gofrs/uuid"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	analytics "gopkg.in/segmentio/analytics-go.v3"

	"github.com/chef/automate/components/trial-license-service/pkg/recorder"
)

func TestRecord(t *testing.T) {
	r := rec{}
	c := newTestRecorder(t, &r)

	name, email := "Gonzo The Great", "gonzo@gonzomail.com"
	customerID, licenseID, deploymentID := uuid.Must(uuid.NewV4()).String(), uuid.Must(uuid.NewV4()).String(), uuid.Must(uuid.NewV4()).String()
	automateVersion := "20180515214820"
	gdprAgree := true

	err := c.Record(licenseID, customerID, name, email, automateVersion, deploymentID, gdprAgree)
	require.NoError(t, err)

	expectedMap := map[string]interface{}{
		// segment record artifact? probably gets filled in when actually sending the request
		"timestamp": "0001-01-01T00:00:00Z",
		"userId":    customerID,
		"traits": map[string]interface{}{
			"automate_version": automateVersion,
			"license_id":       licenseID,
			"deployment_id":    deploymentID,
			"email":            email,
			"gdpr_agree":       gdprAgree,
			"name":             name,
			"form":             "Trial License Service",
		},
	}
	expected := bytes.Buffer{}
	err = json.NewEncoder(&expected).Encode(expectedMap)
	require.NoError(t, err)

	assert.Equal(t, 1, len(r.records))
	actual := bytes.Buffer{}
	err = json.NewEncoder(&actual).Encode(r.records[0])
	require.NoError(t, err)

	assert.JSONEq(t, expected.String(), actual.String())
}

type rec struct {
	records []analytics.Identify
}

func (*rec) Close() error {
	return nil
}
func (r *rec) Enqueue(msg analytics.Message) error {
	if i, ok := msg.(analytics.Identify); ok {
		r.records = append(r.records, i)
		return nil
	}
	return errors.New("unexpected segment message")
}

func newTestRecorder(t *testing.T, r *rec) recorder.Client {
	return &client{c: r}
}
