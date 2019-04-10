package integration

import (
	"encoding/json"
	"io/ioutil"
	"net/http"
	"time"

	uuid "github.com/gofrs/uuid"
	"github.com/pkg/errors"
)

// TimestampName returns a name with a timestamp
func TimestampName() string {
	ts := time.Now()
	return "integration-diagnostic-" + ts.Format("20060102150405")
}

// Password returns a random password
func Password() string {
	return uuid.Must(uuid.NewV4()).String()
}

// mustJSONDecodeSuccess is a thing that will hold a http response or error. It
// should wrap a result that must be 200 to be considered successful
type mustJSONDecodeSuccess struct {
	resp *http.Response
	err  error
}

// MustJSONDecodeSuccess wraps an http request response and error that must not error
// and must 200. You can also decode the json value with it, and it must succeed
func MustJSONDecodeSuccess(resp *http.Response, err error) *mustJSONDecodeSuccess {
	return &mustJSONDecodeSuccess{
		resp: resp,
		err:  err,
	}
}

// WithValue deserializes the json body of the response into value if a 200 is returned.
// Otherwise, an error is returned.
func (m *mustJSONDecodeSuccess) WithValue(value interface{}) error {
	if m.err != nil {
		return m.err
	}
	reqPath := m.resp.Request.URL.String()
	method := m.resp.Request.Method

	defer func() {
		_ = m.resp.Body.Close()
	}()

	body, err := ioutil.ReadAll(m.resp.Body)
	if err != nil {
		return errors.Wrapf(err, "Failed to %s %s. Could not read body", method, reqPath)
	}

	if m.resp.StatusCode != 200 {
		return errors.Errorf("Failed to %s %s. Received unexpected status code %d\nResponse Body:\n %s", method, reqPath, m.resp.StatusCode, body)
	}

	err = json.Unmarshal(body, value)

	if err != nil {
		return errors.Wrapf(err, "Failed to %s %s. Could not decode body.\nResponse Body:\n %s", method, reqPath, body)
	}

	return nil
}

// Error returns an error if this represents an error, otherwise nil
func (m *mustJSONDecodeSuccess) Error() error {
	return m.err
}

// Retry calls f. If f returns an error, it will be retried again after the
// backoff and at most times times.
func Retry(times int, backoff time.Duration, f func() error) error {
	for {
		err := f()
		times--
		if err == nil || times <= 0 {
			return err
		}
		time.Sleep(backoff)
	}
}
