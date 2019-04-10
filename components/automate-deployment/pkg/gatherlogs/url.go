package gatherlogs

import (
	"crypto/tls"
	"io"
	"net/http"

	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/httputils"
)

// URL holds the fetching data
type URL struct {
	Name          string
	URL           string
	OutputHandler func() (io.WriteCloser, error)
}

func (u *URL) execute() error {
	logctx := log.WithFields(log.Fields{"url": u.URL})

	out, err := u.OutputHandler()
	if err != nil {
		logctx.WithError(err).Error("could not open output file")
		return err
	}
	defer out.Close() // nolint: errcheck

	_, err = io.WriteString(out, u.URL+"\n\n")
	if err != nil {
		logctx.WithError(err).Error("write failed")
		return err
	}

	logctx.Info("Collecting output for support bundle")

	tr := httputils.NewDefaultTransport()
	tr.TLSClientConfig = &tls.Config{
		InsecureSkipVerify: true,
	}
	c := *http.DefaultClient // copy
	c.Transport = tr

	resp, err := c.Get(u.URL)
	if err != nil {
		log.WithFields(
			log.Fields{
				"error": err,
				"url":   u.URL,
			},
		).Warn("Failed to fetch URL")
		return err
	}
	defer resp.Body.Close()

	_, err = io.Copy(out, resp.Body)
	if err != nil {
		logctx.WithError(err).Error("Failed to read/write response body")
		return err
	}
	return nil
}
