package server

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/url"
	"strings"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/components/trial-license-service/pkg/license"
	"github.com/chef/automate/components/trial-license-service/pkg/recorder"
	"github.com/chef/automate/components/trial-license-service/pkg/recorder/segmentio"
	"github.com/chef/automate/lib/logger"
	"github.com/chef/automate/lib/version"
)

// trialDays is the number of days for which we request a license from
// license-generation-service (it's passed to the fetcher)
const trialDays = 60

type trialRequest struct {
	FirstName           string `json:"first_name"`
	LastName            string `json:"last_name"`
	Email               string `json:"email"`
	GDPRAgree           bool   `json:"gdpr_agree"`
	DeploymentID        string `json:"deployment_id"`
	ChefAutomateVersion string `json:"chef_automate_version"`
}

// Start initializes a new server listening on host:port.
func Start(host, port string,
	licenseURL *url.URL, apiKey string,
	segmentWriteKey string,
	l logger.Logger) error {
	f := license.NewFetcher(licenseURL, apiKey, trialDays)

	rec, err := segmentio.New(segmentWriteKey, l)
	if err != nil {
		return errors.Wrap(err, "create segmentio recorder")
	}

	serv := &http.Server{
		Addr:    fmt.Sprintf("%s:%s", host, port),
		Handler: NewServeMux(f, rec, l),
	}
	l.Infof("t-l-s starting %s:%s", host, port)
	return serv.ListenAndServe()
}

// NewServeMux returns a http.ServeMux as is used by server.Start()
func NewServeMux(f license.Fetcher, rec recorder.Client, l logger.Logger) *http.ServeMux {
	mux := http.NewServeMux()
	mux.HandleFunc("/status", serviceStatusHandler)
	mux.HandleFunc("/version", serviceVersionHandler)
	mux.HandleFunc("/create-trial", newCreateTrialHandler(f, rec, l))
	return mux
}

func serviceStatusHandler(w http.ResponseWriter, r *http.Request) {
	status := struct {
		Status string `json:"status"`
	}{"OK"}

	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	if err := json.NewEncoder(w).Encode(status); err != nil {
		http.Error(w, errors.Wrap(err, "JSON generation error").Error(), http.StatusInternalServerError)
	}
}

func serviceVersionHandler(w http.ResponseWriter, r *http.Request) {
	resp := struct {
		Version string `json:"version"`
	}{version.Version}
	w.Header().Set("Content-Type", "application/json; charset=utf-8")
	if err := json.NewEncoder(w).Encode(resp); err != nil {
		http.Error(w, errors.Wrap(err, "JSON generation error").Error(), http.StatusInternalServerError)
	}
}

func newCreateTrialHandler(
	fetcher license.Fetcher,
	rec recorder.Client,
	l logger.Logger) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		tReq := trialRequest{}
		err := json.NewDecoder(r.Body).Decode(&tReq)
		if err != nil {
			http.Error(w, errors.Wrap(err, "unable to parse request").Error(), http.StatusBadRequest)
			return
		}

		err = validateTrialRequest(tReq)
		if err != nil {
			http.Error(w, errors.Wrap(err, "invalid trial request").Error(), http.StatusBadRequest)
			return
		}

		customerName := fmt.Sprintf("%s %s <%s> - TRIAL", tReq.FirstName, tReq.LastName, tReq.Email)
		lic, err := fetcher.RequestLicense(r.Context(), customerName)
		if err != nil {
			var code int

			switch err.(type) {
			case *license.ClientError:
				code = http.StatusBadRequest
			case *license.ServerError:
				code = http.StatusInternalServerError
			default:
				code = http.StatusInternalServerError
			}
			l.WithFields(logrus.Fields{"trial-request": tReq}).Error(err)
			http.Error(w, errors.Wrap(err, "unable to generate trial license").Error(), code)
			return
		}

		licID, err := lic.ID()
		if err != nil {
			l.WithFields(logrus.Fields{"trial-request": tReq}).Error(err)
			http.Error(w, errors.Wrap(err, "unable to decode license").Error(), http.StatusInternalServerError)
			return
		}

		err = rec.Record(
			licID, lic.CustomerID,
			fmt.Sprintf("%s %s", tReq.FirstName, tReq.LastName),
			tReq.Email,
			tReq.ChefAutomateVersion,
			tReq.DeploymentID,
			tReq.GDPRAgree)
		if err != nil {
			http.Error(w, errors.Wrap(err, "unable to record trial").Error(), http.StatusInternalServerError)
			return
		}

		resp := struct {
			License string `json:"license"`
		}{lic.License}
		if err := json.NewEncoder(w).Encode(resp); err != nil {
			http.Error(w, errors.Wrap(err, "JSON generation error").Error(), http.StatusInternalServerError)
		}
	}
}

// quick and dirty request validation
func validateTrialRequest(r trialRequest) error {
	missing := []string{}
	if r.FirstName == "" {
		missing = append(missing, "first_name")
	}
	if r.LastName == "" {
		missing = append(missing, "last_name")
	}
	if r.Email == "" {
		missing = append(missing, "email")
	}
	if r.DeploymentID == "" {
		missing = append(missing, "deployment_id")
	}
	if r.ChefAutomateVersion == "" {
		missing = append(missing, "chef_automate_version")
	}
	if len(missing) == 0 {
		return nil
	}
	return errors.New("missing fields: " + strings.Join(missing, ", "))
}
