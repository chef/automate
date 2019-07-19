// Configuration options for the ingest service's GRPC and REST servers. This
// name was chosen instead of "Config" because "config" is used for "config
// management" already.
package serveropts

import (
	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/grpc/secureconn"
)

type ChefIngestRunPipelineConfig struct {
	MaxNumberOfBundledMsgs   int
	NumberOfMsgsTransformers int
	NumberOfPublishers       int
}

type ChefIngestServerConfig struct {
	MaxNumberOfBundledActionMsgs int
	ChefIngestRunPipelineConfig  ChefIngestRunPipelineConfig
}

type Opts struct {
	Host                          string
	Port                          int
	RestPort                      int
	ElasticSearchUrl              string
	EsSidecarAddress              string
	AuthzAddress                  string
	EventAddress                  string
	LogLevel                      string
	PurgeConvergeHistoryAfterDays int32
	PurgeActionsAfterDays         int32
	ConnFactory                   *secureconn.Factory
	projectUpdateStateFilePath    string
	NodeManagerAddress            string
	ChefIngestServerConfig        ChefIngestServerConfig
}

// SetLogLevel sets the log level for the service
func (o *Opts) SetLogLevel() {
	if o.LogLevel == "" {
		return
	}

	log.WithFields(log.Fields{
		"level": o.LogLevel,
	}).Info("Setting log level")

	level, err := log.ParseLevel(o.LogLevel)
	if err != nil {
		log.WithField("level", o.LogLevel).WithError(err).Error("Using default level 'info'")
		return
	}

	log.SetLevel(level)
}
