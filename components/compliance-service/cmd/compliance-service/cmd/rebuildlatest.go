package cmd

import (
	"github.com/chef/automate/components/compliance-service/config"
	"github.com/chef/automate/components/compliance-service/utils/logging"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
)

var rebuildLatestConf = config.Compliance{
	ElasticSearch: config.ElasticSearch{
		Url: "http://127.0.0.1:9200/",
	},
	LatestRebuilder: config.LatestRebuilder{
		Clean:                        false,
		DaysBeyondMostRecentScanDate: 0,
	},
}

// rebuildLatestCmd represents the rebuild-latest command
var rebuildLatestCmd = &cobra.Command{
	Use:   "rebuild-latest",
	Short: "Rebuild ElasticSearch latest index",
	Long:  `Rebuild ElasticSearch latest index from the timeseries indices`,
	Run: func(cmd *cobra.Command, args []string) {
		logging.SetLogLevel(rebuildLatestConf.LogLevel)
		logrus.Info("Rebuilding Compliance Latest Index Cache")
		// define the ElasticSearch Reporting backend
		//	esr := relaxting.ES2Backend{ESUrl: rebuildLatestConf.ElasticSearch.Url}

		// Populate the Compliance Latest index from the timeseries indices
		//esr.GenerateComplianceLatest(rebuildLatestConf.LatestRebuilder.Clean, rebuildLatestConf.LatestRebuilder.DaysBeyondMostRecentScanDate, false)
	},
}

func init() {
	RootCmd.AddCommand(rebuildLatestCmd)

	rebuildLatestCmd.Flags().StringVar(&rebuildLatestConf.Service.LogLevel, "log-level", rebuildLatestConf.Service.LogLevel, "Log Level")

	// ElasticSearch Config Flags
	rebuildLatestCmd.Flags().StringVar(&rebuildLatestConf.ElasticSearch.Url, "es-url", rebuildLatestConf.ElasticSearch.Url, "ES Url")

	//Clean - sets flag to tell GenerateComplianceLatest to start from beginning and regen instead of from most recent valid latest date
	rebuildLatestCmd.Flags().BoolVar(&rebuildLatestConf.LatestRebuilder.Clean, "rebuild-latest-clean", rebuildLatestConf.LatestRebuilder.Clean, "Rebuilds latest indices from beginning of scan history.  Defaults to false.")

	//Clean - sets flag to tell GenerateComplianceLatest to start from beginning and regen instead of from most recent valid latest date
	rebuildLatestCmd.Flags().IntVar(&rebuildLatestConf.LatestRebuilder.DaysBeyondMostRecentScanDate, "days-beyond-most-recent", rebuildLatestConf.LatestRebuilder.DaysBeyondMostRecentScanDate, "Rebuilds latest indices this many days beyond most recent scan. Used for testing only  Defaults to 0.")

}
