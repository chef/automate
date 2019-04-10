package commands

import (
	"context"
	"fmt"
	"strconv"
	"time"

	"github.com/golang/protobuf/ptypes/empty"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	"google.golang.org/grpc"

	api "github.com/chef/automate/api/interservice/es_sidecar"
	"github.com/chef/automate/components/es-sidecar-service/pkg/elastic"
	"github.com/chef/automate/lib/grpc/secureconn"
	"github.com/chef/automate/lib/tls/certs"
)

type devConfig struct {
	GRPCServerHost   string
	GRPCServerPort   int
	ElasticsearchURL string
	ConnFactory      *secureconn.Factory
}

var devCreateRepositoryCmd = &cobra.Command{
	Use:   "create-repo NAME",
	Short: "Creates a testing backup repository",
	Run: func(cmd *cobra.Command, args []string) {
		cfg := readRunParams()
		es := esConnect(cfg.ElasticsearchURL)

		res, err := es.CreateRepository(context.Background(), args[0])
		if err != nil {
			panic(err)
		}
		if res == true {
			fmt.Println("Repository created and verified")
		} else {
			fmt.Println("Repository created but not verified")
		}
	},
}

var devListCmd = &cobra.Command{
	Use:   "list-indices",
	Short: "Show a list of all ES5 indices",
	Run: func(cmd *cobra.Command, args []string) {
		cfg := readRunParams()
		es := esConnect(cfg.ElasticsearchURL)
		res, err := es.AllIndices()
		if err != nil {
			panic(err)
		}
		fmt.Println("Indices:")
		for _, i := range res {
			fmt.Printf(" - %s\n", i)
		}
	},
}

var devCreateIndexRangeCmd = &cobra.Command{
	Use:   "create-indices BASENAME numdays",
	Short: "Create a range of date-named indices for testing",
	Run: func(cmd *cobra.Command, args []string) {
		days, e := strconv.Atoi(args[1])
		if e != nil {
			panic(e)
		}
		cfg := readRunParams()
		es := esConnect(cfg.ElasticsearchURL)
		es.CreateTimeNamedIndices(context.Background(), time.Now(), args[0], days)
		fmt.Printf("Created %d days worth of indices with the base name %s\n", days, args[0])
	},
}
var devCreateIndexCmd = &cobra.Command{
	Use:   "create-index NAME",
	Short: "Just creates a simple index named NAME",
	Run: func(cmd *cobra.Command, args []string) {
		cfg := readRunParams()
		if len(args) != 1 {
			fmt.Println(cmd.UsageString())
			return
		}
		es := esConnect(cfg.ElasticsearchURL)
		err := es.MakeMeAnIndex(context.Background(), args[0])
		if err != nil {
			fmt.Printf("Error creating index: %v\n", err.Error())
		} else {
			fmt.Printf("Created index %s\n", args[0])
		}
	},
}
var devDiskStatsCmd = &cobra.Command{
	Use:   "disk",
	Short: "Show disk stats",
	Run: func(cmd *cobra.Command, args []string) {
		cfg := readRunParams()
		es := esConnect(cfg.ElasticsearchURL)
		stats, err := es.GetDiskStats(context.Background())
		if err != nil {
			panic(err)
		}
		fmt.Printf("%+v\n", stats)
	},
}

var grpcVersionCmd = &cobra.Command{
	Use:   "version",
	Short: "Invokes Version GRPC endpoint",
	Run: func(cmd *cobra.Command, args []string) {
		cfg := readRunParams()
		conn := grpcConnect(cfg)
		client := api.NewEsSidecarClient(conn)
		res, err := client.Version(context.Background(), &empty.Empty{})
		if err != nil {
			panic(err)
		}
		fmt.Printf("Version: %v\n", res.GetVersion())
	},
}

var grpcPurgeDocumentsFromIndexCmd = &cobra.Command{
	Use:   "purge-doc INDEXNAME DAYS",
	Short: "Invokes PurgeDocumentsFromIndexByAge GRPC",
	Run: func(cmd *cobra.Command, args []string) {
		if len(args) != 2 {
			log.Errorf("Usage: %s\n", cmd.UsageString())
			return
		}

		days, err := strconv.Atoi(args[1])
		if err != nil || days < 1 || days > 10000 {
			log.Errorf("Second argument must be a number of days between 1 and 10000\n\n")
			return
		}
		cfg := readRunParams()
		conn := grpcConnect(cfg)
		client := api.NewEsSidecarClient(conn)
		req := api.PurgeRequest{
			Id:                   "123",
			Index:                args[0],
			OlderThanDays:        int32(days),
			PurgeWhenNotInBackup: false,
		}
		res, err := client.PurgeDocumentsFromIndexByAge(context.Background(), &req)
		if err != nil {
			panic(err)
		}
		fmt.Printf("Success: %v\nMesssage: %v\nFailed deletions: %v\n", res.GetSuccess(), res.GetMessage(), res.GetFailures())
	},
}

var grpcPurgeTimeSeriesIndexCmd = &cobra.Command{
	Use:   "purge-ts BASENAME DAYS",
	Short: "Invokes PurgeTimeSeriesIndex over GRPC",
	Run: func(cmd *cobra.Command, args []string) {
		if len(args) != 2 {
			log.Errorf("Usage: %s\n", cmd.UsageString())
			return
		}

		days, err := strconv.Atoi(args[1])
		if err != nil || days < 1 || days > 10000 {
			log.Errorf("Second argument must be a number of days between 1 and 10000\n\n")
			return
		}
		cfg := readRunParams()
		conn := grpcConnect(cfg)
		client := api.NewEsSidecarClient(conn)
		req := api.PurgeRequest{
			Id:                   "123",
			Index:                args[0],
			OlderThanDays:        int32(days),
			PurgeWhenNotInBackup: false,
		}
		res, err := client.PurgeTimeSeriesIndicesByAge(context.Background(), &req)
		if err != nil {
			panic(err)
		}
		fmt.Printf("Success: %v\nMesssage: %v\n", res.GetSuccess(), res.GetMessage())
	},
}

func init() {
	devCmd := &cobra.Command{
		Use:   "dev [OPTIONS] SUBCOMMAND <ARGS>",
		Short: "A suite of commands to assist in local development and testing. Not for production use.",
	}
	// Most exposed 'dev' commands will use our our elastic interface directly, so make it a common option
	devCmd.Flags().String("elasticsearch-url", "http://localhost:9200", "URL to ElasticSearch (<protocol>://domain:<port>)/")
	err := viper.BindPFlags(devCmd.Flags())
	if err != nil {
		panic(errors.Wrap(err, "BindPFlags in init failed"))
	}

	grpcCmd := &cobra.Command{
		Use:   "grpc [OPTIONS] SUBCOMMAND <ARGS>",
		Short: "Invoke service functionality over GRPC",
	}
	grpcCmd.Flags().String("host", "localhost", "Host of es-sidecar-service GRPC service. Default localhost.")
	grpcCmd.Flags().Int("port", 10390, "Port of es-sidecar-service GRPC service. Default 10390.")
	err = viper.BindPFlags(grpcCmd.Flags())
	if err != nil {
		panic(errors.Wrap(err, "BindPFlags in init failed"))
	}

	// Add grpc subcommands here
	grpcCmd.AddCommand(grpcPurgeTimeSeriesIndexCmd)
	grpcCmd.AddCommand(grpcPurgeDocumentsFromIndexCmd)
	grpcCmd.AddCommand(grpcVersionCmd)

	// Add dev subcommands here
	devCmd.AddCommand(devCreateRepositoryCmd)
	devCmd.AddCommand(devCreateIndexRangeCmd)
	devCmd.AddCommand(devDiskStatsCmd)
	devCmd.AddCommand(devListCmd)
	devCmd.AddCommand(devCreateIndexCmd)

	err = viper.BindPFlags(devCmd.Flags())
	if err != nil {
		panic(errors.Wrap(err, "BindPFlags in init failed"))
	}

	RootCmd.AddCommand(devCmd)
	RootCmd.AddCommand(grpcCmd)

}

func readRunParams() devConfig {
	TLSConfig := certs.TLSConfig{
		CertPath:       viper.GetString("cert"),
		KeyPath:        viper.GetString("key"),
		RootCACertPath: viper.GetString("root-cert"),
	}
	serviceCerts, err := TLSConfig.ReadCerts()
	if err != nil {
		log.WithFields(log.Fields{
			"error": err.Error(),
		}).Fatal("Failed to load certs")
	}
	factory := secureconn.NewFactory(*serviceCerts)
	return devConfig{
		GRPCServerHost:   viper.GetString("host"),
		GRPCServerPort:   viper.GetInt("port"),
		ElasticsearchURL: viper.GetString("elasticsearch-url"),
		ConnFactory:      factory,
	}
}

func grpcConnect(cfg devConfig) *grpc.ClientConn {
	host := fmt.Sprintf("%s:%d", cfg.GRPCServerHost, cfg.GRPCServerPort)
	conn, err := cfg.ConnFactory.Dial("es-sidecar-service", host)
	if err != nil {
		log.Errorf("Failed to dial GRPC on %s\n", host)
		panic(err)
	}
	return conn
}

func esConnect(esURL string) (es *elastic.Elastic) {
	es, err := elastic.New(esURL)
	if err != nil {
		panic(err)
	}
	return es
}
