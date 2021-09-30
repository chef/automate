package main

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/chef/automate/components/report-manager-service/config"
	"github.com/minio/minio-go/v7"
	"github.com/minio/minio-go/v7/pkg/credentials"
	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
)

func main() {
	if err := rootCmd.Execute(); err != nil {
		fmt.Println(err.Error())
		os.Exit(1)
	}
}

var cfgFile string
var conf config.ReportManager

var rootCmd = &cobra.Command{
	Use: "report-manager-service",
}

var serveCmd = &cobra.Command{
	Use:  "serve",
	Args: cobra.NoArgs,
	RunE: serve,
}

func initConfig() error {
	if cfgFile != "" {
		viper.SetConfigFile(cfgFile)
		if err := viper.ReadInConfig(); err != nil {
			return err
		}
	}

	if err := viper.Unmarshal(&conf); err != nil {
		return err
	}

	switch conf.Log.Level {
	case "trace":
		logrus.SetLevel(logrus.TraceLevel)
	case "debug":
		logrus.SetLevel(logrus.DebugLevel)
	case "info":
		logrus.SetLevel(logrus.InfoLevel)
	case "warn":
		logrus.SetLevel(logrus.WarnLevel)
	case "error":
		logrus.Error(logrus.ErrorLevel)
	}

	return nil
}

func init() {
	rootCmd.AddCommand(serveCmd)

	serveCmd.PersistentFlags().StringVar(&cfgFile, "config", "", "config file")
}

func serve(*cobra.Command, []string) error {

	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()

	if err := initConfig(); err != nil {
		return errors.Wrap(err, "failed to load config")
	}

	//Wait till report-manager-minio-gateway is up and running
	time.Sleep(30 * time.Second)

	endpoint := "127.0.0.1:10197"
	accessKeyID := "minioadmin"
	secretAccessKey := "minioadmin"
	useSSL := false

	minioClient, err := minio.New(endpoint, &minio.Options{
		Creds:  credentials.NewStaticV4(accessKeyID, secretAccessKey, ""),
		Secure: useSSL,
	})
	if err != nil {
		fmt.Println("Error in establishing a connection to minio", err)
	} else {
		fmt.Println("Minio connection established")
	}

	// temperory code to print the log

	//TODO: Remove the below loop
	for {
		fmt.Println("Printing from Report Manager", conf.Service.Message)
		time.Sleep(5 * time.Second)

		//Test Minio setup
		buckets, err := minioClient.ListBuckets(ctx)
		if err != nil {
			fmt.Println("Error in getting the buckets list:", err)
		}

		fmt.Println("Buckets Count:", len(buckets))
		for _, bucket := range buckets {
			fmt.Println("Buckets: ", bucket.Name)
		}

	}
}
