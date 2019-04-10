// pg-helper: A command-line utility to call p-sidecar-service RPC's to
// perform database operations. It is intended to be used in hab hooks,
// though you can also use the pg-sidecar-service client library package
// in your application if you prefer to perform database operations in
// your application.
package main

import (
	"context"
	"fmt"
	"os"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"
	"github.com/spf13/viper"
	"google.golang.org/grpc/status"

	pgs "github.com/chef/automate/api/interservice/pg_sidecar"
	"github.com/chef/automate/components/pg-sidecar-service/pkg/client"
	"github.com/chef/automate/lib/platform"
)

var ErrUserRequired = errors.New("user not specified")

// HelperOpts are all the available configuration options
type HelperOpts struct {
	Debug      bool                      `mapstructure:"debug"`
	CfgFile    string                    `mapstructure:"cfg_file"`
	Client     ClientOpts                `mapstructure:"client"`
	Migrate    MigrateTablesOpts         `mapstructure:"migrate"`
	AlterRole  *pgs.AlterRoleReq_Options `mapstructure:"alter_role"`
	DropTables *pgs.DropTablesReq        `mapstructure:"drop_tables"`
	Sqitch     SqitchOpts                `mapstructure:"sqitch"`
}

// MigrateTablesOpts are the migration specific options
type MigrateTablesOpts struct {
	// SkipDBCreate allows you to skip the creation of the destination database
	// when trying to migrate tables
	SkipDBCreate bool `mapstructure:"skip_db_create"`

	// FailIfSrcMissing allows you to fail the command if the src database does not
	// exist. The default behaviour is false, which means do nothing and exit 0
	FailIfSrcMissing bool `mapstructure:"fail_if_src_missing"`
}

// ClientOpts are the pg-sidecar client specific options
type ClientOpts struct {
	ConTimeout    int    `mapstructure:"con_timeout"`
	Host          string `mapstructure:"host"`
	Port          int    `mapstructure:"port"`
	User          string `mapstructure:"user"`
	TLSCertPath   string `mapstructure:"tls_cert_path"`
	TLSKeyPath    string `mapstructure:"tls_key_path"`
	TLSRootCAPath string `mapstructure:"tls_root_ca_path"`
}

// SqitchOpts are the sqitch specific options
type SqitchOpts struct {
	User string `mapstructure:"user"`
}

var opts = HelperOpts{
	Client:  ClientOpts{},
	Migrate: MigrateTablesOpts{},
	AlterRole: &pgs.AlterRoleReq_Options{
		Password: &pgs.AlterRoleReq_Password{},
	},
	DropTables: &pgs.DropTablesReq{
		Tables: []string{},
	},
}

func main() {
	cobra.OnInitialize(func() {
		// If we have been configured to use a config file we'll want to load
		// it and use the values. Go's default values make this tricky so we'll
		// have to assume that if you provided a config file then what's in it
		// is the preferred source of configuration.
		if opts.CfgFile != "" {
			viper.SetConfigFile(opts.CfgFile)

			log := logrus.WithField("file", opts.CfgFile)

			// If a config file is found, read it in.
			if err := viper.ReadInConfig(); err != nil {
				log.WithError(err).Warn("Failed to load configuration file")
			} else {
				log.Info("Loading configuration file overrides")
			}

			if err := viper.Unmarshal(&opts); err != nil {
				log.WithError(err).Error("Failed to merge configuration from from")
			}
		} else {
			config, err := platform.ConfigFromEnvironment()
			if err != nil {
				if err == platform.ErrNoPlatformEnvironment {
					return
				}
				logrus.WithError(err).Fatal("Failed to load platform configuration")
			}
			opts.Client.Host = config.GetPgSidecar().GetIp()
			opts.Client.Port = int(config.GetPgSidecar().GetCfg().GetPort())
			opts.Client.User, err = config.PGServiceUser()
			if err != nil {
				logrus.WithError(err).Fatal("Failed to load the pg user from the platform configuration")
			}
			opts.Client.TLSCertPath = config.GetService().GetTls().GetCertPath()
			opts.Client.TLSKeyPath = config.GetService().GetTls().GetKeyPath()
			opts.Client.TLSRootCAPath = config.GetService().GetTls().GetRootCaPath()
		}
	})

	err := newCmd().Execute()
	if err != nil {
		if eerr, ok := err.(ExitError); ok {
			fmt.Println(eerr.Error())
			os.Exit(eerr.ExitCode())
		}
		logrus.Fatal(err)
	}
}

func newCmd() *cobra.Command {
	cmd := &cobra.Command{
		Use:           "pg-helper COMMAND",
		Short:         "Automate Postgresql Helper",
		Long:          "A tool to simplify postgresql operations in Habitat hooks",
		SilenceUsage:  true,
		SilenceErrors: true,
		PersistentPreRunE: func(cmd *cobra.Command, args []string) error {
			if opts.Debug {
				logrus.SetLevel(logrus.DebugLevel)
			} else {
				logrus.SetLevel(logrus.WarnLevel)
			}

			// Disable HTTP proxying for the host we are trying to connect
			// to. Currently we assume that any user-provided proxies will
			// only be used for external services.
			err := os.Setenv("NO_PROXY", opts.Client.Host)
			if err != nil {
				return errors.Wrap(err, "could not override NO_PROXY environment variable")
			}

			return nil
		},
	}

	cmd.PersistentFlags().BoolVarP(
		&opts.Debug,
		"debug",
		"d",
		false,
		"Enabled debug output")

	cmd.PersistentFlags().StringVarP(
		&opts.CfgFile,
		"config-file",
		"",
		"",
		"The path to config file")

	cmd.PersistentFlags().StringVarP(
		&opts.Client.Host,
		"host",
		"",
		"localhost",
		"The hostname of the pg-sidecar-service")

	cmd.PersistentFlags().IntVarP(
		&opts.Client.Port,
		"port",
		"p",
		10100,
		"The listen port of the pg-sidecar-service")

	cmd.PersistentFlags().StringVarP(
		&opts.Client.TLSCertPath,
		"tls-cert",
		"c",
		"",
		"The mutual TLS certificate for pg-sidecar-server auth")

	cmd.PersistentFlags().StringVarP(
		&opts.Client.TLSKeyPath,
		"tls-key",
		"k",
		"",
		"The mutual TLS key for pg-sidecar-server auth")

	cmd.PersistentFlags().StringVarP(
		&opts.Client.TLSRootCAPath,
		"tls-root-ca",
		"r",
		"",
		"The mutual TLS root CA cert for pg-sidecar-server auth")

	cmd.PersistentFlags().IntVarP(
		&opts.Client.ConTimeout,
		"timeout",
		"t",
		1800,
		"The timeout in seconds")

	migrateCmd := &cobra.Command{
		Use:   "migrate-tables SRC_DB_NAME DST_DB_NAME OWNER_ROLE_NAME [TABLE...]",
		Short: "Migrate the table(s) from one database to another one. If no table are supplied all tables will be migrated",
		RunE: func(_ *cobra.Command, args []string) error {
			return migrateTables(args[0], args[1], args[2], args[3:])
		},
		Args: cobra.MinimumNArgs(3),
	}

	migrateCmdV2 := &cobra.Command{
		Use:   "migrate-tables-v2 SRC_DB_NAME DST_DB_NAME [TABLE...]",
		Short: "Migrate the table(s) from one database to another one. If no table are supplied all tables will be migrated",
		RunE: func(_ *cobra.Command, args []string) error {
			if opts.Client.User == "" {
				return ErrUserRequired
			}
			return migrateTables(args[0], args[1], opts.Client.User, args[2:])
		},
		Args: cobra.MinimumNArgs(2),
	}

	for _, c := range []*cobra.Command{migrateCmd, migrateCmdV2} {
		c.Flags().BoolVarP(
			&opts.Migrate.SkipDBCreate,
			"skip-db-creation", "n", false,
			"Skip the creation of the destination database",
		)

		c.Flags().BoolVar(
			&opts.Migrate.FailIfSrcMissing,
			"fail-if-src-missing", false,
			"Fail if the source database does not exist",
		)

	}

	renameCmd := &cobra.Command{
		Use:   "rename-if-exists OLD_DB_NAME NEW_DB_NAME",
		Short: "Rename OLD_DB_NAME to NEW_DB_NAME if it exists",
		RunE:  renameDBIfExists,
		Args:  cobra.ExactArgs(2),
	}

	createCmd := &cobra.Command{
		Use:   "ensure-service-database DATABASE_NAME [USER_NAME]",
		Short: "Creates a standard A2 service database and user.",
		RunE:  createDB,
		Args:  cobra.RangeArgs(1, 2),
	}

	createExtensionCmd := &cobra.Command{
		Use:   "create-extension DBNAME EXTENSION",
		Short: "Run CREATE EXTENSION as the superuser.",
		RunE:  createExtension,
		Args:  cobra.ExactArgs(2),
	}

	sqitchDeployCmd := &cobra.Command{
		Use:   "sqitch-deploy DBNAME SQITCH_DIR",
		Short: "Run sqitch deploy for the given DBNAME using the migration in SQITCH_DIR",
		RunE:  sqitchDeploy,
		Args:  cobra.ExactArgs(2),
	}

	sqitchDeployCmd.Flags().StringVarP(
		&opts.Sqitch.User,
		"user", "i", "automate",
		"User to connect with when running sqitch",
	)

	fixPermsCmd := &cobra.Command{
		Use:   "fix-permissions DBNAME [ROLENAME]",
		Short: "Change owner of all tables in the public and sqitch schemas to the given role name",
		RunE:  fixPermissions,
		Args:  cobra.RangeArgs(1, 2),
	}

	alterRoleCmd := &cobra.Command{
		Use:   "alter-role ROLENAME",
		Short: "Alter the role",
		RunE:  alterRole,
		Args:  cobra.ExactArgs(1),
	}

	alterRoleCmd.Flags().BoolVarP(
		&opts.AlterRole.Superuser,
		"superuser", "", false,
		"Add SUPERUSER",
	)

	alterRoleCmd.Flags().StringVarP(
		&opts.AlterRole.Password.Value,
		"password", "", "",
		"Add a PASSWORD",
	)

	alterRoleCmd.Flags().BoolVarP(
		&opts.AlterRole.Password.Unencrypted,
		"unencrypted", "", false,
		"Set password UNENCRYPTED",
	)

	dropTablesCmd := &cobra.Command{
		Use:   "drop-tables DBNAME [TABLE...]",
		Short: "Alter the role",
		RunE:  dropTables,
		Args:  cobra.MinimumNArgs(2),
	}

	dropTablesCmd.Flags().BoolVarP(
		&opts.DropTables.Cascade,
		"cascade", "", false,
		"Drop objects that depend on the tables",
	)

	cmd.AddCommand(migrateCmd)
	cmd.AddCommand(migrateCmdV2)
	cmd.AddCommand(renameCmd)
	cmd.AddCommand(createCmd)
	cmd.AddCommand(createExtensionCmd)
	cmd.AddCommand(sqitchDeployCmd)
	cmd.AddCommand(fixPermsCmd)
	cmd.AddCommand(alterRoleCmd)
	cmd.AddCommand(dropTablesCmd)
	return cmd
}

func pgsClient() (*client.Client, context.Context, func(), error) {
	ctx, cancel := context.WithTimeout(context.Background(), time.Duration(opts.Client.ConTimeout)*time.Second)
	cl, err := client.NewClient(
		client.WithHost(opts.Client.Host),
		client.WithPort(opts.Client.Port),
		client.WithTLSCertPath(opts.Client.TLSCertPath),
		client.WithTLSKeyPath(opts.Client.TLSKeyPath),
		client.WithTLSRootCAPath(opts.Client.TLSRootCAPath),
	)

	return cl, ctx, cancel, err
}

func renameDBIfExists(_ *cobra.Command, args []string) error {
	pgsClient, ctx, cancel, err := pgsClient()
	defer cancel()
	if err != nil {
		return err
	}

	_, err = pgsClient.RenameDB(ctx, &pgs.RenameDBReq{
		FromDb: args[0],
		ToDb:   args[1],
	})

	return err
}

// createDB sets up a "A2 standard" service database and user. It also
// accounts for possible previous mistakes such as a database user
// with a password or database tables not owned by the user.
func createDB(_ *cobra.Command, args []string) error {
	pgsClient, ctx, cancel, err := pgsClient()
	defer cancel()
	if err != nil {
		return err
	}

	user := ""
	if len(args) == 2 {
		user = args[1]
	} else {
		user = opts.Client.User
	}

	if user == "" {
		return ErrUserRequired
	}

	_, err = pgsClient.CreateDB(ctx, &pgs.CreateDBReq{
		Db:   args[0],
		User: user,
	})

	return err
}

func migrateTables(fromDB string, toDB string, importUser string, tables []string) error {
	pgsClient, ctx, cancel, err := pgsClient()
	defer cancel()
	if err != nil {
		return err
	}

	_, err = pgsClient.MigrateTables(ctx, &pgs.MigrateTablesReq{
		FromDb:             fromDB,
		ToDb:               toDB,
		ImportUser:         importUser,
		Table:              tables,
		FailIfSrcDbMissing: opts.Migrate.FailIfSrcMissing,
		SkipDbCreate:       opts.Migrate.SkipDBCreate,
	})

	if err != nil {
		st := status.Convert(err)
		for _, detail := range st.Details() {
			switch t := detail.(type) {
			// If we encountered a SrcDbMissing error then we need to exit
			// with error code 10 instead of 1 so that clients can act
			// accordingly.
			case *pgs.ErrorDetails:
				if t.Code == pgs.ErrorDetails_SrcDbMissing {
					return NewExitError(10, st.Message())
				}
			}
		}
	}

	return err
}

func fixPermissions(_ *cobra.Command, args []string) error {
	pgsClient, ctx, cancel, err := pgsClient()
	defer cancel()
	if err != nil {
		return err
	}

	user := ""
	if len(args) == 2 {
		user = args[1]
	} else {
		user = opts.Client.User
	}

	_, err = pgsClient.SetPublicSchemaRole(ctx, &pgs.SetPublicSchemaRoleReq{
		Db:   args[0],
		Role: user,
	})

	return err
}

func createExtension(_ *cobra.Command, args []string) error {
	pgsClient, ctx, cancel, err := pgsClient()
	defer cancel()
	if err != nil {
		return err
	}

	_, err = pgsClient.CreateExtension(ctx, &pgs.CreateExtensionReq{
		Db:  args[0],
		Ext: args[1],
	})

	return err
}

func sqitchDeploy(_ *cobra.Command, args []string) error {
	pgsClient, ctx, cancel, err := pgsClient()
	defer cancel()
	if err != nil {
		return err
	}

	_, err = pgsClient.DeploySqitch(ctx, &pgs.DeploySqitchReq{
		Db:   args[0],
		Dir:  args[1],
		User: opts.Sqitch.User,
	})

	return err
}

func alterRole(_ *cobra.Command, args []string) error {
	pgsClient, ctx, cancel, err := pgsClient()
	defer cancel()
	if err != nil {
		return err
	}

	_, err = pgsClient.AlterRole(ctx, &pgs.AlterRoleReq{
		Role: args[0],
		With: opts.AlterRole,
	})

	return err
}

func dropTables(_ *cobra.Command, args []string) error {
	pgsClient, ctx, cancel, err := pgsClient()
	defer cancel()
	if err != nil {
		return err
	}

	opts.DropTables.Db = args[0]
	opts.DropTables.Tables = args[1:]
	_, err = pgsClient.DropTables(ctx, opts.DropTables)

	return err
}
