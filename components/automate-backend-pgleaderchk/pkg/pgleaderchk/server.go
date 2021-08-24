package pgleaderchk

import (
	"database/sql"
	"encoding/json"
	"fmt"
	_ "github.com/lib/pq"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"github.com/spf13/viper"
	"net/http"
)

type Config struct {
	Db   DatabaseConfig `mapstructure:"database"`
	Http HttpConfig     `mapstructure:"httpd"`
	Log  LogConfig      `mapstructure:"log"`
}

type HttpConfig struct {
	Hostname string `mapstructure:"bind" toml:"bind"`
	Port     int    `mapstructure:"port" toml:"port"`
}

type LogConfig struct {
	Level string `mapstructure:"level" toml:"level"`
}

type DatabaseConfig struct {
	Hostname string `mapstructure:"hostname" toml:"hostname"`
	Port     int    `mapstructure:"port" toml:"port"`
	Database string `mapstructure:"dbname" toml:"dbname"`
	User     string `mapstructure:"username" toml:"username"`
	Password string `mapstructure:"password" toml:"password"`
	Sslmode  string `mapstructure:"sslmode" toml:"sslmode"`
}

// Server holds the state of an instance of this service
type Server struct {
	httpListenHost   string
	httpListenPort   int
	dbConnectHost    string
	dbConnectPort    int
	dbConnectUser    string
	dbConnectPass    string
	dbConnectDb      string
	dbConnectSslmode string
}

type ResponseMessage struct {
	Status string
	Body   string
}

// ConfigFromViper returns a Server config from the service's configuration
// file and the viper CLI arguments.
func ConfigFromViper() (*Config, error) {
	config := &Config{}

	// Unmarshall the viper config into the server Config
	if err := viper.Unmarshal(config); err != nil {
		log.WithFields(log.Fields{
			"err": err,
		}).Error("Failed to marshall config options to server config")
		return config, err
	}

	return config, nil
}

// NewFromConfig initializes a Server given a Config
func NewFromConfig(cfg *Config) (*Server, error) {
	opts := []Opts{
		WithURI(cfg.Http.Hostname, cfg.Http.Port),
		WithDB(cfg.Db.Hostname, cfg.Db.Port, cfg.Db.User, cfg.Db.Password, cfg.Db.Database, cfg.Db.Sslmode),
		WithLogLevel(cfg.Log.Level),
	}
	return New(opts...), nil
}

// New initializes a *Server from the passed options
func New(opts ...Opts) *Server {
	s := &Server{}
	for _, opt := range opts {
		opt(s)
	}
	return s
}

type Opts func(*Server)

// WithURI allows setting the URI to use from hostname and port
func WithURI(hostname string, port int) Opts {
	return func(s *Server) {
		s.httpListenHost = hostname
		s.httpListenPort = port
	}
}

// WithDB allows setting the URI to use from hostname and port
func WithDB(hostname string, port int, user string, password string, dbname string, sslmode string) Opts {
	return func(s *Server) {
		s.dbConnectHost    = hostname
		s.dbConnectPort    = port
		s.dbConnectUser    = user
		s.dbConnectPass    = password
		s.dbConnectDb      = dbname
		s.dbConnectSslmode = sslmode
	}
}

// WithLogLevel sets the log level to use.
func WithLogLevel(lvl string) Opts {
	return func(*Server) {
		l, err := log.ParseLevel(lvl)
		if err != nil {
			log.Warnf("unknown log level %q, using default (info)", lvl)
			l = log.InfoLevel
		}
		log.Infof("setting log level %s", lvl)
		log.SetLevel(l)
	}
}

func defaultHandler(s *Server) func(w http.ResponseWriter, r *http.Request) {
	return func(w http.ResponseWriter, r *http.Request) {
		// Set security headers
		w.Header().Set("X-Frame-Option", "SAMEORIGIN")

		psqlInfo := fmt.Sprintf("host=%s port=%d user=%s "+
			"password=%s dbname=%s sslmode=%s",
			s.dbConnectHost, s.dbConnectPort, s.dbConnectUser, s.dbConnectPass, s.dbConnectDb, s.dbConnectSslmode)

		var errs []error
		var isFollower bool
		isFollower = false

		db, err := sql.Open(s.dbConnectDb, psqlInfo)
		if err != nil {
			errs = append(errs, errors.Wrapf(err, "sql.Open failed with: %s", err))
		}
		defer db.Close()

		err = db.Ping()
		if err != nil {
			errs = append(errs, errors.Wrapf(err, "db.Ping failed with: %s", err))
		} else {

			rows, err := db.Query("SELECT pg_is_in_recovery()")
			if err != nil {
				errs = append(errs, errors.Wrapf(err, "SELECT pg_is_in_recovery() failed with: %s", err))
			}

			for rows.Next() {
				err = rows.Scan(&isFollower)
				if err != nil {
					errs = append(errs, errors.Wrapf(err, "rows.Scan failed with: %s", err))
				}
			}

		}

		msg := ResponseMessage{"ok", "this node is the leader"}

		// Make sure to set headers before calling WriteHeader
		// otherwise this doesn't get sent
		w.Header().Set("Content-Type", "application/json")

		l := len(errs)
		if l > 0 || isFollower {
			for _, element := range errs {
				log.Error(element)
			}
			w.WriteHeader(http.StatusServiceUnavailable)
			msg = ResponseMessage{"critical", "this node is not a functioning leader"}
		} else {
			w.WriteHeader(http.StatusOK)
		}
		js, err := json.Marshal(msg)
		if err != nil {
			log.Errorf("Error marshalling json %s", err)
			http.Error(w, err.Error(), http.StatusInternalServerError)
			return
		}

		w.Write(js)

		log.WithFields(log.Fields{
			"Status": msg.Status,
			"Body":   msg.Body,
		}).Debug("server response")
	}
}

func healthzHandler(w http.ResponseWriter, r *http.Request) {
	msg := ResponseMessage{"ok", "the service is running"}
	js, err := json.Marshal(msg)
	if err != nil {
		log.Errorf("Error marshalling json %s", err)
		http.Error(w, err.Error(), http.StatusInternalServerError)
		return
	}

	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("X-Frame-Option", "SAMEORIGIN")
	w.WriteHeader(http.StatusOK)
	w.Write(js)

	log.WithFields(log.Fields{
		"Status": msg.Status,
		"Body":   msg.Body,
	}).Debug("server response")
}

// Serve finalizes the Server setup, and listens for connections. Only
// returns if something went wrong, with a non-nil error.
func (s *Server) Serve() error {
	log.WithFields(log.Fields{
		"http-host":  s.httpListenHost,
		"http-port":  s.httpListenPort,
		"db-host":    s.dbConnectHost,
		"db-port":    s.dbConnectPort,
		"db-user":    s.dbConnectUser,
		"db-dbname":  s.dbConnectDb,
		"db-sslmode": s.dbConnectSslmode,
	}).Info("Starting server")

	// start server
	uri := fmt.Sprintf("%s:%d", s.httpListenHost, s.httpListenPort)
	http.HandleFunc("/", defaultHandler(s))
	http.HandleFunc("/healthz", healthzHandler)
	err := http.ListenAndServe(uri, nil)
	if err != nil {
		log.Fatal("ListenAndServe: ", err)
		return err
	}
	return nil
}
