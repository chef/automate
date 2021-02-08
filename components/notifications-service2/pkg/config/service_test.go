package config

import (
	"io/ioutil"
	"os"
	"testing"

	log "github.com/sirupsen/logrus"

	"github.com/chef/automate/components/automate-deployment/pkg/toml"
	"github.com/chef/automate/lib/tls/certs"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

func TestConfigMarshaling(t *testing.T) {
	original := &Notifications{
		Service: &Service{
			ExternalFQDN: "automate-fe.example",
			Host:         "localhost",
			Port:         1234,
			LogLevel:     "debug",
		},
		Postgres: &Postgres{
			URI:          "postgresql://notifications@127.0.0.1:10145/notifications_service?opts=vals",
			Database:     "notifications_service",
			SchemaPath:   "/hab/pkgs/example/notifications-service/2.0.0/20210204232203/schema",
			MaxOpenConns: 15,
			MaxIdleConns: 17,
		},
		Secrets: &Secrets{
			Host: "secrets.example",
			Port: 4567,
		},
		TLSConfig: &certs.TLSConfig{
			CertPath:       "/hab/svc/notifications-service/config/service.crt",
			KeyPath:        "/hab/svc/notifications-service/config/service.key",
			RootCACertPath: "/hab/svc/notifications-service/config/root_ca.crt",
		},
	}
	configBytes, err := toml.Marshal(original)
	require.NoError(t, err)

	f, err := ioutil.TempFile("", "notifications_service_config_test*.toml")
	require.NoError(t, err)
	defer os.Remove(f.Name())
	_, err = f.Write(configBytes)
	require.NoError(t, err)
	err = f.Close()
	require.NoError(t, err)

	unmarshaled, err := MarshalFromFile(f.Name())
	require.NoError(t, err)
	assert.Equal(t, original, unmarshaled)
}

func TestConfigLogLevel(t *testing.T) {
	levelBeforeTest := log.GetLevel()
	defer log.SetLevel(levelBeforeTest)

	validLogLevels := []string{
		"trace",
		"debug",
		"info",
		"error",
		"warning",
		"fatal",
		"panic",
	}
	for _, levelToSet := range validLogLevels {
		n := &Notifications{
			Service: &Service{
				LogLevel: levelToSet,
			},
		}
		err := n.SetLogLevel()
		require.NoError(t, err)
		assert.Equal(t, levelToSet, log.GetLevel().String())
	}

	nInvalid := &Notifications{
		Service: &Service{
			LogLevel: "bork",
		},
	}

	err := nInvalid.SetLogLevel()
	require.Error(t, err)
}

func TestConfigReadCerts(t *testing.T) {
	nValid := &Notifications{
		TLSConfig: &certs.TLSConfig{
			CertPath:       "/src/dev/certs/notifications-service.crt",
			KeyPath:        "/src/dev/certs/notifications-service.key",
			RootCACertPath: "/src/dev/certs/Chef_Automate_FAKE_Dev.crt",
		},
	}
	err := nValid.ReadCerts()
	require.NoError(t, err)

}
