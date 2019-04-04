package shared

import (
	"testing"

	wrappers "github.com/golang/protobuf/ptypes/wrappers"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/chef/toml"
)

func TestValidate(t *testing.T) {
	t.Run("without v1 key", func(t *testing.T) {
		c := &GlobalConfig{}
		err := c.Validate()
		require.Error(t, err)
		cfgErr, ok := err.(Error)
		require.True(t, ok)
		assert.Contains(t, cfgErr.MissingKeys(), "global.v1")
	})

	t.Run("without fqdn", func(t *testing.T) {
		c := &GlobalConfig{
			V1: &V1{},
		}
		err := c.Validate()
		require.Error(t, err)
		cfgErr, ok := err.(Error)
		require.True(t, ok)
		assert.Contains(t, cfgErr.MissingKeys(), "global.v1.fqdn")
	})

	t.Run("backups with unknown location", func(t *testing.T) {
		c := &GlobalConfig{
			V1: &V1{
				Backups: &Backups{
					Location: w.String("azure"),
				},
			},
		}
		err := c.Validate()
		require.Error(t, err)
		cfgErr, ok := err.(Error)
		require.True(t, ok)
		assert.Contains(t, cfgErr.Error(), "global.v1.backups.location")
	})

	t.Run("filesystem backup without path", func(t *testing.T) {
		c := &GlobalConfig{
			V1: &V1{
				Backups: &Backups{
					Location:   w.String("filesystem"),
					Filesystem: &Backups_Filesystem{},
				},
			},
		}
		err := c.Validate()
		require.Error(t, err)
		cfgErr, ok := err.(Error)
		require.True(t, ok)
		assert.Contains(t, cfgErr.Error(), "global.v1.backups.filesystem.path")
	})

	t.Run("legacy filesystem config without location", func(t *testing.T) {
		c := &GlobalConfig{
			V1: &V1{
				Fqdn: w.String("this.is.a.host"),
				Backups: &Backups{
					Filesystem: &Backups_Filesystem{
						Path: w.String("/path/to/backups"),
					},
				},
			},
		}
		require.NoError(t, c.Validate())
	})

	t.Run("valid filesystem backup", func(t *testing.T) {
		c := &GlobalConfig{
			V1: &V1{
				Fqdn: w.String("this.is.a.host"),
				Backups: &Backups{
					Location: w.String("filesystem"),
					Filesystem: &Backups_Filesystem{
						Path: w.String("/this/is/a/path"),
					},
				},
			},
		}
		require.NoError(t, c.Validate())
	})

	t.Run("s3 backup without bucket", func(t *testing.T) {
		c := &GlobalConfig{
			V1: &V1{
				Backups: &Backups{
					Location: w.String("s3"),
				},
			},
		}
		err := c.Validate()
		require.Error(t, err)
		cfgErr, ok := err.(Error)
		require.True(t, ok)
		assert.Contains(t, cfgErr.MissingKeys(), "global.v1.backups.s3.bucket.endpoint")
		assert.Contains(t, cfgErr.MissingKeys(), "global.v1.backups.s3.bucket.name")
	})

	t.Run("s3 backup with access_key no secret_key", func(t *testing.T) {
		c := &GlobalConfig{
			V1: &V1{
				Backups: &Backups{
					Location: w.String("s3"),
					S3: &Backups_S3{
						Credentials: &Backups_S3_AWSCredentials{
							AccessKey: w.String("AKIACCESSKEY"),
						},
					},
				},
			},
		}
		err := c.Validate()
		require.Error(t, err)
		cfgErr, ok := err.(Error)
		require.True(t, ok)
		assert.Contains(t, cfgErr.MissingKeys(), "global.v1.backups.s3.credentials.secret_key")
	})

	t.Run("s3 backup with secret_key no access_key", func(t *testing.T) {
		c := &GlobalConfig{
			V1: &V1{
				Backups: &Backups{
					Location: w.String("s3"),
					S3: &Backups_S3{
						Credentials: &Backups_S3_AWSCredentials{
							SecretKey: w.String("SUPERSECRET"),
						},
					},
				},
			},
		}
		err := c.Validate()
		require.Error(t, err)
		cfgErr, ok := err.(Error)
		require.True(t, ok)
		assert.Contains(t, cfgErr.MissingKeys(), "global.v1.backups.s3.credentials.access_key")
	})

	t.Run("valid s3 backup with creds", func(t *testing.T) {
		c := &GlobalConfig{
			V1: &V1{
				Fqdn: w.String("this.is.a.host"),
				Backups: &Backups{
					Location: w.String("s3"),
					S3: &Backups_S3{
						Bucket: &Backups_S3_Bucket{
							Name:     w.String("backup-bucket"),
							Endpoint: w.String("s3.amazonaws.com"),
						},
						Credentials: &Backups_S3_AWSCredentials{
							AccessKey: w.String("AKIACCESSKEY"),
							SecretKey: w.String("SUPERSECRET"),
						},
					},
				},
			},
		}
		res := c.Validate()
		assert.Nil(t, res)
	})

	t.Run("valid s3 backup without creds", func(t *testing.T) {
		c := &GlobalConfig{
			V1: &V1{
				Fqdn: w.String("this.is.a.host"),
				Backups: &Backups{
					Location: w.String("s3"),
					S3: &Backups_S3{
						Bucket: &Backups_S3_Bucket{
							Name:     w.String("backup-bucket"),
							Endpoint: w.String("s3.amazonaws.com"),
						},
					},
				},
			},
		}
		res := c.Validate()
		assert.Nil(t, res)
	})

	t.Run("with proxy properly configured", func(t *testing.T) {
		noProxy := []string{"packages.chef.io", "api.bintray.com"}
		c := DefaultGlobalConfig()
		c.V1.Fqdn = w.String("this.is.a.host")
		c.V1.Proxy = &Proxy{
			Host:     w.String("localhost"),
			Port:     w.Int32(3128),
			User:     w.String("proxyuser"),
			Password: w.String("proxy123"),
			NoProxy:  noProxy,
		}
		res := c.Validate()
		assert.Nil(t, res)
	})

	t.Run("with valid log level", func(t *testing.T) {
		c := DefaultGlobalConfig()
		c.V1.Fqdn = w.String("this.is.a.host")
		c.V1.Log = &Log{
			Level:  w.String("fatal"),
			Format: w.String("json"),
		}
		err := c.Validate()
		require.Nil(t, err)
	})

	t.Run("with invalid log level", func(t *testing.T) {
		c := DefaultGlobalConfig()
		c.V1.Fqdn = w.String("this.is.a.host")
		c.V1.Log = &Log{Level: w.String("trace")}
		err := c.Validate()
		require.Error(t, err)
		cfgErr, ok := err.(Error)
		require.True(t, ok)
		expected := NewInvalidConfigError()
		expected.AddInvalidValue("global.v1.log.level",
			"'trace' must be one of 'debug, 'info', 'warning', 'error', 'fatal', 'panic'")
		assert.EqualError(t, cfgErr, expected.Error(), "")
	})

	t.Run("with invalid log format", func(t *testing.T) {
		c := DefaultGlobalConfig()
		c.V1.Fqdn = w.String("this.is.a.host")
		c.V1.Log = &Log{Format: w.String("xml")}
		err := c.Validate()
		require.Error(t, err)
		cfgErr, ok := err.(Error)
		require.True(t, ok)
		expected := NewInvalidConfigError()
		expected.AddInvalidValue("global.v1.log.format", "'xml' must be 'text' or 'json'")
		assert.EqualError(t, cfgErr, expected.Error(), "")
	})

	t.Run("with DefaultGlobalConfig", func(t *testing.T) {
		c := DefaultGlobalConfig()
		err := c.Validate()
		require.Error(t, err)
		cfgErr, ok := err.(Error)
		require.True(t, ok)
		assert.Contains(t, cfgErr.MissingKeys(), "global.v1.fqdn")
	})

	t.Run("DefaultGlobalConfig and FQDN", func(t *testing.T) {
		c := DefaultGlobalConfig()
		c.V1.Fqdn = w.String("this.is.a.host")
		require.NoError(t, c.Validate())
	})

	t.Run("from TOML: without v1 key", func(t *testing.T) {
		c := loadFromToml(`
keys = "not here"
`)
		err := c.Validate()
		require.Error(t, err)
		cfgErr, ok := err.(Error)
		require.True(t, ok)
		assert.Contains(t, cfgErr.MissingKeys(), "global.v1")
	})

	t.Run("from TOML: valid config", func(t *testing.T) {
		c := loadFromToml(`
[v1]
fqdn = "this.is.a.host"

[v1.proxy]
host = "172.28.5.249"
port = 3128
no_proxy = ["packages.chef.io","raw.githubusercontent.com","api.bintray.com","bldr.habitat.sh","akamai.bintray.com","dl.bintray.com","bintray.com"]

[v1.backups]
location = "filesystem"
[v1.backups.filesystem]
path = "/this/is/a/path"

[v1.log]
level = "debug"
format = "json"
`)
		res := c.Validate()
		assert.Nil(t, res)
	})

	t.Run("with mixed http and https external elasticsearch nodes", func(t *testing.T) {
		c := &GlobalConfig{
			V1: &V1{
				Fqdn: w.String("this.is.a.host"),
				External: &External{
					Elasticsearch: &External_Elasticsearch{
						Enable: w.Bool(true),
						Nodes: []*wrappers.StringValue{
							w.String("http://server1:9200"),
							w.String("https://server2:9200"),
						},
					},
				},
			},
		}
		err := c.Validate()
		require.Error(t, err)
		cfgErr, ok := err.(Error)
		require.True(t, ok)
		expected := NewInvalidConfigError()
		expected.AddInvalidValue("global.v1.external.elasticsearch.nodes", "Cannot mix http and https nodes")
		assert.EqualError(t, cfgErr, expected.Error(), "")
	})

	t.Run("with both root_cert and root_cert_file set", func(t *testing.T) {
		c := &GlobalConfig{
			V1: &V1{
				Fqdn: w.String("this.is.a.host"),
				External: &External{
					Elasticsearch: &External_Elasticsearch{
						Enable: w.Bool(true),
						Ssl: &External_Elasticsearch_SSL{
							RootCert:     w.String("rootcert"),
							RootCertFile: w.String("rootcertfile"),
						},
					},
				},
			},
		}
		err := c.Validate()
		require.Error(t, err)
		cfgErr, ok := err.(Error)
		require.True(t, ok)
		expected := NewInvalidConfigError()
		expected.AddInvalidValue("global.v1.external.elasticsearch.ssl", "Specify either global.v1.external.elasticsearch.ssl.root_cert or global.v1.external.elasticsearch.ssl.root_cert_file, but not both.")
		assert.EqualError(t, cfgErr, expected.Error(), "")
	})
}

func loadFromToml(s string) *GlobalConfig {
	c := &GlobalConfig{}
	bytes := []byte(s)
	err := toml.Unmarshal(bytes, c)
	if err != nil {
		panic(err)
	}
	return c
}
