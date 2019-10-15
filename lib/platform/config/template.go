package config

import (
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"text/template"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"

	"github.com/chef/automate/lib/secrets"
)

var errMissingPGGatewayConfig = errors.New("Missing pg-gateway config. Did you forget to bind?")

func (c *Config) builtinFuncs(opts *templateOptions) template.FuncMap {
	secretsReader := secrets.NewDiskStoreReader(opts.secretsPath)
	return template.FuncMap{
		"fail": func(msg string) (string, error) {
			return "", errors.New(msg)
		},
		"svc_path": func() string {
			return c.GetService().GetPath()
		},
		"svc_config_path": func() string {
			return path.Join(c.GetService().GetPath(), "config")
		},
		"svc_static_path": func() string {
			return path.Join(c.GetService().GetPath(), "static")
		},
		"svc_var_path": func() string {
			return path.Join(c.GetService().GetPath(), "var")
		},
		"svc_cert_path": func() string {
			return c.GetService().GetTls().GetCertPath()
		},
		"svc_key_path": func() string {
			return c.GetService().GetTls().GetKeyPath()
		},
		"svc_root_ca_path": func() string {
			return c.GetService().GetTls().GetRootCaPath()
		},
		"pg_gateway_host": func() (string, error) {
			host := c.GetPostgresql().GetIp()
			if host == "" {
				return "", errMissingPGGatewayConfig
			}
			return host, nil
		},
		"pg_gateway_port": func() (int, error) {
			port := c.GetPostgresql().GetCfg().GetPort()
			if port == 0 {
				return 0, errMissingPGGatewayConfig
			}
			return int(port), nil
		},
		"pg_svc_user": func() (string, error) {
			return c.PGServiceUser()
		},
		"is_external_pg": func() bool {
			return c.IsExternalPG()
		},
		"env": func(envVar string) string {
			val, _ := os.LookupEnv(envVar)
			return val
		},
		"pg_uri": func(dbname string) (string, error) {
			return c.GetPGURI(dbname)
		},
		"pg_root_ca_cert_path": func() string {
			return c.ExternalPGRootCertPath()
		},
		"getSecret": func(name string) (string, error) {
			secretName, err := secrets.SecretNameFromString(name)
			if err != nil {
				return "", err
			}
			data, err := secretsReader.GetSecret(secretName)
			return string(data), err
		},
	}
}

func (c *Config) TemplatesPath() string {
	return path.Join(c.GetPackage().GetPath(), "templates")
}

type templateOptions struct {
	secretsPath string
}

func defaultTemplateOptions() *templateOptions {
	return &templateOptions{
		secretsPath: secrets.DefaultDiskStoreDataDir,
	}
}

type TemplateOpt func(*templateOptions)

func WithSecretsPath(path string) TemplateOpt {
	return func(t *templateOptions) {
		t.secretsPath = path
	}
}

func (c *Config) LoadTemplates(funcs template.FuncMap, opts ...TemplateOpt) (*template.Template, error) {
	options := defaultTemplateOptions()
	for _, o := range opts {
		o(options)
	}

	t := template.New("__empty__").Funcs(c.builtinFuncs(options)).Funcs(funcs)

	templatesPath := c.TemplatesPath()
	err := filepath.Walk(templatesPath, func(p string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}

		if info.IsDir() {
			return nil
		}

		relPath, err := filepath.Rel(templatesPath, p)
		if err != nil {
			return errors.Wrapf(err, "Could not get relative path from %s to %s", templatesPath, p)
		}

		templateBytes, err := ioutil.ReadFile(p)
		if err != nil {
			return errors.Wrapf(err, "Failed to read template %s", p)
		}

		logrus.Debugf("Adding template %s", relPath)
		_, err = t.New(relPath).Parse(string(templateBytes))

		return err
	})
	if err != nil {
		return nil, err
	}
	return t, nil
}
