package config_test

import (
	"fmt"
	"testing"

	"github.com/chef/automate/components/data-feed-service/config"
	"github.com/stretchr/testify/assert"
)

func TestMaskPGCredInURI(t *testing.T) {
	configData := &config.DataFeedConfig{
		CerealConfig:   config.CerealConfig{Target: "cereal://user:pass@127.0.01:3333"},
		PostgresConfig: config.PostgresConfig{ConnectionString: "postgresql://user:pass@127.0.0.1:10145"},
	}
	fullConfig := fmt.Sprintf("DATA FEED SERVICE CONFIG: %+v", configData)
	tests := []struct {
		source   string
		expected string
	}{
		{source: "postgresql://user:pass@127.0.0.1:10145", expected: "postgresql://<USER>:<PASSWORD>@127.0.0.1:10145"},
		{source: "postgresql://user:@127.0.0.1:10145", expected: "postgresql://<USER>:<PASSWORD>@127.0.0.1:10145"},
		{source: "postgresql://:pass@127.0.0.1:10145", expected: "postgresql://<USER>:<PASSWORD>@127.0.0.1:10145"},
		{source: "postgresql://127.0.0.1:10145", expected: "postgresql://127.0.0.1:10145"},
		{source: "postgresql://:@127.0.0.1:10145", expected: "postgresql://<USER>:<PASSWORD>@127.0.0.1:10145"},
		{source: "postgresql://user:pass127.0.0.1:10145", expected: "postgresql://user:pass127.0.0.1:10145"},
		{source: fullConfig, expected: "DATA FEED SERVICE CONFIG: &{ServiceConfig:{Host: Port:0 FeedInterval:0s AssetPageSize:0 ReportsPageSize:0 NodeBatchSize:0 UpdatedNodesOnly:false DisableCIDRFilter:false CIDRFilter: ExternalFqdn: AcceptedStatusCodes:[] ContentType:} LogConfig:{LogLevel: LogFormat:} TLSConfig:{CertPath: KeyPath: RootCACertPath:} SecretsConfig:{Target:} CfgmgmtConfig:{Target:} ComplianceConfig:{Target:} CerealConfig:{Target:cereal://user:pass@127.0.01:3333} PostgresConfig:{ConnectionString:postgresql://<USER>:<PASSWORD>@127.0.0.1:10145 Database: MigrationsPath:} ServiceCerts:<nil>}"},
	}

	for _, test := range tests {
		actual := config.MaskPGCredInURI(test.source)

		assert.Equal(t, test.expected, actual)
	}
}
