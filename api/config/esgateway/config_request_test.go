package esgateway

import (
	"testing"

	wrappers "github.com/golang/protobuf/ptypes/wrappers"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	ac "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
)

func TestExternalElasticsearch(t *testing.T) {
	t.Run("single http endpoint", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.SetGlobalConfig(&ac.GlobalConfig{
			V1: &ac.V1{
				External: &ac.External{
					Elasticsearch: &ac.External_Elasticsearch{
						Enable: w.Bool(true),
						Nodes: []*wrappers.StringValue{
							w.String("http://server1:9200"),
						},
					},
				},
			},
		})

		assert.True(t,
			c.GetV1().GetSys().GetExternal().GetEnable().GetValue(),
			"expected external ES to be enabled")

		assert.False(t,
			c.GetV1().GetSys().GetExternal().GetSslUpstream().GetValue(),
			"expected http")

		assert.Equal(t, "server1", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetAddress().GetValue())
		assert.Equal(t, "9200", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetPort().GetValue())
		assert.Equal(t, true, c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetIsDomain().GetValue())

	})

	t.Run("multiple http endpoints", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.SetGlobalConfig(&ac.GlobalConfig{
			V1: &ac.V1{
				External: &ac.External{
					Elasticsearch: &ac.External_Elasticsearch{
						Enable: w.Bool(true),
						Nodes: []*wrappers.StringValue{
							w.String("http://server1:9200"),
							w.String("http://server2:9200"),
						},
					},
				},
			},
		})

		assert.True(t,
			c.GetV1().GetSys().GetExternal().GetEnable().GetValue(),
			"expected external ES to be enabled")

		assert.False(t,
			c.GetV1().GetSys().GetExternal().GetSslUpstream().GetValue(),
			"expected http")

		assert.Equal(t, "server1", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetAddress().GetValue())
		assert.Equal(t, "9200", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetPort().GetValue())

		assert.Equal(t, "server2", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[1].GetAddress().GetValue())
		assert.Equal(t, "9200", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[1].GetPort().GetValue())

	})

	t.Run("single https endpoint", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.SetGlobalConfig(&ac.GlobalConfig{
			V1: &ac.V1{
				External: &ac.External{
					Elasticsearch: &ac.External_Elasticsearch{
						Enable: w.Bool(true),
						Nodes: []*wrappers.StringValue{
							w.String("https://server1:9200"),
						},
					},
				},
			},
		})

		assert.True(t,
			c.GetV1().GetSys().GetExternal().GetEnable().GetValue(),
			"expected external ES to be enabled")

		assert.True(t,
			c.GetV1().GetSys().GetExternal().GetSslUpstream().GetValue(),
			"expected http")

		assert.Equal(t, "server1", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetAddress().GetValue())
		assert.Equal(t, "9200", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetPort().GetValue())

	})

	t.Run("single http endpoint with default port", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.SetGlobalConfig(&ac.GlobalConfig{
			V1: &ac.V1{
				External: &ac.External{
					Elasticsearch: &ac.External_Elasticsearch{
						Enable: w.Bool(true),
						Nodes: []*wrappers.StringValue{
							w.String("http://server1"),
						},
					},
				},
			},
		})

		assert.True(t,
			c.GetV1().GetSys().GetExternal().GetEnable().GetValue(),
			"expected external ES to be enabled")

		assert.False(t,
			c.GetV1().GetSys().GetExternal().GetSslUpstream().GetValue(),
			"expected http")

		assert.Equal(t, "server1", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetAddress().GetValue())
		assert.Equal(t, "80", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetPort().GetValue())

	})

	t.Run("single https endpoint with default port", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.SetGlobalConfig(&ac.GlobalConfig{
			V1: &ac.V1{
				External: &ac.External{
					Elasticsearch: &ac.External_Elasticsearch{
						Enable: w.Bool(true),
						Nodes: []*wrappers.StringValue{
							w.String("https://server1"),
						},
					},
				},
			},
		})

		assert.True(t,
			c.GetV1().GetSys().GetExternal().GetEnable().GetValue(),
			"expected external ES to be enabled")

		assert.True(t,
			c.GetV1().GetSys().GetExternal().GetSslUpstream().GetValue(),
			"expected https")

		assert.Equal(t, "server1", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetAddress().GetValue())
		assert.Equal(t, "443", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetPort().GetValue())
	})

	t.Run("single http endpoint with IP", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.SetGlobalConfig(&ac.GlobalConfig{
			V1: &ac.V1{
				External: &ac.External{
					Elasticsearch: &ac.External_Elasticsearch{
						Enable: w.Bool(true),
						Nodes: []*wrappers.StringValue{
							w.String("http://10.0.0.1:1120"),
						},
					},
				},
			},
		})

		assert.True(t,
			c.GetV1().GetSys().GetExternal().GetEnable().GetValue(),
			"expected external ES to be enabled")

		assert.False(t,
			c.GetV1().GetSys().GetExternal().GetSslUpstream().GetValue(),
			"expected http")

		assert.Equal(t, "10.0.0.1", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetAddress().GetValue())
		assert.Equal(t, "1120", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetPort().GetValue())
		assert.Equal(t, false, c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetIsDomain().GetValue())
	})

	t.Run("single http endpoint with IP and default port", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.SetGlobalConfig(&ac.GlobalConfig{
			V1: &ac.V1{
				External: &ac.External{
					Elasticsearch: &ac.External_Elasticsearch{
						Enable: w.Bool(true),
						Nodes: []*wrappers.StringValue{
							w.String("http://10.0.0.1"),
						},
					},
				},
			},
		})

		assert.True(t,
			c.GetV1().GetSys().GetExternal().GetEnable().GetValue(),
			"expected external ES to be enabled")

		assert.False(t,
			c.GetV1().GetSys().GetExternal().GetSslUpstream().GetValue(),
			"expected http")

		assert.Equal(t, "10.0.0.1", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetAddress().GetValue())
		assert.Equal(t, "80", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetPort().GetValue())
		assert.Equal(t, false, c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetIsDomain().GetValue())
	})

	t.Run("single https endpoint with IP", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.SetGlobalConfig(&ac.GlobalConfig{
			V1: &ac.V1{
				External: &ac.External{
					Elasticsearch: &ac.External_Elasticsearch{
						Enable: w.Bool(true),
						Nodes: []*wrappers.StringValue{
							w.String("https://10.0.0.1:1120"),
						},
					},
				},
			},
		})

		assert.True(t,
			c.GetV1().GetSys().GetExternal().GetEnable().GetValue(),
			"expected external ES to be enabled")

		assert.True(t,
			c.GetV1().GetSys().GetExternal().GetSslUpstream().GetValue(),
			"expected https")

		assert.Equal(t, "10.0.0.1", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetAddress().GetValue())
		assert.Equal(t, "1120", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetPort().GetValue())
		assert.Equal(t, false, c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetIsDomain().GetValue())
	})

	t.Run("single https endpoint with IP and default port", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.SetGlobalConfig(&ac.GlobalConfig{
			V1: &ac.V1{
				External: &ac.External{
					Elasticsearch: &ac.External_Elasticsearch{
						Enable: w.Bool(true),
						Nodes: []*wrappers.StringValue{
							w.String("https://10.0.0.1"),
						},
					},
				},
			},
		})

		assert.True(t,
			c.GetV1().GetSys().GetExternal().GetEnable().GetValue(),
			"expected external ES to be enabled")

		assert.True(t,
			c.GetV1().GetSys().GetExternal().GetSslUpstream().GetValue(),
			"expected https")

		assert.Equal(t, "10.0.0.1", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetAddress().GetValue())
		assert.Equal(t, "443", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetPort().GetValue())
		assert.Equal(t, false, c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetIsDomain().GetValue())
	})

	t.Run("setting platformconfig for nameservers", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.V1.Sys.Ngx.Main.Resolvers.Nameservers = []*wrappers.StringValue{w.String("111.11.11.11:50")}
		c.PrepareSystemConfig(&ac.TLSCredentials{})

		require.Equal(t, c.V1.Sys.Ngx.Main.Resolvers.NameserversString.GetValue(), "111.11.11.11:50", "does not match with the nameserver passed")
	})

	t.Run("aws elasticsearch", func(t *testing.T) {
		c := DefaultConfigRequest()
		c.SetGlobalConfig(&ac.GlobalConfig{
			V1: &ac.V1{
				External: &ac.External{
					Elasticsearch: &ac.External_Elasticsearch{
						Enable: w.Bool(true),
						Nodes: []*wrappers.StringValue{
							w.String("https://server1"),
						},
						Auth: &ac.External_Elasticsearch_Authentication{
							Scheme: w.String("aws_es"),
							AwsEs: &ac.External_Elasticsearch_Authentication_AwsElasticsearchAuth{
								Username: w.String("testuser"),
								Password: w.String("testpassword"),
							},
						},
					},
				},
			},
		})

		require.True(t,
			c.GetV1().GetSys().GetExternal().GetEnable().GetValue(),
			"expected external ES to be enabled")

		require.Equal(t, "server1", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetAddress().GetValue())
		require.Equal(t, "443", c.GetV1().GetSys().GetExternal().GetParsedEndpoints()[0].GetPort().GetValue())
		require.Equal(t, "server1", c.GetV1().GetSys().GetNgx().GetHttp().GetProxySetHeaderHost().GetValue())

	})
}
