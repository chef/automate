package esgateway

import (
	"testing"

	wrappers "github.com/golang/protobuf/ptypes/wrappers"
	"github.com/stretchr/testify/assert"

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

		assert.Equal(t, "server1:9200", c.GetV1().GetSys().GetExternal().GetEndpoints()[0].GetValue())
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

		assert.Equal(t, "server1:9200", c.GetV1().GetSys().GetExternal().GetEndpoints()[0].GetValue())
		assert.Equal(t, "server2:9200", c.GetV1().GetSys().GetExternal().GetEndpoints()[1].GetValue())
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

		assert.Equal(t, "server1:9200", c.GetV1().GetSys().GetExternal().GetEndpoints()[0].GetValue())
	})
}
