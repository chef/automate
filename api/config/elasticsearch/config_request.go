package es

import (
	"fmt"
	"math"

	ac "github.com/chef/automate/api/config/shared"
	w "github.com/chef/automate/api/config/shared/wrappers"
	"github.com/chef/automate/lib/platform/sys"
)

// NewConfigRequest returns a new instance of ConfigRequest with zero values.
func NewConfigRequest() *ConfigRequest {
	return &ConfigRequest{
		V1: &ConfigRequest_V1{
			Sys: &ConfigRequest_V1_System{
				Proxy: &ConfigRequest_V1_ProxyConfig{},
				Cluster: &ConfigRequest_V1_Cluster{
					Routing: &ConfigRequest_V1_Routing{
						Allocation: &ConfigRequest_V1_Allocation{},
					},
				},
				Node: &ConfigRequest_V1_Node{},
				Path: &ConfigRequest_V1_Path{},
				Indices: &ConfigRequest_V1_Indices{
					Recovery:  &ConfigRequest_V1_Recovery{},
					Fielddata: &ConfigRequest_V1_Fielddata{},
					Breaker:   &ConfigRequest_V1_Breaker{},
				},
				Index:     &ConfigRequest_V1_Index{},
				Bootstrap: &ConfigRequest_V1_Bootstrap{},
				Network:   &ConfigRequest_V1_Network{},
				Transport: &ConfigRequest_V1_Transport{},
				Discovery: &ConfigRequest_V1_Discovery{},
				Gateway:   &ConfigRequest_V1_Gateway{},
				Action:    &ConfigRequest_V1_Action{},
				Logger:    &ConfigRequest_V1_Logger{},
				Plugins:   &ConfigRequest_V1_Plugins{},
				Runtime:   &ConfigRequest_V1_Runtime{},
				S3: &ConfigRequest_V1_S3{
					Client: &ConfigRequest_V1_S3_Client{},
				},
			},
			Svc: &ConfigRequest_V1_Service{},
		},
	}
}

// DefaultConfigRequest returns a new instance of ConfigRequest with default values.
// nolint: gomnd
func DefaultConfigRequest() *ConfigRequest {
	c := NewConfigRequest()
	sys := c.V1.Sys

	// cluster
	sys.Cluster.Name = w.String("chef-insights") // user.toml override
	sys.Cluster.MaxShardsPerNode = w.Int32(1000) // default
	sys.Cluster.Routing.Allocation.NodeConcurrentRecoveries = w.Int32(2)
	sys.Cluster.Routing.Allocation.NodeInitialPrimariesRecoveries = w.Int32(4)
	sys.Cluster.Routing.Allocation.SameShardHost = w.Bool(false)

	// node
	sys.Node.MaxLocalStorageNodes = w.Int32(1)
	sys.Node.Master = w.Bool(true)
	sys.Node.Data = w.Bool(true)

	// path
	//sys.Path.Data = ""
	sys.Path.Logs = w.String("logs")

	// indices
	sys.Indices.Recovery.MaxBytesPerSec = w.String("20mb")
	sys.Indices.Breaker.TotalLimit = w.String("70%")
	sys.Indices.Breaker.FielddataLimit = w.String("60%")
	sys.Indices.Breaker.FielddataOverhead = w.String("1.03")
	sys.Indices.Breaker.RequestLimit = w.String("40%")
	sys.Indices.Breaker.RequestOverhead = w.String("1")

	// index
	sys.Index.NumberOfReplicas = w.Int32(0)
	sys.Index.RefreshInterval = w.String("1s")

	// bootstrap
	sys.Bootstrap.MemoryLock = w.Bool(false)

	// network
	sys.Network.Port = w.Int32(10141) // user.toml override
	sys.Network.Host = w.String("")

	// transport
	sys.Transport.Port = w.String("10142") // user.toml override

	// discovery
	sys.Discovery.ZenFdPingTimeout = w.String("30s")
	// This is overridden by the bind if one is provided.
	sys.Discovery.PingUnicastHosts = w.String("[]")
	sys.Discovery.MinimumMasterNodes = w.Int32(1)

	// gateway
	sys.Gateway.ExpectedNodes = w.Int32(0)
	sys.Gateway.ExpectedMasterNodes = w.Int32(0)
	sys.Gateway.ExpectedDataNodes = w.Int32(0)

	// action
	sys.Action.DestructiveRequiresName = w.Bool(true)

	// logger
	sys.Logger.Level = w.String("info")

	// runtime
	sys.Runtime.MaxLockedMemory = w.String("unlimited")
	sys.Runtime.EsJavaOpts = w.String("")
	sys.Runtime.Heapsize = w.String(defaultHeapSize())

	// s3
	sys.S3.Client.Name = w.String("default")
	sys.S3.Client.ReadTimeout = w.String("50s")
	sys.S3.Client.MaxRetries = w.Int32(3)
	sys.S3.Client.UseThrottleRetries = w.Bool(true)

	return c
}

// Validate validates that the config is sufficient to start the Service. If
// validation succeeds it will return nil, if it fails it will return a new
// instance of config.InvalidConfigError that has the missing keys and invalid
// fields populated.
func (c *ConfigRequest) Validate() error {
	return nil
}

// PrepareSystemConfig returns a system configuration that can be used
// to start the service.
func (c *ConfigRequest) PrepareSystemConfig(creds *ac.TLSCredentials) (ac.PreparedSystemConfig, error) {
	c.V1.Sys.Tls = creds

	return c.V1.Sys, nil
}

// SetGlobalConfig injects global configuration overrides
func (c *ConfigRequest) SetGlobalConfig(g *ac.GlobalConfig) {
	// Handle external configuration
	if g.GetV1().GetExternal().GetElasticsearch().GetEnable().GetValue() {
		// Disable deploying automate-elasticsearch when we're using external es
		c.V1.Sys.Disable = w.Bool(true)

		return
	}

	if b := g.GetV1().GetBackups(); b != nil {
		if b.GetFilesystem() != nil {
			// If backups are configured to use a filesystem location make sure
			// that the path is specified in the repo, otherwise Elasticsearch
			// will not be able to access the directory.
			c.V1.Sys.Path.Repo = b.GetFilesystem().GetPath()

			if !c.GetV1().GetSys().GetNode().GetData().GetValue() {
				c.V1.Sys.Deprecated = &ConfigRequest_V1_Deprecated{
					ExternalEs: w.Bool(true),
				}
			}
		}

		if s3 := b.GetS3(); s3 != nil {
			client := c.GetV1().GetSys().GetS3().GetClient()
			if es := s3.GetEs(); es != nil {
				if rt := es.GetReadTimeout(); rt != nil {
					client.ReadTimeout = rt
				}

				if mr := es.GetMaxRetries(); mr != nil {
					client.MaxRetries = mr
				}

				if tr := es.GetUseThrottleRetries(); tr != nil {
					client.UseThrottleRetries = tr
				}
			}
		}
	}

	if logLevel := g.GetV1().GetLog().GetLevel().GetValue(); logLevel != "" {
		c.V1.Sys.Logger.Level.Value = logLevel
	}
}

const (
	// ESHeapMinGB is the minimum amount of memory in GB we will
	// recommend for ES Heap
	ESHeapMinGB = 1
	// ESHeapMaxGB is the maximum amount of memory in GB we will
	// recommend for ES Heap
	ESHeapMaxGB = 16
	// ESHeapPortion is the proportion of system memory we
	// recommend for ES Heap
	ESHeapPortion = 0.25
	// KBPerGB is the number of KB in a GB, used for conversions
	KBPerGB = 1048576
)

func defaultHeapSize() string {
	sysMem, err := sys.SystemMemoryKB()
	if err != nil {
		sysMem = 0
	}
	return fmt.Sprintf("%dg", RecommendedHeapSizeGB(sysMem))
}

// RecommendedHeapSizeGB returns the recommended size of the
// Elasticsearch heap settings. The returned value is in Gigabytes.
func RecommendedHeapSizeGB(sysMemKB int) int {
	recommend := int(math.Round((float64(sysMemKB) * ESHeapPortion) / KBPerGB))
	if recommend < ESHeapMinGB {
		return ESHeapMinGB
	} else if recommend > ESHeapMaxGB {
		return ESHeapMaxGB
	} else {
		return recommend
	}
}
