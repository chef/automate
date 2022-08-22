package majorupgradechecklist

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"strconv"
	"strings"

	"github.com/chef/automate/components/automate-cli/pkg/status"
	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	platform_config "github.com/chef/automate/lib/platform/config"
	"github.com/pkg/errors"
)

const (
	initMsg = `This is a Major upgrade. 
========================

  1) In this release Embedded PostgreSQL is upgraded to version 13.5 
  2) This will need special care if you use Embedded PostgreSQL. 

===== Your installation is using %s PostgreSQL =====

  Please confirm this checklist that you have taken care of these steps 
  before continuing with the Upgrade to version %s:
`

	downTimeError = "There will be a downtime while upgrading. Please prepare for down time and run the upgrade"

	backupError = "Please take a backup and restart the upgrade process."

	diskSpaceError = `Please ensure to have 60% free disk space`

	postChecklistIntimationError = "Post upgrade steps need to be run, after this upgrade completed."

	run_chef_automate_upgrade_status_cmd = `chef-automate upgrade status`
	run_chef_automate_upgrade_status     = `Check the status of your upgrade using:  
     $ ` + run_chef_automate_upgrade_status_cmd + `
   This should return: Automate is up-to-date`

	run_pg_data_migrate = `Migrate Data from PG 9.6 to PG 13.5 using this command:
     $ ` + run_pg_data_migrate_cmd

	run_pg_data_migrate_cmd = `chef-automate post-major-upgrade migrate --data=pg`

	run_chef_automate_status_cmd = `chef-automate status`
	run_chef_automate_status     = `Check all services are running using: 
     $ chef-automate status`

	run_pg_data_cleanup = `If you are sure all data is available in Upgraded Automate, then we can free up old PostgreSQL 9.6 Data by running: 
     $ ` + run_pg_data_cleanup_cmd

	run_pg_data_cleanup_cmd        = `chef-automate post-major-upgrade clear-data --data=PG`
	v3_post_checklist_confirmation = `**** In case of any errors, please refer to docs.chef.io and release notes for this version. ****

Now, upgrade will start, Please confirm to continue...`
	ui_check           = `Check Automate UI everything is running and all data is visible`
	patch_new_conf_cmd = `chef-automate config patch config.toml`
	patch_new_conf     = `If your PostgreSQL Connection URL and Credential are changed then update them by putting them in config.toml and patching it in using:
     $ chef-automate config patch config.toml`
	POST_UPGRADE_HEADER = `
Post Upgrade Steps:
===================
`
)

var postChecklistEmbedded = []PostCheckListItem{
	{
		Id:         "upgrade_status",
		Msg:        run_chef_automate_upgrade_status,
		Cmd:        run_chef_automate_upgrade_status_cmd,
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "migrate_pg",
		Msg:        run_pg_data_migrate,
		Cmd:        run_pg_data_migrate_cmd,
		IsExecuted: false,
	}, {
		Id:         "check_ui",
		Msg:        ui_check,
		Cmd:        "",
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "clean_up",
		Msg:        run_pg_data_cleanup,
		Cmd:        run_pg_data_cleanup_cmd,
		Optional:   true,
		IsExecuted: false,
	},
}

var postChecklistExternal = []PostCheckListItem{
	{
		Id:         "patch_new_config",
		Msg:        patch_new_conf,
		Cmd:        patch_new_conf_cmd,
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "upgrade_status",
		Msg:        run_chef_automate_upgrade_status,
		Cmd:        run_chef_automate_upgrade_status_cmd,
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "status",
		Msg:        run_chef_automate_status,
		Cmd:        run_chef_automate_status_cmd,
		Optional:   true,
		IsExecuted: false,
	}, {
		Id:         "check_ui",
		Msg:        ui_check,
		Cmd:        "",
		Optional:   true,
		IsExecuted: false,
	},
}

type esIndicesShardTotal struct {
	indices struct {
		shards struct {
			total int `json:"total"`
		}
	}
}

type esClusterSetting struct {
	persistent struct{}
	transient  struct{}
}

type V3ChecklistManager struct {
	writer       cli.FormatWriter
	version      string
	isExternalPG bool
}

type ESClusterSetting struct {
	Persistent struct {
	} `json:"persistent"`
	Transient struct {
	} `json:"transient"`
	Defaults struct {
		Cluster struct {
			Routing struct {
				Allocation struct {
					NodeConcurrentIncomingRecoveries string `json:"node_concurrent_incoming_recoveries"`
					NodeInitialPrimariesRecoveries   string `json:"node_initial_primaries_recoveries"`
					SameShard                        struct {
						Host string `json:"host"`
					} `json:"same_shard"`
					TotalShardsPerNode               string `json:"total_shards_per_node"`
					Type                             string `json:"type"`
					Enable                           string `json:"enable"`
					NodeConcurrentOutgoingRecoveries string `json:"node_concurrent_outgoing_recoveries"`
					AllowRebalance                   string `json:"allow_rebalance"`
					ClusterConcurrentRebalance       string `json:"cluster_concurrent_rebalance"`
					NodeConcurrentRecoveries         string `json:"node_concurrent_recoveries"`
				} `json:"allocation"`
			} `json:"routing"`
			Name             string `json:"name"`
			MaxShardsPerNode string `json:"max_shards_per_node"`
		} `json:"cluster"`

		Bootstrap struct {
			MemoryLock       string `json:"memory_lock"`
			SystemCallFilter string `json:"system_call_filter"`
			Ctrlhandler      string `json:"ctrlhandler"`
		} `json:"bootstrap"`
		Processors string `json:"processors"`
		Network    struct {
			Host []interface{} `json:"host"`
			TCP  struct {
				ReuseAddress      string `json:"reuse_address"`
				KeepAlive         string `json:"keep_alive"`
				ConnectTimeout    string `json:"connect_timeout"`
				ReceiveBufferSize string `json:"receive_buffer_size"`
				NoDelay           string `json:"no_delay"`
				SendBufferSize    string `json:"send_buffer_size"`
			} `json:"tcp"`
			BindHost []interface{} `json:"bind_host"`
			Server   string        `json:"server"`
			Breaker  struct {
				InflightRequests struct {
					Limit    string `json:"limit"`
					Overhead string `json:"overhead"`
				} `json:"inflight_requests"`
			} `json:"breaker"`
			PublishHost []interface{} `json:"publish_host"`
		} `json:"network"`
		Path struct {
			Data       []string `json:"data"`
			Logs       string   `json:"logs"`
			SharedData string   `json:"shared_data"`
			Home       string   `json:"home"`
			Repo       []string `json:"repo"`
		} `json:"path"`
		Action struct {
			AutoCreateIndex string `json:"auto_create_index"`
			Search          struct {
				ShardCount struct {
					Limit string `json:"limit"`
				} `json:"shard_count"`
			} `json:"search"`
			DestructiveRequiresName string `json:"destructive_requires_name"`
			Master                  struct {
				ForceLocal string `json:"force_local"`
			} `json:"master"`
		} `json:"action"`
		Client struct {
			Type      string `json:"type"`
			Transport struct {
				IgnoreClusterName    string `json:"ignore_cluster_name"`
				NodesSamplerInterval string `json:"nodes_sampler_interval"`
				Sniff                string `json:"sniff"`
				PingTimeout          string `json:"ping_timeout"`
			} `json:"transport"`
		} `json:"client"`
		Transport struct {
			TCP struct {
				ReuseAddress      string `json:"reuse_address"`
				ConnectTimeout    string `json:"connect_timeout"`
				Compress          string `json:"compress"`
				Port              string `json:"port"`
				NoDelay           string `json:"no_delay"`
				KeepAlive         string `json:"keep_alive"`
				ReceiveBufferSize string `json:"receive_buffer_size"`
				SendBufferSize    string `json:"send_buffer_size"`
			} `json:"tcp"`
			BindHost           []interface{} `json:"bind_host"`
			ConnectTimeout     string        `json:"connect_timeout"`
			Compress           string        `json:"compress"`
			PingSchedule       string        `json:"ping_schedule"`
			ConnectionsPerNode struct {
				Recovery string `json:"recovery"`
				State    string `json:"state"`
				Bulk     string `json:"bulk"`
				Reg      string `json:"reg"`
				Ping     string `json:"ping"`
			} `json:"connections_per_node"`
			Tracer struct {
				Include []interface{} `json:"include"`
				Exclude []string      `json:"exclude"`
			} `json:"tracer"`
			Type        string `json:"type"`
			TypeDefault string `json:"type.default"`
			Features    struct {
				XPack string `json:"x-pack"`
			} `json:"features"`
			Port        string        `json:"port"`
			Host        []interface{} `json:"host"`
			PublishPort string        `json:"publish_port"`
			TCPNoDelay  string        `json:"tcp_no_delay"`
			PublishHost []interface{} `json:"publish_host"`
			Netty       struct {
				ReceivePredictorSize string `json:"receive_predictor_size"`
				ReceivePredictorMax  string `json:"receive_predictor_max"`
				WorkerCount          string `json:"worker_count"`
				ReceivePredictorMin  string `json:"receive_predictor_min"`
				BossCount            string `json:"boss_count"`
			} `json:"netty"`
		} `json:"transport"`
		Script struct {
			AllowedContexts     []interface{} `json:"allowed_contexts"`
			MaxCompilationsRate string        `json:"max_compilations_rate"`
			Cache               struct {
				MaxSize string `json:"max_size"`
				Expire  string `json:"expire"`
			} `json:"cache"`
			Painless struct {
				Regex struct {
					Enabled string `json:"enabled"`
				} `json:"regex"`
			} `json:"painless"`
			MaxSizeInBytes string        `json:"max_size_in_bytes"`
			AllowedTypes   []interface{} `json:"allowed_types"`
		} `json:"script"`
		Node struct {
			Data                          string `json:"data"`
			EnableLuceneSegmentInfosTrace string `json:"enable_lucene_segment_infos_trace"`
			LocalStorage                  string `json:"local_storage"`
			MaxLocalStorageNodes          string `json:"max_local_storage_nodes"`
			Name                          string `json:"name"`
			ID                            struct {
				Seed string `json:"seed"`
			} `json:"id"`
			Store struct {
				AllowMmap   string `json:"allow_mmap"`
				AllowMmapfs string `json:"allow_mmapfs"`
			} `json:"store"`
			Attr struct {
				Xpack struct {
					Installed string `json:"installed"`
				} `json:"xpack"`
			} `json:"attr"`
			Portsfile string `json:"portsfile"`
			Ingest    string `json:"ingest"`
			Master    string `json:"master"`
			Ml        string `json:"ml"`
		} `json:"node"`
		Indices struct {
			Cache struct {
				CleanupInterval string `json:"cleanup_interval"`
			} `json:"cache"`
			Mapping struct {
				DynamicTimeout string `json:"dynamic_timeout"`
			} `json:"mapping"`
			Memory struct {
				Interval           string `json:"interval"`
				MaxIndexBufferSize string `json:"max_index_buffer_size"`
				ShardInactiveTime  string `json:"shard_inactive_time"`
				IndexBufferSize    string `json:"index_buffer_size"`
				MinIndexBufferSize string `json:"min_index_buffer_size"`
			} `json:"memory"`
			Breaker struct {
				Request struct {
					Limit    string `json:"limit"`
					Type     string `json:"type"`
					Overhead string `json:"overhead"`
				} `json:"request"`
				Total struct {
					Limit string `json:"limit"`
				} `json:"total"`
				Accounting struct {
					Limit    string `json:"limit"`
					Overhead string `json:"overhead"`
				} `json:"accounting"`
				Fielddata struct {
					Limit    string `json:"limit"`
					Type     string `json:"type"`
					Overhead string `json:"overhead"`
				} `json:"fielddata"`
				Type string `json:"type"`
			} `json:"breaker"`
			Query struct {
				Bool struct {
					MaxClauseCount string `json:"max_clause_count"`
				} `json:"bool"`
				QueryString struct {
					AnalyzeWildcard      string `json:"analyze_wildcard"`
					AllowLeadingWildcard string `json:"allowLeadingWildcard"`
				} `json:"query_string"`
			} `json:"query"`
		} `json:"indices"`
	} `json:"defaults"`
}

func NewV3ChecklistManager(writer cli.FormatWriter, version string) *V3ChecklistManager {
	return &V3ChecklistManager{
		writer:       writer,
		version:      version,
		isExternalPG: IsExternalPG(),
	}
}

type checkFunc func() Checklist

func preChecklist(check checkFunc) []Checklist {
	return []Checklist{
		downTimeCheck(),
		backupCheck(),
		check(),
		postChecklistIntimationCheck(),
	}
}

func IsExternalPG() bool {
	config, err := platform_config.ConfigFromParams("pg-sidecar-service", "/hab/svc/pg-sidecar-service", "")
	if err != nil {
		fmt.Println("error in config from environment")
		return false
	}
	return config.IsExternalPG()
}

func (ci *V3ChecklistManager) GetPostChecklist() []PostCheckListItem {
	var postChecklist []PostCheckListItem
	if ci.isExternalPG {
		postChecklist = postChecklistExternal
	} else {
		postChecklist = postChecklistEmbedded
	}
	return postChecklist
}

func (ci *V3ChecklistManager) RunChecklist(timeout int64) error {

	var dbType string
	checklists := []Checklist{}
	var postcheck []PostCheckListItem

	if ci.isExternalPG {
		dbType = "External"
		postcheck = postChecklistExternal
		checklists = append(checklists, preChecklist(externalPGUpgradeCheck)...)
	} else {
		dbType = "Embedded"
		postcheck = postChecklistEmbedded
		checklists = append(checklists, preChecklist(diskSpaceCheck)...)
	}
	checklists = append(checklists, showPostChecklist(&postcheck), promptUpgradeContinue())

	helper := ChecklistHelper{
		Writer: ci.writer,
	}

	ci.writer.Println(fmt.Sprintf(initMsg, dbType, ci.version)) //display the init message

	for _, item := range checklists {
		if item.TestFunc == nil {
			continue
		}
		if err := item.TestFunc(helper); err != nil {
			return errors.Wrap(err, "one of the checklist was not accepted/satisfied for upgrade")
		}
	}
	return nil
}

func showPostChecklist(postCheck *[]PostCheckListItem) Checklist {
	return Checklist{
		Name:        "Show_Post_Checklist",
		Description: "Show Post Checklist",
		TestFunc: func(h ChecklistHelper) error {
			displayed := false
			for i, item := range *postCheck {
				if !item.IsExecuted {
					if !displayed {
						h.Writer.Println(POST_UPGRADE_HEADER)
						displayed = true
					}
					h.Writer.Println(fmt.Sprintf("%d", i+1) + ") " + item.Msg + "\n")
				}
			}
			return nil
		},
	}

}

func downTimeCheck() Checklist {
	return Checklist{
		Name:        "down_time_acceptance",
		Description: "confirmation for downtime",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm("You had planned for a downtime?:")
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error(downTimeError)
				return status.New(status.InvalidCommandArgsError, downTimeError)
			}
			return nil
		},
	}
}

func backupCheck() Checklist {
	return Checklist{
		Name:        "backup_acceptance",
		Description: "confirmation check for creating a backup",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm("You have taken backup of your data and kept it safe, preferred on other disk or location?")
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error(backupError)
				return status.New(status.InvalidCommandArgsError, backupError)
			}
			return nil
		},
	}
}

func diskSpaceCheck() Checklist {
	return Checklist{
		Name:        "disk_space_acceptance",
		Description: "confirmation check for disk space",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm("Ensure you have more than 60 percent free disk space")
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error(diskSpaceError)
				return status.New(status.InvalidCommandArgsError, diskSpaceError)
			}
			return nil
		},
	}
}

func postChecklistIntimationCheck() Checklist {
	return Checklist{
		Name:        "post_checklist_intimation_acceptance",
		Description: "confirmation check for post checklist intimation",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm("After this upgrade completes, you will have to run Post upgrade steps to ensure your data is migrated and your Automate is ready for use")
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error(postChecklistIntimationError)
				return status.New(status.InvalidCommandArgsError, postChecklistIntimationError)
			}
			return nil
		},
	}
}

func externalPGUpgradeCheck() Checklist {
	return Checklist{
		Name:        "external_pg_upgrade_acceptance",
		Description: "confirmation check for external PG upgrade",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm("Upgrade your PostgreSQL 9.6 to 13.5 with the help of your Database Administrator")
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error(backupError)
				return status.New(status.InvalidCommandArgsError, downTimeError)
			}
			return nil
		},
	}
}

func promptUpgradeContinue() Checklist {
	return Checklist{
		Name:        "post_checklist",
		Description: "display post checklist and ask for final confirmation",
		TestFunc: func(h ChecklistHelper) error {
			resp, err := h.Writer.Confirm(v3_post_checklist_confirmation)
			if err != nil {
				h.Writer.Error(err.Error())
				return status.Errorf(status.InvalidCommandArgsError, err.Error())
			}
			if !resp {
				h.Writer.Error("end user not ready to upgrade")
				return status.New(status.InvalidCommandArgsError, "end user not ready to upgrade")
			}
			return nil
		},
	}
}

func getELasticSearchBasePath() string {
	var basePath = "http://localhost:10144/"

	habpath := getHabRootPath(habrootcmd)

	input, err := ioutil.ReadFile(habpath + "svc/automate-es-gateway/config/URL") // nosemgrep
	if err != nil {
		fmt.Printf("Failed to read URL file")
	}
	url := strings.TrimSuffix(string(input), "\n")
	if url != "" {
		basePath = "http://" + url + "/"
	}
	return basePath
}

func getElasticSearchClusterStas() (string, error) {
	basePath := getELasticSearchBasePath()
	indicesTotalShard, err := getDataFromUrl(basePath + "_cluster/stats?filter_path=indices.shards.total")
	if err != nil {
		return "", err
	}
	return string(indicesTotalShard), nil
}

func getElasticSearchCusterSetting() {
	basePath := getELasticSearchBasePath()
	allClusterSettings, err := getDataFromUrl(basePath + "_cluster/settings?include_defaults=true")
	if err != nil {
		return err
	}
	return nil
}

func getElasticSearchRuntimeSettings() (string, error) {
	pid, err := ioutil.ReadFile("/hab/svc/automate-elasticsearch/PID")
	if err != nil {
		return "", err
	}
	pidInt64, err := strconv.ParseInt(string(pid), 10, 64)
	if err != nil {
		return "", err
	}
	processProcFile := `/proc/` + strconv.FormatInt(pidInt64, 10) + `/stat`
	data, err := ioutil.ReadFile(processProcFile)
	if err != nil {
		return "", err
	}
	json.Unmarshal(data, ESClusterSetting{})
	return string(data), nil
}
