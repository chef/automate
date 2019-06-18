package backup

import (
	"github.com/chef/automate/lib/platform/command"
	"github.com/chef/automate/lib/stringutils"
)

// Spec describes the operations required to backup a given service
type Spec struct {
	// Name of the a2 service to be backed up
	Name string `json:"name"`

	// Version of the backup specification. If the specification changes such
	// that a backup created by the older specification cannot be properly
	// restored by the newer specification, then both must be defined and the
	// newer one should be a new version.
	Version int `json:"version"`

	// Write the backup metadata
	WriteMetadata bool `json:"metadata"`

	// Paths to back up. Broken down into which phase of the backup they
	// should run.
	SyncPaths  []PathCopyOperation `json:"sync_paths"`
	AsyncPaths []PathCopyOperation `json:"async_paths"`

	// Backup commands to run. Broken down into which phase of the backup they
	// should run.
	SyncCmds  []CommandExecuteOperation `json:"sync_cmds"`
	AsyncCmds []CommandExecuteOperation `json:"async_cmds"`

	// Databases to dump. Broken down into which phase the database dumps should
	// occur. When DB operations have been implemented they should be used here.
	SyncDbsV2  []DatabaseDumpOperationV2 `json:"sync_dbs_v2"`
	AsyncDbsV2 []DatabaseDumpOperationV2 `json:"async_dbs_v2"`

	SyncEsIndices  []ElasticsearchOperation `json:"sync_es"`
	AsyncEsIndices []ElasticsearchOperation `json:"async_es"`

	// Test operations
	testSyncOps  []testOperation
	testAsyncOps []testOperation

	// DEPRECATED
	SyncDbs  []DatabaseDumpOperation `json:"sync_dbs"`
	AsyncDbs []DatabaseDumpOperation `json:"async_dbs"`
}

// SyncOps returns a slice of Operations that should be run synchronously
func (s *Spec) SyncOps() []Operation {
	ops := []Operation{}

	for _, sp := range s.SyncPaths {
		p := sp
		ops = append(ops, &p)
	}

	for _, sc := range s.SyncCmds {
		c := sc
		ops = append(ops, &c)
	}

	for _, sc := range s.SyncDbs {
		c := sc
		ops = append(ops, &c)
	}

	for _, sc := range s.SyncDbsV2 {
		c := sc
		ops = append(ops, &c)
	}

	for _, se := range s.SyncEsIndices {
		c := se
		ops = append(ops, &c)
	}

	for _, ts := range s.testSyncOps {
		t := ts
		ops = append(ops, &t)
	}

	return ops
}

// AsyncOps returns a slice of Operations that should be run asynchronously
func (s *Spec) AsyncOps() []Operation {
	ops := []Operation{}

	for _, ap := range s.AsyncPaths {
		p := ap
		ops = append(ops, &p)
	}

	for _, ac := range s.AsyncCmds {
		c := ac
		ops = append(ops, &c)
	}

	for _, sc := range s.AsyncDbs {
		c := sc
		ops = append(ops, &c)
	}

	for _, sc := range s.AsyncDbsV2 {
		c := sc
		ops = append(ops, &c)
	}

	for _, ae := range s.AsyncEsIndices {
		c := ae
		ops = append(ops, &c)
	}

	for _, ts := range s.testAsyncOps {
		t := ts
		ops = append(ops, &t)
	}

	return ops
}

// FinalizingOps returns a slice of Operations that should be run after the
// synchronous and asynchronous data backup operations have completed.
// Currently this includes only the MetadataWriterOperation, if the relevant
// service supports it.
func (s *Spec) FinalizingOps() []Operation {
	ops := []Operation{}

	if s.WriteMetadata {
		op := &MetadataWriterOperation{
			Spec:       s,
			ObjectName: []string{s.Name},
		}
		ops = append(ops, op)
	}
	return ops
}

// DefaultSpecs returns a list of backup specifications
func DefaultSpecs(serviceNames []string) []Spec {
	specs := []Spec{
		{
			Name:          "authn-service",
			WriteMetadata: true,
			SyncDbsV2: []DatabaseDumpOperationV2{
				{
					Name: "chef_authn_service",
					User: "authn",
				},
			},
		},
		{
			Name:          "authz-service",
			WriteMetadata: true,
			SyncDbsV2: []DatabaseDumpOperationV2{
				{
					Name: "chef_authz_service",
					User: "authz",
				},
			},
		},
		{
			Name:          "automate-dex",
			WriteMetadata: true,
			SyncDbsV2: []DatabaseDumpOperationV2{
				{
					Name: "dex",
					User: "dex",
				},
			},
		},
		{
			Name:          "compliance-service",
			WriteMetadata: true,
			// The compliance service no longer writes
			// this file, but we still try to back it up
			// and restore it if it is there.
			SyncPaths: []PathCopyOperation{
				{
					Name:    "secrets_key",
					SrcPath: "/hab/svc/compliance-service/data",
					RsyncMatchers: []RsyncMatcher{
						Include("secrets_key"),
						Exclude("*"),
					},
				},
			},
			SyncDbsV2: []DatabaseDumpOperationV2{
				{
					Name: "chef_compliance_service",
					User: "compliance",
				},
			},
			SyncEsIndices: []ElasticsearchOperation{
				{
					ServiceName:    "compliance-service",
					MultiIndexSpec: "comp-*",
				},
			},
		},
		{
			Name:          "deployment-service",
			WriteMetadata: true,
			SyncPaths: []PathCopyOperation{
				{
					Name:    "config",
					SrcPath: "/hab/user/deployment-service/config",
				},
				{
					Name:    "data",
					SrcPath: "/hab/svc/deployment-service/data",
					RsyncMatchers: []RsyncMatcher{
						Exclude("bolt.db"),
						Exclude("airgap"),
						Exclude("shared"),
						Exclude("runtime"),
					},
				},
				{
					Name:    "shared",
					SrcPath: "/hab/svc/deployment-service/data/shared",
					Owner:   "hab",
				},
			},
			SyncCmds: []CommandExecuteOperation{
				{
					Name: "bolt",
					Cmd: Cmd{
						Name:    "bolt.db",
						Dump:    []string{"deployment-service", "dumpdb"},
						Restore: []string{"deployment-service", "restoredb"},
					},
				},
				{
					Name: "package-manifest",
					Cmd: Cmd{
						Name: "package-manifest.json",
						Dump: []string{"deployment-service", "package-manifest"},
						// No restore because the deployment-service bootstrap
						// will take care of it.
					},
				},
			},
		},
		{
			Name:          "license-control-service",
			WriteMetadata: true,
			SyncPaths: []PathCopyOperation{
				{
					Name:    "data",
					SrcPath: "/hab/svc/license-control-service/data",
				},
				{
					// TODO(ssd) 2018-04-16: How
					// do we encode the analogous
					// restore operation?
					Name:    "opt-out-config",
					SrcPath: "/hab/svc/license-control-service/config",
					RsyncMatchers: []RsyncMatcher{
						Include("opt_out"),
						Exclude("*"),
					},
				},
			},
		},
		{
			Name:          "teams-service",
			WriteMetadata: true,
			SyncDbsV2: []DatabaseDumpOperationV2{
				{
					Name: "chef_teams_service",
					User: "teams",
				},
			},
		},
		{
			Name:          "session-service",
			WriteMetadata: true,
			SyncDbsV2: []DatabaseDumpOperationV2{
				{
					Name: "chef_session_service",
					User: "session",
				},
			},
		},
		{
			Name:          "ingest-service",
			WriteMetadata: true,
			SyncPaths: []PathCopyOperation{
				{
					Name:    "data",
					SrcPath: "/hab/svc/ingest-service/data",
				},
			},
			SyncEsIndices: []ElasticsearchOperation{
				{
					ServiceName:    "ingest-service",
					MultiIndexSpec: "node-state-6,node-attribute,converge-history-*,actions-*",
				},
			},
		},
		{
			Name:          "notifications-service",
			WriteMetadata: true,
			SyncDbsV2: []DatabaseDumpOperationV2{
				{
					Name: "notifications_service",
					User: "notifications",
				},
			},
		},
		{
			Name:          "secrets-service",
			WriteMetadata: true,
			SyncDbsV2: []DatabaseDumpOperationV2{
				{
					Name: "secrets_service",
					User: "secrets",
				},
			},
			SyncPaths: []PathCopyOperation{
				{
					Name:    "secrets_key",
					SrcPath: "/hab/svc/secrets-service/data",
					RsyncMatchers: []RsyncMatcher{
						Include("secrets_key"),
						Exclude("*"),
					},
				},
			},
		},
		{
			Name:          "nodemanager-service",
			WriteMetadata: true,
			SyncDbsV2: []DatabaseDumpOperationV2{
				{
					Name: "nodemanager_service",
					User: "nodemanager",
				},
			},
		},
		{
			Name:          "applications-service",
			WriteMetadata: true,
		},
		// NOTE(ssd) 2018-04-16:
		// The following services don't have anything to back
		// up yet. Some of them may in the future once we have
		// runtime configuration. Although maybe that will all
		// be handled via postgresql which would be nice.
		{Name: "automate-elasticsearch", WriteMetadata: false},
		{Name: "automate-es-gateway", WriteMetadata: false},
		{Name: "automate-gateway", WriteMetadata: false},
		{Name: "event-gateway", WriteMetadata: false},
		{Name: "automate-load-balancer", WriteMetadata: false},
		{Name: "automate-postgresql", WriteMetadata: false},
		{Name: "automate-pg-gateway", WriteMetadata: false},
		{Name: "automate-ui", WriteMetadata: false},
		{Name: "backup-gateway", WriteMetadata: false},
		{Name: "config-mgmt-service", WriteMetadata: false},
		{Name: "data-lifecycle-service", WriteMetadata: false},
		{Name: "es-sidecar-service", WriteMetadata: false},
		{Name: "event-service", WriteMetadata: false},
		{Name: "local-user-service", WriteMetadata: false},
		{Name: "pg-sidecar-service", WriteMetadata: false},
		{Name: "data-feed-service", WriteMetadata: false},

		// Chef Server Stuff
		{
			Name:          "automate-cs-bookshelf",
			WriteMetadata: true,
			SyncDbsV2: []DatabaseDumpOperationV2{
				{
					Name: "automate-cs-bookshelf",
					User: "automate-cs-bookshelf",
				},
			},
		},
		{
			Name:          "automate-cs-oc-bifrost",
			WriteMetadata: true,
			SyncDbsV2: []DatabaseDumpOperationV2{
				{
					Name: "automate-cs-oc-bifrost",
					User: "automate-cs-oc-bifrost",
				},
			},
		},
		{
			Name:          "automate-cs-oc-erchef",
			WriteMetadata: true,
			SyncDbsV2: []DatabaseDumpOperationV2{
				{
					Name: "automate-cs-oc-erchef",
					User: "automate-cs-oc-erchef",
				},
			},
			SyncEsIndices: []ElasticsearchOperation{
				{
					ServiceName:    "automate-cs-oc-erchef",
					MultiIndexSpec: "chef",
				},
			},
			SyncPaths: []PathCopyOperation{
				{
					Name:    "pivotal.pem",
					SrcPath: "/hab/svc/automate-cs-oc-erchef/data",
					RsyncMatchers: []RsyncMatcher{
						Include("pivotal.pem"),
						Exclude("*"),
					},
				},
				{
					Name:    "webui_priv.pem",
					SrcPath: "/hab/svc/automate-cs-oc-erchef/data",
					RsyncMatchers: []RsyncMatcher{
						Include("webui_priv.pem"),
						Exclude("*"),
					},
				},
				{
					Name:    "webui_pub.pem",
					SrcPath: "/hab/svc/automate-cs-oc-erchef/data",
					RsyncMatchers: []RsyncMatcher{
						Include("webui_pub.pem"),
						Exclude("*"),
					},
				},
				{
					Name:    "pivotal.pub.pem",
					SrcPath: "/hab/svc/automate-cs-oc-erchef/data",
					RsyncMatchers: []RsyncMatcher{
						Include("pivotal.pub.pem"),
						Exclude("*"),
					},
				},
			},
		},
		{
			Name: "automate-cs-nginx", WriteMetadata: false,
		},

		// Workflow stuff
		{
			Name:          "automate-workflow-server",
			WriteMetadata: true,
			SyncDbsV2: []DatabaseDumpOperationV2{
				{
					Name: "chef_workflow",
					User: "workflow",
				},
			},
			SyncPaths: []PathCopyOperation{
				{
					Name:    "git-server-ssh-keys",
					SrcPath: "/hab/svc/automate-workflow-server/data",
					RsyncMatchers: []RsyncMatcher{
						Include("ssh_git_server_keys"),
						Exclude("*"),
					},
				},
				{
					Name:    "git-repos",
					SrcPath: "/hab/svc/automate-workflow-server/data/git/repos",
				},
				{
					Name:    "git-workspace",
					SrcPath: "/hab/svc/automate-workflow-server/data/git/workspace",
				},
				{
					Name:    "git-template",
					SrcPath: "/hab/svc/automate-workflow-server/files/git_repo_template",
				},
				{
					Name:    "builder_key",
					SrcPath: "/hab/svc/automate-workflow-server/var/etc",
					RsyncMatchers: []RsyncMatcher{
						Include("builder_key"),
						Exclude("*"),
					},
				},
				{
					Name:    "builder_key.pub",
					SrcPath: "/hab/svc/automate-workflow-server/var/etc",
					RsyncMatchers: []RsyncMatcher{
						Include("builder_key.pub"),
						Exclude("*"),
					},
				},
				{
					Name:    "delivery.pem",
					SrcPath: "/hab/svc/automate-workflow-server/var/etc",
					RsyncMatchers: []RsyncMatcher{
						Include("delivery.pem"),
						Exclude("*"),
					},
				},
			},
		},
		{
			Name: "automate-workflow-nginx", WriteMetadata: false,
		},
	}

	for _, spec := range specs {
		setDefaults(spec)
	}

	cmd := command.NewExecExecutor()
	for _, spec := range specs {
		SetCommandExecutor(spec, cmd)
	}

	filtered := []Spec{}
	for _, defaultSpec := range specs {
		if defaultSpec.Name == "deployment-service" || stringutils.SliceContains(serviceNames, defaultSpec.Name) {
			filtered = append(filtered, defaultSpec)
		}
	}
	return filtered
}

func setDefaults(spec Spec) {
	for i := range spec.SyncPaths {
		spec.SyncPaths[i].ObjectName = []string{spec.Name, spec.SyncPaths[i].Name}
	}
	for i := range spec.AsyncPaths {
		spec.AsyncPaths[i].ObjectName = []string{spec.Name, spec.AsyncPaths[i].Name}
	}
	for i := range spec.SyncCmds {
		spec.SyncCmds[i].ObjectName = []string{spec.Name, spec.SyncCmds[i].Name}
		spec.SyncCmds[i].PkgOrigin = "chef"
		spec.SyncCmds[i].PkgName = spec.Name
	}

	for i := range spec.AsyncCmds {
		spec.AsyncCmds[i].ObjectName = []string{spec.Name, spec.AsyncCmds[i].Name}
		spec.AsyncCmds[i].PkgOrigin = "chef"
		spec.AsyncCmds[i].PkgName = spec.Name
	}

	for i := range spec.SyncDbs {
		spec.SyncDbs[i].ObjectName = []string{spec.Name, "pg_data"}
	}

	for i := range spec.AsyncDbs {
		spec.AsyncDbs[i].ObjectName = []string{spec.Name, "pg_data"}
	}

	for i := range spec.SyncDbsV2 {
		spec.SyncDbsV2[i].ObjectName = []string{spec.Name, "pg_data"}
	}

	for i := range spec.AsyncDbsV2 {
		spec.AsyncDbsV2[i].ObjectName = []string{spec.Name, "pg_data"}
	}
}

// SetCommandExecutor sets the command executor for a given spec
func SetCommandExecutor(spec Spec, exec command.Executor) {
	for i := range spec.SyncPaths {
		spec.SyncPaths[i].cmdExecutor = exec
	}
	for i := range spec.AsyncPaths {
		spec.AsyncPaths[i].cmdExecutor = exec
	}
	for i := range spec.SyncCmds {
		spec.SyncCmds[i].cmdExecutor = exec
	}

	for i := range spec.AsyncCmds {
		spec.AsyncCmds[i].cmdExecutor = exec
	}

	for i := range spec.SyncDbs {
		spec.SyncDbs[i].cmdExecutor = exec
	}

	for i := range spec.AsyncDbs {
		spec.AsyncDbs[i].cmdExecutor = exec
	}

	for i := range spec.SyncDbsV2 {
		spec.SyncDbsV2[i].cmdExecutor = exec
	}

	for i := range spec.AsyncDbsV2 {
		spec.AsyncDbsV2[i].cmdExecutor = exec
	}
}
