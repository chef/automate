package backup

import (
	"reflect"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/lib/platform/command"
)

// TestDefaultSpecs tests that all service specs have their desired backup
// operations.
func TestDefaultSpecs(t *testing.T) {
	t.Run("authn-service", func(t *testing.T) {
		spec := testSpecFor(t, "authn-service")
		testRequireMetadata(t, spec)
		testRequireDb(t, spec, "sync", "chef_authn_service", "authn")
	})
	t.Run("authz-service", func(t *testing.T) {
		spec := testSpecFor(t, "authz-service")
		testRequireMetadata(t, spec)
		testRequireDb(t, spec, "sync", "chef_authz_service", "authz")
	})
	t.Run("automate-dex", func(t *testing.T) {
		spec := testSpecFor(t, "automate-dex")
		testRequireMetadata(t, spec)
		testRequireDb(t, spec, "sync", "dex", "dex")
	})
	t.Run("automate-elasticsearch", func(t *testing.T) {
		testSpecFor(t, "automate-elasticsearch")
	})
	t.Run("automate-gateway", func(t *testing.T) {
		testSpecFor(t, "automate-gateway")
	})
	t.Run("automate-load-balancer", func(t *testing.T) {
		testSpecFor(t, "automate-load-balancer")
	})
	t.Run("automate-postgresql", func(t *testing.T) {
		testSpecFor(t, "automate-postgresql")
	})
	t.Run("automate-ui", func(t *testing.T) {
		testSpecFor(t, "automate-ui")
	})
	t.Run("compliance-service", func(t *testing.T) {
		spec := testSpecFor(t, "compliance-service")
		testRequireMetadata(t, spec)
		testRequireDb(t, spec, "sync", "chef_compliance_service", "compliance")
		testRequireEs(t, spec, "sync", "compliance-service", "comp-*")
	})
	t.Run("event-feed-service", func(t *testing.T) {
		spec := testSpecFor(t, "event-feed-service")
		testRequireMetadata(t, spec)
		testRequireEs(t, spec, "sync", "event-feed-service", "event-feed-*")
	})
	t.Run("config-mgmt-service", func(t *testing.T) {
		testSpecFor(t, "config-mgmt-service")
	})
	t.Run("data-lifecycle-service", func(t *testing.T) {
		testSpecFor(t, "data-lifecycle-service")
	})
	t.Run("deployment-service", func(t *testing.T) {
		spec := testSpecFor(t, "deployment-service")
		testRequireMetadata(t, spec)
		testRequirePath(t, spec, "sync", "/hab/user/deployment-service/config")
		testRequirePath(t, spec, "sync", "/hab/svc/deployment-service/data", Exclude("bolt.db"))
		testRequireCmd(t, spec, "sync", "deployment-service dumpdb", "deployment-service restoredb")
	})
	t.Run("es-sidecar-service", func(t *testing.T) {
		testSpecFor(t, "es-sidecar-service")
	})
	t.Run("ingest-service", func(t *testing.T) {
		spec := testSpecFor(t, "ingest-service")
		testRequireMetadata(t, spec)
		testRequirePath(t, spec, "sync", "/hab/svc/ingest-service/data")
		testRequireEs(t, spec, "sync", "ingest-service", "node-state-6,node-attribute,converge-history-*,actions-*")
	})
	t.Run("license-control-service", func(t *testing.T) {
		spec := testSpecFor(t, "license-control-service")
		testRequireMetadata(t, spec)
		testRequirePath(t, spec, "sync", "/hab/svc/license-control-service/data")
		testRequirePath(t, spec, "sync", "/hab/svc/license-control-service/config", Include("opt_out"), Exclude("*"))
	})
	t.Run("local-user-service", func(t *testing.T) {
		testSpecFor(t, "local-user-service")
	})
	t.Run("notifications-service", func(t *testing.T) {
		spec := testSpecFor(t, "notifications-service")
		testRequireMetadata(t, spec)
		testRequireDb(t, spec, "sync", "notifications_service", "notifications")
	})
	t.Run("teams-service", func(t *testing.T) {
		spec := testSpecFor(t, "teams-service")
		testRequireMetadata(t, spec)
		testRequireDb(t, spec, "sync", "chef_teams_service", "teams")
	})
	t.Run("session-service", func(t *testing.T) {
		spec := testSpecFor(t, "session-service")
		testRequireMetadata(t, spec)
		testRequireDb(t, spec, "sync", "chef_session_service", "session")
	})
	t.Run("automate-cs-bookshelf", func(t *testing.T) {
		spec := testSpecFor(t, "automate-cs-bookshelf")
		testRequireMetadata(t, spec)
		testRequireDb(t, spec, "sync", "automate-cs-bookshelf", "automate-cs-bookshelf")
	})
	t.Run("automate-cs-oc-bifrost", func(t *testing.T) {
		spec := testSpecFor(t, "automate-cs-oc-bifrost")
		testRequireMetadata(t, spec)
		testRequireDb(t, spec, "sync", "automate-cs-oc-bifrost", "automate-cs-oc-bifrost")
	})
	t.Run("automate-cs-oc-erchef", func(t *testing.T) {
		spec := testSpecFor(t, "automate-cs-oc-erchef")
		testRequireMetadata(t, spec)
		testRequireDb(t, spec, "sync", "automate-cs-oc-erchef", "automate-cs-oc-erchef")
		testRequireEs(t, spec, "sync", "automate-cs-oc-erchef", "chef")
	})
	t.Run("nodemanager-service", func(t *testing.T) {
		spec := testSpecFor(t, "nodemanager-service")
		testRequireMetadata(t, spec)
		testRequireDb(t, spec, "sync", "nodemanager_service", "nodemanager")
	})
}

func TestDeploymentServiceAlways(t *testing.T) {
	specs := DefaultSpecs([]string{"authn-service", "authn-service", "authn-service", "authn-service", "authn-service", "authn-service"})
	found := false
	for _, s := range specs {
		if s.Name == "deployment-service" {
			found = true
			break
		}
	}
	assert.True(t, found)
}

func TestDeduplication(t *testing.T) {
	specs := DefaultSpecs([]string{"authn-service", "authn-service", "authn-service", "authn-service", "authn-service", "authn-service"})
	assert.Len(t, specs, 2)
	found := false
	for _, s := range specs {
		if s.Name == "authn-service" {
			found = true
			break
		}
	}
	assert.True(t, found)
}

func TestSetCommandExecutor(t *testing.T) {
	testCmdExec := command.NewMockExecutor(t)

	t.Run("sets async path defaults", func(t *testing.T) {
		s := Spec{Name: "test",
			AsyncPaths: []PathCopyOperation{
				{Name: "sync", SrcPath: "/tmp/test"},
			},
		}
		SetCommandExecutor(s, testCmdExec)

		require.Equal(t, testCmdExec, s.AsyncPaths[0].cmdExecutor, "command executor must match")
	})
	t.Run("sets sync path defaults", func(t *testing.T) {
		s := Spec{Name: "test",
			SyncPaths: []PathCopyOperation{
				{Name: "foo", SrcPath: "/tmp/test"},
			},
		}
		SetCommandExecutor(s, testCmdExec)

		require.Equal(t, testCmdExec, s.SyncPaths[0].cmdExecutor, "command executor must match")
	})
	t.Run("sets async cmd defaults", func(t *testing.T) {
		s := Spec{Name: "postgres", AsyncCmds: []CommandExecuteOperation{
			{Name: "async-pgdump", Cmd: Cmd{
				Name: "async", Dump: []string{"pgdump"},
			}},
		}}
		SetCommandExecutor(s, testCmdExec)

		require.Equal(t, testCmdExec, s.AsyncCmds[0].cmdExecutor, "command executor must match")
	})
	t.Run("sets sync cmd defaults", func(t *testing.T) {
		s := Spec{Name: "postgres", SyncCmds: []CommandExecuteOperation{
			{Name: "sync-pgdump", Cmd: Cmd{
				Name: "sync", Dump: []string{"pgdump"},
			}},
		}}
		SetCommandExecutor(s, testCmdExec)

		require.Equal(t, testCmdExec, s.SyncCmds[0].cmdExecutor, "command executor must match")
	})
}

func TestSetDefaults(t *testing.T) {
	t.Run("sets async path defaults", func(t *testing.T) {
		s := Spec{Name: "test",
			AsyncPaths: []PathCopyOperation{
				{Name: "sync", SrcPath: "/tmp/test"},
			},
		}
		setDefaults(s)

		require.Equal(t, []string{"test", "sync"}, s.AsyncPaths[0].ObjectName)
	})
	t.Run("sets sync path defaults", func(t *testing.T) {
		s := Spec{Name: "test",
			SyncPaths: []PathCopyOperation{
				{Name: "foo", SrcPath: "/tmp/test"},
			},
		}
		setDefaults(s)

		require.Equal(t, []string{"test", "foo"}, s.SyncPaths[0].ObjectName)
	})
	t.Run("sets async cmd defaults", func(t *testing.T) {
		s := Spec{Name: "postgres", AsyncCmds: []CommandExecuteOperation{
			{Name: "async-pgdump", Cmd: Cmd{
				Name: "async", Dump: []string{"pgdump"},
			}},
		}}
		setDefaults(s)

		require.Equal(t, []string{"postgres", "async-pgdump"}, s.AsyncCmds[0].ObjectName)
		require.Equal(t, "chef", s.AsyncCmds[0].PkgOrigin)
		require.Equal(t, "postgres", s.AsyncCmds[0].PkgName)
	})
	t.Run("sets sync cmd defaults", func(t *testing.T) {
		s := Spec{Name: "postgres", SyncCmds: []CommandExecuteOperation{
			{Name: "sync-pgdump", Cmd: Cmd{
				Name: "sync", Dump: []string{"pgdump"},
			}},
		}}
		setDefaults(s)

		require.Equal(t, []string{"postgres", "sync-pgdump"}, s.SyncCmds[0].ObjectName)
		require.Equal(t, "chef", s.SyncCmds[0].PkgOrigin)
		require.Equal(t, "postgres", s.SyncCmds[0].PkgName)
	})
}

func testRequirePath(t *testing.T, spec Spec, mode, path string, matchers ...RsyncMatcher) {
	validMode := false
	if mode == "sync" || mode == "async" {
		validMode = true
	}
	require.Truef(t, validMode,
		"%s in not a valid mode for path %s. must be one of 'sync' or 'async'",
		mode, path,
	)

	var p PathCopyOperation
	if mode == "sync" {
		for _, sp := range spec.SyncPaths {
			if sp.SrcPath == path {
				p = sp
			}
		}
	}
	if mode == "async" {
		for _, asp := range spec.AsyncPaths {
			if asp.SrcPath == path {
				p = asp
			}
		}
	}
	require.Equal(t, path, p.SrcPath, "could not find path matching %s", path)
	for _, dm := range matchers {
		match := false
		for _, pm := range p.RsyncMatchers {

			pmFlag, pmArgs := pm.RsyncArgs()
			dmFlag, dmArgs := dm.RsyncArgs()

			if pmFlag == dmFlag && pmArgs == dmArgs {
				match = true
			}
		}
		f, a := dm.RsyncArgs()
		require.Truef(t, match, "could not find required matcher (%s %s) for path %s", f, a, path)
	}
}

func findDbByName(name string, dbs []DatabaseDumpOperationV2) DatabaseDumpOperationV2 {
	for _, d := range dbs {
		if d.Name == name {
			return d
		}
	}

	return DatabaseDumpOperationV2{}
}

func testRequireDb(t *testing.T, spec Spec, mode, name string, user string) {
	validMode := mode == "sync" || mode == "async"
	require.Truef(t, validMode,
		"%s in not a valid mode for DB %s. must be one of 'sync' or 'async'",
		mode, name,
	)

	var p DatabaseDumpOperationV2
	if mode == "sync" {
		p = findDbByName(name, spec.SyncDbsV2)
	}
	if mode == "async" {
		p = findDbByName(name, spec.AsyncDbsV2)
	}

	assert.Equal(t, name, p.Name, "could not find database matching %s", name)
	assert.Equal(t, user, p.User, "username for database %s doesn't match %s", name, user)
}

func testRequireCmd(t *testing.T, spec Spec, mode, dumpCmd, restoreCmd string) {
	validMode := false
	if mode == "sync" || mode == "async" {
		validMode = true
	}
	require.Truef(t, validMode,
		"%s in not a valid mode for dumpcmd %s. must be one of 'sync' or 'async'",
		mode, dumpCmd,
	)

	dc := strings.Split(dumpCmd, " ")
	rc := strings.Split(restoreCmd, " ")

	var c CommandExecuteOperation
	if mode == "sync" {
		for _, sc := range spec.SyncCmds {
			if reflect.DeepEqual(sc.Cmd.Dump, dc) && reflect.DeepEqual(sc.Cmd.Restore, rc) {
				c = sc
			}
		}
	}
	if mode == "async" {
		for _, ac := range spec.AsyncCmds {
			if reflect.DeepEqual(ac.Cmd.Dump, dc) && reflect.DeepEqual(ac.Cmd.Restore, rc) {
				c = ac
			}
		}
	}
	require.Truef(t, reflect.DeepEqual(dc, c.Cmd.Dump),
		"could not find command with Dump matching %s and Restore matching %s",
		dumpCmd, restoreCmd,
	)
}

func testRequireMetadata(t *testing.T, spec Spec) {
	require.True(t, spec.WriteMetadata, "spec should have WriteMetadata enabled")
}

func findEsByName(name string, esops []ElasticsearchOperation) ElasticsearchOperation {
	for _, e := range esops {
		if e.ServiceName == name {
			return e
		}
	}

	return ElasticsearchOperation{}
}

func testRequireEs(t *testing.T, spec Spec, mode, serviceName string, multiIndexSpec string) {
	validMode := mode == "sync" || mode == "async"
	require.Truef(t, validMode,
		"%s in not a valid mode for DB %s. must be one of 'sync' or 'async'",
		mode, serviceName,
	)

	var op ElasticsearchOperation
	if mode == "sync" {
		op = findEsByName(serviceName, spec.SyncEsIndices)
	}
	if mode == "async" {
		op = findEsByName(serviceName, spec.AsyncEsIndices)
	}

	assert.Equal(t, serviceName, op.ServiceName, "could not find es operation for %s", serviceName)
	assert.Equal(t, multiIndexSpec, op.MultiIndexSpec, "incorrect index spec for %s", serviceName)
}

func testSpecFor(t *testing.T, desired string) Spec {
	for _, s := range DefaultSpecs([]string{desired}) {
		if s.Name == desired {
			return s
		}
	}

	require.Truef(t, false, "could not find spec for %s", desired)
	return Spec{}
}

func hasSpec(desired string) bool {
	for _, s := range DefaultSpecs([]string{desired}) {
		if s.Name == desired {
			return true
		}
	}

	return false
}
