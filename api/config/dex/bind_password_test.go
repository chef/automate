package dex_test

import (
	"bytes"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"testing"

	"github.com/ghodss/yaml"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/toml"
)

// What's happening in between the config.toml passed to `chef-automate config`
// and dex processing the values is quite involved. This test is meant to give
// us some assurance about the interplay of the various actors.
//
// We're *approximating* the data flow that happens. Differences are
// - not using habitat's toJson handlebar helper, but relying on golang's
//   encoding/json
func TestRoundtrip(t *testing.T) {
	cases := []string{`foo'bar`, `foo$bar`,
		`foo"bar`, `foobar # `, `@#%@&$^*$*)`,
		`%$(rm-rf /)'?!!   `}
	for _, tc := range cases {
		t.Run(tc, func(t *testing.T) {

			// This is a stand-in for what we have in config/config.yml
			dexConfig := `config:
  bind_password: $LDAP_BINDPW
  bind_dn: $LDAP_BINDDN
  canary: peep peep
  `
			// This is a stand-in for the user-provided config.toml (that is fed into
			// chef-automate config {set,patch})
			tomlContent := fmt.Sprintf(`[ldap]
bind_password = '''%s'''
bind_dn = "cn=admin,dc=example,dc=org"
`, tc)
			m := map[string]interface{}{}
			_, err := toml.Decode(tomlContent, &m)
			require.NoError(t, err)

			// dex creates an intermediate json doc, and os.ExpandEnv's the connector
			// config section(s) AS JSON, before calling json.Unmarshal
			jsIntermed, err := yaml.YAMLToJSON([]byte(dexConfig))
			require.NoError(t, err)
			t.Log(string(jsIntermed))

			// This approximates the {{ toJson cfg.connectors.ldap.bind_password }}
			// template from which habitat creates config/ldap_{bind_dn,bind_password}
			bindDNFileContent := bytes.Buffer{}
			err = json.NewEncoder(&bindDNFileContent).Encode(m["ldap"].(map[string]interface{})["bind_dn"])
			t.Log(bindDNFileContent.String())
			require.NoError(t, err)
			bindPasswordFileContent := bytes.Buffer{}
			err = json.NewEncoder(&bindPasswordFileContent).Encode(m["ldap"].(map[string]interface{})["bind_password"])
			t.Log(bindPasswordFileContent.String())
			require.NoError(t, err)

			// This approximates what dex does when reading in its config
			mapping := func(s string) string {
				switch s {
				case "LDAP_BINDPW":
					return sed(t, bindPasswordFileContent)
				case "LDAP_BINDDN":
					return sed(t, bindDNFileContent)
				}
				return ""
			}
			expandedDexCfg := os.Expand(string(jsIntermed), mapping)
			t.Log(expandedDexCfg)

			c := struct {
				Config struct {
					BindPW string `json:"bind_password"`
					BindDN string `json:"bind_dn"`
					Canary string `json:"canary"`
				} `json:"config"`
			}{}
			err = json.Unmarshal([]byte(expandedDexCfg), &c)
			require.NoError(t, err)

			t.Log(c)
			assert.Equal(t, tc, c.Config.BindPW)
			assert.Equal(t, "cn=admin,dc=example,dc=org", c.Config.BindDN)
			assert.Equal(t, "peep peep", c.Config.Canary)
		})
	}
}

func sed(t *testing.T, content bytes.Buffer) string {
	cmd := exec.Command("sed", `s/^\"\(.*\)\"$/\1/`)
	cmd.Stdin = bytes.NewReader(content.Bytes())
	out := bytes.Buffer{}
	cmd.Stdout = &out
	stderr := bytes.Buffer{}
	cmd.Stderr = &stderr

	if err := cmd.Run(); err != nil {
		t.Errorf("sed error: %s", err)
		t.Fatalf("stderr: %s", stderr.String())
	}

	r := strings.TrimRight(out.String(), "\n")
	t.Log(r)
	return r
}
