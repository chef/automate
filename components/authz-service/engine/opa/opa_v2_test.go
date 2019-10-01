// These tests are for the v2 specific functionality.
package opa_test

import (
	"context"
	"fmt"
	"io"
	"os"
	"strings"
	"testing"
	"time"

	"github.com/mitchellh/mapstructure"
	"github.com/open-policy-agent/opa/ast"
	"github.com/open-policy-agent/opa/rego"
	"github.com/open-policy-agent/opa/storage/inmem"
	"github.com/open-policy-agent/opa/topdown"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

/************ ************ ************ ************ ************ ************
 * NOTE: These tests are low-level unit tests for the OPA engine,            *
 * so should be the first line of attack when crafting new rego code.        *
 *                                                                           *
 * These tests do NOT actually invoke the functions in opa.go! Rather,       *
 * they connect to OPA with direct calls (see `resultSetV2` in this file).   *
 *                                                                           *
 * Once these tests pass, move on to the conformance_v2_test.go              *
 * that actually do test the functions in opa.go.                            *
 ************ ************ ************ ************ ************ ************/

// Note: what this tests will likely be moved into tests that use the service's
// interface -- like what's happening in engine/conformance. For now, however,
// the gap between the GRPC API and the OPA engine hasn't been closed, so we'll
// have some tests here, closely aligned with the Rego code.

func TestAuthorizedWithStatements(t *testing.T) {

	// Turns out providing the data as JSON is simpler than creating
	// a nested map[string]interface{} construction.
	// PAY ATTENTION: it has to be proper JSON. Any missing " or , will not be
	// tolerated; also, there's no way to comment.
	//
	// This data is what our rego code expects to find in the store.
	//
	// NOTE the the statements have a similar "id => data" layout as used with
	// policies in v1 (and v2); that's done to avoid the state explosion issues
	// we had thought with v1 policies when there's an exponential runtime (in the
	// number of policies).
	// It might make sense to adapt our storage layer to expose IDs there -- if we
	// end up using them internally anyways, we might as well use those in OPA,
	// instead of generating new ones on-the-fly.
	//
	// It should be considered a playground more so than a definite schema.
	// Since this test affects all the different matching things, members,
	// resources, and actions, it's quite brittle. So, this is just a playground.
	data := `{
  "policies": {
    "9acbe920-d977-4c4d-a482-f125fe83a95a": {
      "name": "pol01",
      "description": "we will perhaps not feed this into the OPA store -- what is the use?",
      "members": [
        "team:local:admins",
        "user:local:*"
      ],
      "statements": {
        "900e12b7-8625-4881-985f-8c460b79f142": {
          "effect": "allow",
          "resources": [
            "infra:nodes",
            "compliance:profiles:*"
          ],
          "actions": [
            "infra:nodes:delete",
            "compliance:profiles:create"
          ]
        },
        "7bca8de2-665b-4843-aa2e-c48850bbfe83": {
          "effect": "allow",
          "resources": [ "iam:teams" ],
          "actions": [ "iam:teams:create" ]
        }
      }
    }
  }
}`

	cases := map[string]map[string]interface{}{
		"exact match": {
			"subjects": []string{"team:local:admins"},
			"action":   "iam:teams:create",
			"resource": "iam:teams",
		},
		"subject wildcard": {
			"subjects": []string{"user:local:alice"},
			"action":   "iam:teams:create",
			"resource": "iam:teams",
		},
		"one of multiple actions": {
			"subjects": []string{"team:local:admins"},
			"action":   "infra:nodes:delete",
			"resource": "infra:nodes",
		},
		"one of multiple resources (wildcard)": {
			"subjects": []string{"team:local:admins"},
			"action":   "compliance:profiles:create",
			"resource": "compliance:profiles:yadda",
		},
	}

	query := "data.authz_v2.authorized"

	for descr, input := range cases {
		t.Run(descr, func(t *testing.T) {
			rs := resultSetV2(t, input, strings.NewReader(data), query)

			require.Equal(t, 1, len(rs), "expected one result")
			require.Equal(t, 1, len(rs[0].Expressions), "expected one result expression")
			b, ok := rs[0].Expressions[0].Value.(bool)
			require.True(t, ok, "result value is a boolean")
			assert.Truef(t, b, "expected %q to be true", query)
		})
	}

	// Next, we re-run the same tests again, but with a second policy having
	// statements that deny what had just been authorized:
	data = `{
  "policies": {
    "9acbe920-d977-4c4d-a482-f125fe83a95a": {
      "name": "pol01",
      "description": "we will perhaps not feed this into the OPA store -- what is the use?",
      "members": [
        "team:local:admins",
        "user:local:*"
      ],
      "statements": {
        "900e12b7-8625-4881-985f-8c460b79f142": {
          "effect": "allow",
          "resources": [
            "infra:nodes",
            "compliance:profiles:*"
          ],
          "actions": [
            "infra:nodes:delete",
            "compliance:profiles:create"
          ]
        },
        "7bca8de2-665b-4843-aa2e-c48850bbfe83": {
          "effect": "allow",
          "resources": [ "iam:teams" ],
          "actions": [ "iam:teams:create" ]
        }
      }
    },
    "8ed84d95-400c-454d-8e70-50171c8a7543": {
      "name": "pol2",
      "members": [
        "team:local:admins",
        "user:local:*"
      ],
      "statements": {
        "900e12b7-8625-4881-985f-8c460b79f142": {
          "effect": "deny",
          "resources": [
            "infra:nodes",
            "compliance:profiles:*"
          ],
          "actions": [
            "infra:nodes:delete",
            "compliance:profiles:create"
          ]
        },
        "7bca8de2-665b-4843-aa2e-c48850bbfe83": {
          "effect": "deny",
          "resources": [ "iam:teams" ],
          "actions": [ "iam:teams:create" ]
        }
      }
    }
  }
}`

	for descr, input := range cases {
		t.Run(descr+" (DENY)", func(t *testing.T) {
			rs := resultSetV2(t, input, strings.NewReader(data), query)

			require.Equal(t, 1, len(rs), "expected one result")
			require.Equal(t, 1, len(rs[0].Expressions), "expected one result expression")
			b, ok := rs[0].Expressions[0].Value.(bool)
			require.True(t, ok, "result value is a boolean")
			assert.Falsef(t, b, "expected %q to be false", query)
		})
	}
}

func TestAuthorizedProjects(t *testing.T) {

	data := `{
  "policies": {
    "9acbe920-d977-4c4d-a482-f125fe83a95a": {
      "name": "pol01",
      "description": "we will perhaps not feed this into the OPA store -- what is the use?",
      "members": [
        "team:local:admins",
        "user:local:*"
      ],
      "statements": {
        "900e12b7-8625-4881-985f-8c460b79f142": {
          "effect": "allow",
          "projects": [ "p1", "p2" ],
          "resources": [
            "infra:nodes",
            "compliance:profiles:*"
          ],
          "actions": [
            "infra:nodes:delete",
            "compliance:profiles:create"
          ]
        },
        "7bca8de2-665b-4843-aa2e-c48850bbfe83": {
          "effect": "allow",
          "projects": [ "p3", "p4", "p5" ],
          "resources": [ "iam:teams" ],
          "actions": [ "iam:teams:create" ]
        }
      }
    },

    "9acbe920-d977-4c4d-a482-0125fe83a95a": {
      "name": "Administrator",
      "id": "administrator-access",
      "type": "CHEF_MANAGED",
      "members": [
        "team:local:admins",
        "user:local:localuser"
      ],
      "statements": {
        "9acbe920-d977-4c4d-a482-0125fe83a901": {
          "effect": "allow",
          "actions": [
            "*"
          ],
          "role": "",
          "resources": [
            "*"
          ],
          "projects": [
            "*"
          ]
        },
        "9acbe920-d977-4c4d-a482-0125fe83a902": {
          "effect": "DENY",
          "actions": [
            "iam:policies:delete",
            "iam:policies:update"
          ],
          "role": "",
          "resources": [
            "iam:policies:administrator-access"
          ],
          "projects": [
            "*"
          ]
        }
      },
      "projects": [
      ]
    },
    "9acbe920-d977-4c4d-a482-1125fe83a95a": {
      "name": "Editors",
      "id": "editor-access",
      "type": "CHEF_MANAGED",
      "members": [
        "team:local:editors"
      ],
      "statements": {
       "9acbe920-d977-4c4d-a482-0125fe83a903":  {
          "effect": "allow",
          "actions": [
          ],
          "role": "editor",
          "resources": [
            "*"
          ],
          "projects": [
            "*"
          ]
        }
      },
      "projects": [
      ]
    },
    "9acbe920-d977-4c4d-a482-2125fe83a95a": {
      "name": "Viewers",
      "id": "viewer-access",
      "type": "CHEF_MANAGED",
      "members": [
        "team:local:viewers"
      ],
      "statements": {
       "9acbe920-d977-4c4d-a482-0125fe83a904":  {
          "effect": "allow",
          "actions": [
          ],
          "role": "viewer",
          "resources": [
            "*"
          ],
          "projects": [
            "*"
          ]
        }
      },
      "projects": [
      ]
    },
    "9acbe920-d977-4c4d-a482-3125fe83a95a": {
      "name": "Ingest",
      "id": "ingest-access",
      "type": "CHEF_MANAGED",
      "members": [
        "user:local:admin",
        "team:local:admins"
      ],
      "statements": {
       "9acbe920-d977-4c4d-a482-0125fe83a905":  {
          "effect": "allow",
          "actions": [
          ],
          "role": "ingest",
          "resources": [
            "*"
          ],
          "projects": [
            "*"
          ]
        }
      },
      "projects": [
      ]
    },
    "9acbe920-d977-4c4d-a482-4125fe83a95a": {
      "name": "policy 3",
      "id": "policy-3 ",
      "type": "CUSTOM",
      "members": [
        "user:local:other"
      ],
      "statements": {
       "9acbe920-d977-4c4d-a482-0125fe83a906":  {
          "effect": "allow",
          "actions": [
          ],
          "role": "viewer",
          "resources": [
            "*"
          ],
          "projects": [
            "vcs1"
          ]
        }
      },
      "projects": [
      ]
    },
    "9acbe920-d977-4c4d-a482-5125fe83a95a": {
      "name": "policy 2",
      "id": "policy-2",
      "type": "CUSTOM",
      "members": [
        "user:local:other"
      ],
      "statements": {
       "9acbe920-d977-4c4d-a482-0125fe83a907":  {
          "effect": "allow",
          "actions": [
          ],
          "role": "viewer",
          "resources": [
            "*"
          ],
          "projects": [
            "other2"
          ]
        }
      },
      "projects": [
      ]
    },
    "9acbe920-d977-4c4d-a482-6125fe83a95a": {
      "name": "Other3",
      "id": "policy-other3",
      "type": "CUSTOM",
      "members": [
        "user:local:other"
      ],
      "statements": {
       "9acbe920-d977-4c4d-a482-0125fe83a908":  {
          "effect": "allow",
          "actions": [
          ],
          "role": "viewer",
          "resources": [
            "*"
          ],
          "projects": [
            "other"
          ]
        }
      },
      "projects": [
      ]
    },
    "9acbe920-d977-4c4d-a482-7125fe83a95a": {
      "name": "Other1",
      "id": "other1",
      "type": "CUSTOM",
      "members": [
        "team:ldap:Aenean a lorem sodales"
      ],
      "statements": {
       "9acbe920-d977-4c4d-a482-0125fe83a909":  {
          "effect": "allow",
          "actions": [
            "*"
          ],
          "role": "",
          "resources": [
            "*"
          ],
          "projects": [
            "my-target-project"
          ]
        }
      },
      "projects": [
      ]
    },
    "9acbe920-d977-4c4d-a482-8125fe83a95a": {
      "name": "Other2",
      "id": "other2",
      "type": "CUSTOM",
      "members": [
        "team:ldap:Aenean a lorem sodalesas"
      ],
      "statements": {
       "9acbe920-d977-4c4d-a482-0125fe83a910":  {
          "effect": "allow",
          "actions": [
            "*"
          ],
          "role": "",
          "resources": [
            "*"
          ],
          "projects": [
            "my-target-project"
          ]
        }
      },
      "projects": [
      ]
    }
  }
}`

	cases := map[string]map[string]interface{}{
		"exact match": {
			"subjects": []string{
				"team:ldap:Aenean a lorem soas",
				"team:ldap:Aenean ac libero il",
				"team:ldap:Aenean at tellus se",
				"team:ldap:Aenean dapibus riut",
				"team:ldap:Aenean scelerisqueu",
				"team:ldap:Aliquam a nibh eg a",
				"team:ldap:Aliquam auctor tefr",
				"team:ldap:Aliquam convallis q",
				"team:ldap:Aliquam eget libet ",
				"team:ldap:Aliquam id arcu vgu",
				"team:ldap:Aliquam id elit al ",
				"team:ldap:Aliquam rutrum tuco",
				"team:ldap:Aliquam vel mi eu d",
				"team:ldap:Aliquam vitae nunae",
				"team:ldap:Cras ac ex quis ape",
				"team:ldap:Cras commodo diamve",
				"team:ldap:Cras porta justo x ",
				"team:ldap:Cras quis eros vea ",
				"team:ldap:Curabitur convallre",
				"team:ldap:Curabitur eget nicu",
				"team:ldap:Curabitur euismods ",
				"team:ldap:Curabitur vitae nap",
				"team:ldap:Donec a turpis not ",
				"team:ldap:Donec congue lacu l",
				"team:ldap:Donec consequat qit",
				"team:ldap:Donec efficitur nri",
				"team:ldap:Donec efficitur rso",
				"team:ldap:Donec eget urna pto",
				"team:ldap:Donec fringilla aet",
				"team:ldap:Donec id lectus la ",
				"team:ldap:Donec interdum du r",
				"team:ldap:Donec laoreet ips e",
				"team:ldap:Donec molestie er s",
				"team:ldap:Donec ultricies nne",
				"team:ldap:Donec ut nunc contu",
				"team:ldap:Donec vel magna d, ",
				"team:ldap:Duis feugiat nibhel",
				"team:ldap:Duis id sapien vidi",
				"team:ldap:Duis tristique niet",
				"team:ldap:Duis vel tellus eeo",
				"team:ldap:Etiam molestie main",
				"team:ldap:Etiam suscipit feed",
				"team:ldap:Fusce ac nunc at tr",
				"team:ldap:Fusce et sapien sct",
				"team:ldap:Fusce fringilla mac",
				"team:ldap:Fusce in arcu at  d",
				"team:ldap:Fusce pulvinar ur a",
				"team:ldap:Fusce ultrices lat ",
				"team:ldap:Fusce ut eros pos a",
				"team:ldap:Fusce ut nisl sit t",
				"team:ldap:Fusce vel lectus ta",
				"team:ldap:Fusce vitae nullaua",
				"team:ldap:In at est nec nunci",
				"team:ldap:In dapibus risus me",
				"team:ldap:In non ipsum consur",
				"team:ldap:In placerat dui nss",
				"team:ldap:In ullamcorper arl ",
				"team:ldap:Integer a libero us",
				"team:ldap:Integer in ipsum at",
				"team:ldap:Integer in maurisrd",
				"team:ldap:Integer pharetra  n",
				"team:ldap:Integer sit amet  f",
				"team:ldap:Integer sodales ad ",
				"team:ldap:Maecenas ac sapieli",
				"team:ldap:Maecenas blandit ut",
				"team:ldap:Maecenas dictum qit",
				"team:ldap:Maecenas dignissic ",
				"team:ldap:Maecenas egestas  s",
				"team:ldap:Maecenas elementue ",
				"team:ldap:Maecenas mollis s e",
				"team:ldap:Mauris non justo ci",
				"team:ldap:Mauris tempor lac o",
				"team:ldap:Mauris ut purus a a",
				"team:ldap:Morbi eget lacus r,",
				"team:ldap:Morbi id sem lobo f",
				"team:ldap:Morbi imperdiet ms ",
				"team:ldap:Morbi ornare diamco",
				"team:ldap:Morbi rhoncus neq m",
				"team:ldap:Morbi vitae dui p s",
				"team:ldap:Nam a enim quis mre",
				"team:ldap:Nam ac ligula sede ",
				"team:ldap:Nam facilisis niss ",
				"team:ldap:Nam id enim ac pure",
				"team:ldap:Nam in est vitae in",
				"team:ldap:Nam lobortis justve",
				"team:ldap:Nam mattis diam nim",
				"team:ldap:Nam pulvinar ex ss,",
				"team:ldap:Nam vitae eros quit",
				"team:ldap:Nulla eu sapien fsi",
				"team:ldap:Nulla fringilla deg",
				"team:ldap:Nulla nec nisl ve g",
				"team:ldap:Nulla scelerisques ",
				"team:ldap:Nulla sed mauris du",
				"team:ldap:Nulla ut nulla sit ",
				"team:ldap:Nulla vitae nullaeo",
				"team:ldap:Nullam et leo necm ",
				"team:ldap:Nullam faucibus oed",
				"team:ldap:Nullam in urna vot ",
				"team:ldap:Nullam lobortis eui",
				"team:ldap:Nullam nec risus te",
				"user:ldap:Nullam pellentesqio",
				"user:ldap:Nunc malesuada ip u",
				"user:ldap:Nunc porttitor er b",
				"user:ldap:Pellentesque consur",
				"user:ldap:Pellentesque effi f",
				"user:ldap:Pellentesque in jmo",
				"user:ldap:Pellentesque male t",
				"user:ldap:Pellentesque vulp e",
				"user:ldap:Phasellus lobortiie",
				"user:ldap:Phasellus sed telal",
				"user:ldap:Phasellus sit ameem",
				"user:ldap:Praesent sed odiopi",
				"user:ldap:Proin eget justo gu",
				"user:ldap:Proin et mauris bum",
				"user:ldap:Proin rutrum ipsuin",
				"user:ldap:Proin sed leo in ul",
				"user:ldap:Proin varius arcu i",
				"user:ldap:Quisque eget est cu",
				"user:ldap:Quisque in urna mad",
				"user:ldap:Quisque nec mauriel",
				"user:ldap:Quisque sagittis s ",
				"user:ldap:Sed blandit ligul n",
				"user:ldap:Sed facilisis dolt ",
				"user:ldap:Sed feugiat justoe ",
				"user:ldap:Sed id mi nec masll",
				"user:ldap:Sed id nunc at ernd",
				"user:ldap:Sed imperdiet diahe",
				"user:ldap:Sed laoreet magname",
				"user:ldap:Sed mattis nulla  d",
				"user:ldap:Sed maximus massaam",
				"user:ldap:Sed mollis sem teod",
				"user:ldap:Sed pharetra nibhac",
				"user:ldap:Sed semper nisl ni ",
				"user:ldap:Sed vitae magna eap",
				"user:ldap:Sed vitae mi vel lu",
				"user:ldap:Suspendisse eget pe",
				"user:ldap:Suspendisse placerc",
				"user:ldap:Suspendisse vel uol",
				"user:ldap:Ut efficitur nuncve",
				"user:ldap:Ut sed arcu eget le",
				"user:ldap:Ut ullamcorper duni",
				"user:ldap:Ut vel nisl quis ef",
				"user:ldap:Ut vitae metus veu ",
				"user:ldap:Ut viverra risus us",
				"user:ldap:Vestibulum in ligac",
				"user:ldap:Vivamus congue teat",
				"user:ldap:Vivamus cursus nuet",
				"user:ldap:Vivamus eu ex mol, ",
				"user:ldap:Vivamus id magna ip",
				"team:ldap:Aenean a lorem sodalesas",
				"team:ldap:Aenean ac libero fringil",
				"team:ldap:Aenean at tellus quis se",
				"team:ldap:Aenean dapibus risus rut",
				"team:ldap:Aenean scelerisque ex eu",
				"team:ldap:Aliquam a nibh egestas a",
				"team:ldap:Aliquam auctor tellus fr",
				"team:ldap:Aliquam convallis nisi q",
				"team:ldap:Aliquam eget libero sit ",
				"team:ldap:Aliquam id arcu vel augu",
				"team:ldap:Aliquam id elit at nisl ",
				"team:ldap:Aliquam rutrum turpis co",
				"team:ldap:Aliquam vel mi eu eros d",
				"team:ldap:Aliquam vitae nunc vitae",
				"team:ldap:Cras ac ex quis augue pe",
				"team:ldap:Cras commodo diam nec ve",
				"team:ldap:Cras porta justo non ex ",
				"team:ldap:Cras quis eros vel urna ",
				"team:ldap:Curabitur convallis lore",
				"team:ldap:Curabitur eget nibh accu",
				"team:ldap:Curabitur euismod purus ",
				"team:ldap:Curabitur vitae nunc dap",
				"team:ldap:Donec a turpis non elit ",
				"team:ldap:Donec congue lacus vel l",
				"team:ldap:Donec consequat quam sit",
				"team:ldap:Donec efficitur nunc tri",
				"team:ldap:Donec efficitur risus so",
				"team:ldap:Donec eget urna porttito",
				"team:ldap:Donec fringilla augue et",
				"team:ldap:Donec id lectus lacinia ",
				"team:ldap:Donec interdum dui non r",
				"team:ldap:Donec laoreet ipsum et e",
				"team:ldap:Donec molestie eros et s",
				"team:ldap:Donec ultricies neque ne",
				"team:ldap:Donec ut nunc consectetu",
				"team:ldap:Donec vel magna dictum, ",
				"team:ldap:Duis feugiat nibh eu fel",
				"team:ldap:Duis id sapien vitae odi",
				"team:ldap:Duis tristique nisl eget",
				"team:ldap:Duis vel tellus eget leo",
				"team:ldap:Etiam molestie mauris in",
				"team:ldap:Etiam suscipit felis sed",
				"team:ldap:Fusce ac nunc at elit tr",
				"team:ldap:Fusce et sapien sed lect",
				"team:ldap:Fusce fringilla massa ac",
				"team:ldap:Fusce in arcu at justo d",
				"team:ldap:Fusce pulvinar urna ac a",
				"team:ldap:Fusce ultrices lacus ut ",
				"team:ldap:Fusce ut eros posuere, a",
				"team:ldap:Fusce ut nisl sit amet t",
				"team:ldap:Fusce vel lectus vulputa",
				"team:ldap:Fusce vitae nulla in qua",
				"team:ldap:In at est nec nunc tinci",
				"team:ldap:In dapibus risus sit ame",
				"team:ldap:In non ipsum consectetur",
				"team:ldap:In placerat dui non mass",
				"team:ldap:In ullamcorper arcu vel ",
				"team:ldap:Integer a libero dapibus",
				"team:ldap:Integer in ipsum in erat",
				"team:ldap:Integer in mauris imperd",
				"team:ldap:Integer pharetra est a n",
				"team:ldap:Integer sit amet augue f",
				"team:ldap:Integer sodales arcu id ",
				"team:ldap:Maecenas ac sapien et li",
				"team:ldap:Maecenas blandit nisi ut",
				"team:ldap:Maecenas dictum quam sit",
				"team:ldap:Maecenas dignissim nunc ",
				"team:ldap:Maecenas egestas purus s",
				"team:ldap:Maecenas elementum ante ",
				"team:ldap:Maecenas mollis sapien e",
				"team:ldap:Mauris non justo et orci",
				"team:ldap:Mauris tempor lacus ac o",
				"team:ldap:Mauris ut purus a nisl a",
				"team:ldap:Morbi eget lacus tempor,",
				"team:ldap:Morbi id sem lobortis, f",
				"team:ldap:Morbi imperdiet mi quis ",
				"team:ldap:Morbi ornare diam vel co",
				"team:ldap:Morbi rhoncus neque id m",
				"team:ldap:Morbi vitae dui porta, s",
				"team:ldap:Nam a enim quis mi phare",
				"team:ldap:Nam ac ligula sed neque ",
				"team:ldap:Nam facilisis nisl quis ",
				"team:ldap:Nam id enim ac purus pre",
				"team:ldap:Nam in est vitae odio in",
				"team:ldap:Nam lobortis justo ac ve",
				"team:ldap:Nam mattis diam nec enim",
				"team:ldap:Nam pulvinar ex sodales,",
				"team:ldap:Nam vitae eros quis elit",
				"team:ldap:Nulla eu sapien facilisi",
				"team:ldap:Nulla fringilla dolor eg",
				"team:ldap:Nulla nec nisl vel leo g",
				"team:ldap:Nulla scelerisque felis ",
				"team:ldap:Nulla sed mauris tincidu",
				"team:ldap:Nulla ut nulla sit amet ",
				"team:ldap:Nulla vitae nulla id leo",
				"team:ldap:Nullam et leo nec lorem ",
				"team:ldap:Nullam faucibus orci sed",
				"team:ldap:Nullam in urna volutpat ",
				"team:ldap:Nullam lobortis enim qui",
				"team:ldap:Nullam nec risus ac ante",
				"user:ldap:Nullam pellentesque odio",
				"user:ldap:Nunc malesuada ipsum a u",
				"user:ldap:Nunc porttitor erat in b",
				"user:ldap:Pellentesque consectetur",
				"user:ldap:Pellentesque efficitur f",
				"user:ldap:Pellentesque in justo mo",
				"user:ldap:Pellentesque malesuada t",
				"user:ldap:Pellentesque vulputate e",
				"user:ldap:Phasellus lobortis sapie",
				"user:ldap:Phasellus sed tellus mal",
				"user:ldap:Phasellus sit amet lorem",
				"user:ldap:Praesent sed odio a sapi",
				"user:ldap:Proin eget justo at augu",
				"user:ldap:Proin et mauris bibendum",
				"user:ldap:Proin rutrum ipsum lacin",
				"user:ldap:Proin sed leo in eros ul",
				"user:ldap:Proin varius arcu quis i",
				"user:ldap:Quisque eget est id lacu",
				"user:ldap:Quisque in urna malesuad",
				"user:ldap:Quisque nec mauris at el",
				"user:ldap:Quisque sagittis mauris ",
				"user:ldap:Sed blandit ligula sed n",
				"user:ldap:Sed facilisis dolor sit ",
				"user:ldap:Sed feugiat justo vitae ",
				"user:ldap:Sed id mi nec massa moll",
				"user:ldap:Sed id nunc at erat hend",
				"user:ldap:Sed imperdiet diam et he",
				"user:ldap:Sed laoreet magna vel me",
				"user:ldap:Sed mattis nulla vitae d",
				"user:ldap:Sed maximus massa sit am",
				"user:ldap:Sed mollis sem tempor od",
				"user:ldap:Sed pharetra nibh at lac",
				"user:ldap:Sed semper nisl nec dui ",
				"user:ldap:Sed vitae magna eget sap",
				"user:ldap:Sed vitae mi vel mi volu",
				"user:ldap:Suspendisse eget nibh pe",
				"user:ldap:Suspendisse placerat arc",
				"user:ldap:Suspendisse vel urna mol",
				"user:ldap:Ut efficitur nunc non ve",
				"user:ldap:Ut sed arcu eget mi mole",
				"user:ldap:Ut ullamcorper dui ut ni",
				"user:ldap:Ut vel nisl quis enim ef",
				"user:ldap:Ut vitae metus vel arcu ",
				"user:ldap:Ut viverra risus dapibus",
				"user:ldap:Vestibulum in ligula fac",
				"user:ldap:Vivamus congue tellus at",
				"user:ldap:Vivamus cursus nunc eget",
				"user:ldap:Vivamus eu ex molestie, ",
				"user:ldap:Vivamus id magna eget ip",
			},
			"projects": []string{"my-target-project"},
			"action":   "iam:teams:create",
			"resource": "iam:teams",
		},
	}

	query := "data.authz_v2.authorized_project"

	for descr, input := range cases {
		t.Run(descr, func(t *testing.T) {
			rs := resultSetV2(t, input, strings.NewReader(data), query)

			require.Equal(t, 1, len(rs), "expected one result")
			require.Equal(t, 1, len(rs[0].Expressions), "expected one result expression")
			projects, ok := rs[0].Expressions[0].Value.([]interface{})
			require.True(t, ok, "result value is an array")
			expectedProjects := []string{"my-target-project"}
			assert.ElementsMatch(t, expectedProjects, projects, "expected %q to return %v", query, expectedProjects)
		})
	}
}

func TestIntrospectionV2(t *testing.T) {
	data := `{
  "policies": {
    "9acbe920-d977-4c4d-a482-f125fe83a95a": {
      "name": "pol01",
      "description": "we will perhaps not feed this into the OPA store -- what is the use?",
      "members": [
        "team:local:admins",
        "user:local:*"
      ],
      "statements": {
        "900e12b7-8625-4881-985f-8c460b79f142": {
          "effect": "allow",
          "resources": [
            "cfgmgmt:nodes",
            "compliance:profiles:*"
          ],
          "actions": [
            "cfgmgmt:delete",
            "compliance:upload"
          ]
        },
        "7bca8de2-665b-4843-aa2e-c48850bbfe83": {
          "effect": "allow",
          "resources": [ "auth:teams" ],
          "actions": [ "admin:create" ]
        }
      }
    }
  }
}`

	cases := map[string]map[string]interface{}{
		"exact match": {
			"subjects": []string{"team:local:admins"},
			"pairs":    []map[string]string{{"action": "admin:create", "resource": "auth:teams"}},
		},
		"subject wildcard": {
			"subjects": []string{"user:local:alice"},
			"pairs":    []map[string]string{{"action": "admin:create", "resource": "auth:teams"}},
		},
		"one of multiple actions": {
			"subjects": []string{"team:local:admins"},
			"pairs":    []map[string]string{{"action": "cfgmgmt:delete", "resource": "cfgmgmt:nodes"}},
		},
		"one of multiple resources (wildcard)": {
			"subjects": []string{"team:local:admins"},
			"pairs":    []map[string]string{{"action": "compliance:upload", "resource": "compliance:profiles:yadda"}},
		},
		"multiple matches (all of the above)": {
			"subjects": []string{"team:local:admins"},
			"pairs": []map[string]string{
				{"action": "cfgmgmt:delete", "resource": "cfgmgmt:nodes"},
				{"action": "admin:create", "resource": "auth:teams"},
				{"action": "compliance:upload", "resource": "compliance:profiles:yadda"},
			},
		},
	}

	query := "data.authz_v2.introspection.authorized_pair[pair]"

	for descr, input := range cases {
		t.Run(descr, func(t *testing.T) {
			rs := resultSetV2(t, input, strings.NewReader(data), query)

			require.NotZero(t, len(rs), "expected at least one result")
			for _, result := range rs {
				bs := map[string]map[string]string{}
				err := mapstructure.Decode(result.Bindings, &bs)
				require.NoError(t, err, "decode result bindings")

				// The output always is a subset of the inputs. In our case, the input
				// only contains pairs we want to get out again, so, we match directly:
				assert.Contains(t, input["pairs"].([]map[string]string), bs["pair"])
			}
		})
	}
}

func TestActionsMatching(t *testing.T) {
	// This test, which checks a function's outputs given certain inputs,
	// does not depend on the OPA input or OPA data -- since the function
	// doesn't.
	input := map[string]interface{}{}
	data := "{}"

	// we're testing action_matches(in, stored)
	in := "iam:users:update" // "in" argument

	// expectedSuccess => "stored" argument
	classes := map[bool][]string{
		true: {
			"iam:users:update",
			"iam:users:*",
			"iam:*",
			"*",
			"iam:*:update",
			"*:update",
		},
		false: {
			"infra:nodes:delete",
			"infra:nodes:*",
			"infra:*",
			"infra:*:delete",
			"*:delete",
			"*:users:update", // it's only "*:VERB" that's allowed
			"*:iam:update",
			"*:*:update",
			"*:*:*",
			"iam:users:up*", // prefix
			"iam:us*:update",
			"ia*:users:update",
			"*am:users:update", // suffix
			"iam:*ers:update",
			"iam:users:*ate",
			"iam:users:update ", // space matters
			" iam:users:update",
		},
	}

	for expectedSuccess, actions := range classes {
		t.Run(fmt.Sprintf("%v", expectedSuccess), func(t *testing.T) {
			for _, stored := range actions {
				t.Run(stored, func(t *testing.T) {

					// We ask directly for what's used in the definition of
					// data.authz_v2.has_action: the action_matches(in, stored) function
					query := fmt.Sprintf("data.authz_v2.action_matches(%q, %q)", in, stored)
					rs := resultSetV2(t, input, strings.NewReader(data), query)
					if expectedSuccess {
						require.Equal(t, 1, len(rs))
						require.Equal(t, 1, len(rs[0].Expressions), "expected one result expression")
						result, ok := rs[0].Expressions[0].Value.(bool)
						require.True(t, ok, "expected result expression value to be boolean")
						assert.True(t, result)
					} else {
						require.Equal(t, 0, len(rs), "no result expected")
					}
				})
			}
		})
	}
}

// TestHasAction is focussing on finding the matching action in a policy (with
// one or more statements)

// Note: the policy evaluation logic this tests is the same used by
// data.authz_v2.has_resource[[pol_id, statement_id]]. However, having the exact
// same set of tests for both of those paths, with only the word "action[s]"
// replaced by "resource[s]" doesn't feel right. Let's take this as another
// potentially useful way to test our policies, and decide how we want to
// achieve our desired assurances best.
func TestHasAction(t *testing.T) {
	// These tests set up different data layouts (policies), but always use the
	// same id for the matching policy/statement:
	polID, statementID := "86d88515-5f41-400d-8c2b-237bad00ff81", "7ae5ea03-eb67-4935-8387-1eafb4dffd78"

	query := "data.authz_v2.has_action[[pol_id, statement_id]]"
	expectedBinding := map[string]string{
		"pol_id":       polID,
		"statement_id": statementID,
	}
	action, otherAction := "iam:users:update", "iam:users:delete"
	otherPolID, otherStatementID := "c525817a-6ded-426e-92e3-e9e5184da9a9", "b263a2cf-8b25-4f46-a00f-5c771594742c"
	input := map[string]interface{}{"action": action}

	// description => data JSON string containing policy setup
	cases := map[string]string{
		"one policy, only statement, only one action": fmt.Sprintf(
			`{"policies": {%q: {"statements": {%q: {"actions": [%q]}}}}}`, polID, statementID, action),
		"one policy, only statement, first action": fmt.Sprintf(
			`{"policies": {%q: {"statements": {%q: {"actions": [%q, %q]}}}}}`, polID, statementID, action, otherAction),
		"one policy, only statement, second action": fmt.Sprintf(
			`{"policies": {%q: {"statements": {%q: {"actions": [%q, %q]}}}}}`, polID, statementID, otherAction, action),
		"one policy, two statements, only one action": fmt.Sprintf(
			`{"policies": {%q: {"statements": {
        %q: {"actions": [%q]},
        %q: {"actions": [%q]}}}}}`, polID, otherStatementID, otherAction, statementID, action),
		"one policy, two statements, first action": fmt.Sprintf(
			`{"policies": {%q: {"statements": {
        %q: {"actions": [%q]},
        %q: {"actions": [%q, %q]}}}}}`, polID, otherStatementID, otherAction, statementID, action, otherAction),
		"one policy, two statements, second action": fmt.Sprintf(
			`{"policies": {%q: {"statements": {
        %q: {"actions": [%q]},
        %q: {"actions": [%q, %q]}}}}}`, polID, otherStatementID, otherAction, statementID, otherAction, action),
		"two policies, each with only one statement, only one action": fmt.Sprintf(
			`{"policies": {
        %q: {"statements": {%q: {"actions": [%q]}}},
        %q: {"statements": {%q: {"actions": [%q]}}}}}`, otherPolID, otherStatementID, otherAction,
			polID, statementID, action),
		"two policies, mixed statements, multiple action": fmt.Sprintf(
			`{"policies": {
        %q: {"statements": {%q: {"actions": [%q, %q]}}},
        %q: {"statements": {%q: {"actions": [%q, %q]}}}}}`, otherPolID, otherStatementID, otherAction, otherAction,
			polID, statementID, otherAction, action),
	}

	for desc, data := range cases {
		t.Run(desc, func(t *testing.T) {
			rs := resultSetV2(t, input, strings.NewReader(data), query)
			require.Equal(t, 1, len(rs))
			actualBinding := map[string]string{}
			err := mapstructure.Decode(rs[0].Bindings, &actualBinding)
			require.NoError(t, err, "error decoding bindings")
			assert.Equal(t, expectedBinding, actualBinding)
		})
	}
}

func TestHasProject(t *testing.T) {
	// These are smoke tests -- just to see that the rego code is accessible from Go code.
	// The `has_project` rego function has a rich set of rego unit tests for validation.
	polID, statementID := "86d88515-5f41-400d-8c2b-237bad00ff81", "7ae5ea03-eb67-4935-8387-1eafb4dffd78"
	project, otherProject := "project-9", "(unassigned)"
	input := map[string]interface{}{"projects": []string{project}}

	query := "data.authz_v2.has_project[[project, pol_id, statement_id]]"
	expectedBinding := map[string]string{
		"project":      project,
		"pol_id":       polID,
		"statement_id": statementID,
	}

	// description => data JSON string containing policy setup
	cases := map[string]string{
		"one policy, only statement, one project": fmt.Sprintf(
			`{"policies": {%q: {"statements": {%q: {"projects": [%q]}}}}}`,
			polID, statementID, project),
		"one policy, only statement, two projects": fmt.Sprintf(
			`{"policies": {%q: {"statements": {%q: {"projects": [%q, %q]}}}}}`,
			polID, statementID, otherProject, project),
	}

	for desc, data := range cases {
		t.Run(desc, func(t *testing.T) {
			rs := resultSetV2(t, input, strings.NewReader(data), query)
			require.Equal(t, 1, len(rs))
			actualBinding := map[string]string{}
			err := mapstructure.Decode(rs[0].Bindings, &actualBinding)
			require.NoError(t, err, "error decoding bindings")
			assert.Equal(t, expectedBinding, actualBinding)
		})
	}
}

// Helper functions

func resultSetV2(t *testing.T,
	input map[string]interface{},
	data io.Reader,
	query string,
) rego.ResultSet {
	t.Helper()
	var tracer *topdown.BufferTracer
	// ⓘ DEBUG note: to see what's happening during policy execution in OPA,
	// uncomment the following line then execute some tests in this file.
	// tracer = topdown.NewBufferTracer()

	r := rego.New(
		rego.Query(query),
		rego.Compiler(compilerV2(t)),
		rego.Store(inmem.NewFromReader(data)),
		rego.Tracer(tracer),
		rego.Input(input),
	)

	ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
	defer cancel()

	rs, err := r.Eval(ctx)
	if err != nil {
		t.Fatalf("OPA eval failed: %s", err)
	}

	if tracer.Enabled() {
		topdown.PrettyTrace(os.Stderr, *tracer)
	}
	return rs
}

func compilerV2(t *testing.T) *ast.Compiler {
	t.Helper()
	return compilerWithModules(t, map[string]string{
		"authz_v2.rego":         "../opa/policy/authz_v2.rego",
		"introspection_v2.rego": "../opa/policy/introspection_v2.rego",
		"common.rego":           "../opa/policy/common.rego",
	})
}
