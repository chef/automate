package v2_test

import (
	"fmt"
	"strings"
	"testing"

	"github.com/grpc-ecosystem/grpc-gateway/runtime"
	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	v2 "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/lib/grpc/grpctest"
	uuid "github.com/chef/automate/lib/uuid4"
)

func TestGeneratedProtobufUpToDate(t *testing.T) {
	grpctest.AssertCompiledInUpToDate(t, "api/interservice/authz/v2/")
}

func TestWhatItLooksLikeInJSON(t *testing.T) {
	pol := v2.Policy{
		Id:      uuid.Must(uuid.NewV4()).String(),
		Members: []string{"user:local:alice", "team:local:ops"},
	}

	// role and projects
	statement0 := v2.Statement{
		Effect:   v2.Statement_DENY,
		Projects: []string{"bu1"},
		Role:     "admin",
	}
	statement1 := v2.Statement{
		Effect:    v2.Statement_ALLOW,
		Resources: []string{"cfgmgmt:delete", "cfgmgmt:list"},
		Actions:   []string{"cfgmgmt:nodes:*"},
	}
	pol.Statements = append(pol.Statements, &statement0, &statement1)

	// this is the marshaler settings used by grpc-gateway
	m := runtime.JSONPb{OrigName: true, EmitDefaults: true}

	j, err := m.Marshal(&pol)
	require.NoError(t, err)
	t.Log(string(j))
}

func TestValidationCreatePolicy(t *testing.T) {
	validStatement := v2.Statement{
		Effect:    v2.Statement_ALLOW,
		Resources: []string{"cfgmgmt:delete", "cfgmgmt:list"},
		Actions:   []string{"cfgmgmt:nodes:*"},
	}

	negativeCases := map[string]*v2.CreatePolicyReq{
		// ID
		"with uppercase ID": &v2.CreatePolicyReq{
			Id:         "TestID",
			Statements: []*v2.Statement{&validStatement},
		},
		"with ID with spaces": &v2.CreatePolicyReq{
			Id:         "test id",
			Statements: []*v2.Statement{&validStatement},
		},
		// Members
		"zero-length members in create req": &v2.CreatePolicyReq{
			Id:         "test-id",
			Members:    []string{""},
			Statements: []*v2.Statement{&validStatement},
		},
		"empty members in create req": &v2.CreatePolicyReq{
			Id:         "test-id",
			Members:    []string{"user:local:not-empty", "", "user:local:another-one"},
			Statements: []*v2.Statement{&validStatement},
		},
		"repeated members in create req": &v2.CreatePolicyReq{
			Id:         "test-id",
			Members:    []string{"user:local:repeat", "user:local:not-repeat", "user:local:repeat"},
			Statements: []*v2.Statement{&validStatement},
		},
		"malformed user": &v2.CreatePolicyReq{
			Id:         "test-id",
			Members:    []string{"user:"},
			Statements: []*v2.Statement{&validStatement},
		},
		"malformed team": &v2.CreatePolicyReq{
			Id:         "test-id",
			Members:    []string{"team:"},
			Statements: []*v2.Statement{&validStatement},
		},
		"malformed token": &v2.CreatePolicyReq{
			Id:         "test-id",
			Members:    []string{"token:"},
			Statements: []*v2.Statement{&validStatement},
		},
		"malformed type": &v2.CreatePolicyReq{
			Id:         "test-id",
			Members:    []string{"user:notldap:something"},
			Statements: []*v2.Statement{&validStatement},
		},
		"malformed token type": &v2.CreatePolicyReq{
			Id:         "test-id",
			Members:    []string{"token:moretype:lmpT7GHDJ1Ec3H08rVXKyfBKc78="},
			Statements: []*v2.Statement{&validStatement},
		},
		"wildcard with specific member": &v2.CreatePolicyReq{
			Id:         "test-id",
			Members:    []string{"user:*:username"},
			Statements: []*v2.Statement{&validStatement},
		},
		"leading wildcard": &v2.CreatePolicyReq{
			Id:         "test-id",
			Members:    []string{"*:local:*"},
			Statements: []*v2.Statement{&validStatement},
		},
		"empty member": &v2.CreatePolicyReq{
			Id:         "test-id",
			Members:    []string{"user:local:test", "", "team:local:test"},
			Statements: []*v2.Statement{&validStatement},
		},
		"unknown first term any:*": &v2.CreatePolicyReq{
			Id:         "test-id",
			Members:    []string{"any:*"},
			Statements: []*v2.Statement{&validStatement},
		},
		"missing third term user:local:": &v2.CreatePolicyReq{
			Id:         "test-id",
			Members:    []string{"user:local:"},
			Statements: []*v2.Statement{&validStatement},
		},
		"wrong second term tls:other:a:b": &v2.CreatePolicyReq{
			Id:         "test-id",
			Members:    []string{"tls:other:a:b"},
			Statements: []*v2.Statement{&validStatement},
		},
		// projects
		"whitespace projects list": &v2.CreatePolicyReq{
			Id:         "this-is-valid-1",
			Projects:   []string{"     ", "test"},
			Statements: []*v2.Statement{&validStatement},
		},
		"repeated projects in list": &v2.CreatePolicyReq{
			Id:         "this-is-valid-1",
			Projects:   []string{"repeat", "repeat"},
			Statements: []*v2.Statement{&validStatement},
		},
		"Project has invalid characters": &v2.CreatePolicyReq{
			Id:         "this-is-valid-1",
			Projects:   []string{"valid", "wrong~"},
			Statements: []*v2.Statement{&validStatement},
		},
		"Project has spaces": &v2.CreatePolicyReq{
			Id:         "this-is-valid-1",
			Projects:   []string{"valid", "wrong space"},
			Statements: []*v2.Statement{&validStatement},
		},
		"project has uppercase characters": &v2.CreatePolicyReq{
			Id:         "this-is-valid-1",
			Projects:   []string{"valid", "PROJECT1"},
			Statements: []*v2.Statement{&validStatement},
		},
	}
	positiveCases := map[string]*v2.CreatePolicyReq{
		// ID
		"with ID all lowercase": &v2.CreatePolicyReq{
			Id:         "test",
			Statements: []*v2.Statement{&validStatement},
		},
		"with ID that has dashes": &v2.CreatePolicyReq{
			Id:         "-test-with-dashes-",
			Statements: []*v2.Statement{&validStatement},
		},
		"with ID that has dashes and numbers": &v2.CreatePolicyReq{
			Id:         "1-test-with-1-and-dashes-0",
			Statements: []*v2.Statement{&validStatement},
		},
		"with ID that has only numbers": &v2.CreatePolicyReq{
			Id:         "1235",
			Statements: []*v2.Statement{&validStatement},
		},
		// Members
		"without members": &v2.CreatePolicyReq{
			Id:         "test-id",
			Statements: []*v2.Statement{&validStatement},
		},
		"a single member": &v2.CreatePolicyReq{
			Id:         "test-id",
			Members:    []string{"user:local:member1"},
			Statements: []*v2.Statement{&validStatement},
		},
		"multiple members": &v2.CreatePolicyReq{
			Id:         "test-id",
			Statements: []*v2.Statement{&validStatement},
			Members: []string{"user:local:member1", "user:saml:member2", "user:ldap:member3",
				"user:local:*", "user:ldap:*", "user:saml:*", "team:local:member1", "team:saml:member2",
				"team:ldap:member3", "team:local:*", "team:ldap:*", "team:saml:*", "token:lmpT7GHDJ1Ec3H08rVXKyfBKc78=",
				"user:*", "team:*", "token:*", "tls:*", "tls:service:*", "tls:service:any", "tls:service:any:*",
				"tls:service:any:other", "*"},
		},
		// Statements
		"a single statement deny": &v2.CreatePolicyReq{
			Id:      "test-id",
			Members: []string{"user:local:member1", "user:local:member2", "user:local:member3"},
			Statements: []*v2.Statement{
				&v2.Statement{
					Effect:    v2.Statement_DENY,
					Resources: []string{"some-resource", "some-other-resource"},
					Actions:   []string{"infra:some:action", "infra:some:other"},
				},
			},
		},
		"a single statement allow": &v2.CreatePolicyReq{
			Id:      "test-id",
			Members: []string{"user:local:member1", "user:local:member2", "user:local:member3"},
			Statements: []*v2.Statement{
				&v2.Statement{
					Effect:    v2.Statement_ALLOW,
					Resources: []string{"some-resource", "some-other-resource"},
					Actions:   []string{"infra:some:action", "infra:some:other"},
				},
			},
		},
		"multi-statement": &v2.CreatePolicyReq{
			Id:      "test-id",
			Members: []string{"user:local:member1", "user:local:member2", "user:local:member3"},
			Statements: []*v2.Statement{
				&v2.Statement{
					Effect:    v2.Statement_ALLOW,
					Resources: []string{"some-resource", "some-other-resource"},
					Actions:   []string{"infra:some:action", "infra:some:other"},
				},
				&v2.Statement{
					Effect:    v2.Statement_DENY,
					Resources: []string{"some-resource2", "some-other-resource2"},
					Actions:   []string{},
				},
				&v2.Statement{
					Effect:    v2.Statement_ALLOW,
					Resources: []string{},
					Actions:   []string{},
				},
			},
		},
		// Projects
		"without projects": &v2.CreatePolicyReq{
			Id:         "test-id",
			Statements: []*v2.Statement{&validStatement},
		},
		"empty projects": &v2.CreatePolicyReq{
			Id:         "test-id",
			Projects:   []string{},
			Statements: []*v2.Statement{&validStatement},
		},
		"a single project": &v2.CreatePolicyReq{
			Id:         "test-id",
			Projects:   []string{"project-1"},
			Statements: []*v2.Statement{&validStatement},
		},
		"multiple projects": &v2.CreatePolicyReq{
			Id:         "test-id",
			Projects:   []string{"project-1", "project-2", "project-3"},
			Statements: []*v2.Statement{&validStatement},
		},
		// Resources
		"no resources (validated in server.go)": &v2.CreatePolicyReq{
			Id:      "test-id",
			Members: []string{"user:local:member1", "user:local:member2", "user:local:member3"},
			Statements: []*v2.Statement{
				&v2.Statement{
					Effect:    v2.Statement_DENY,
					Resources: []string{},
					Actions:   []string{"infra:some:action", "infra:some:other"},
				},
			},
		},
		// Actions
		"no actions (validated in server.go)": &v2.CreatePolicyReq{
			Id:      "test-id",
			Members: []string{"user:local:member1", "user:local:member2", "user:local:member3"},
			Statements: []*v2.Statement{
				&v2.Statement{
					Effect:    v2.Statement_DENY,
					Resources: []string{"some-resource", "some-other-resource"},
					Actions:   []string{},
				},
			},
		},
	}

	classes := map[bool]map[string]*v2.CreatePolicyReq{
		true:  positiveCases,
		false: negativeCases,
	}

	for expectedSuccess, cases := range classes {
		for name, tc := range cases {
			t.Run(name, func(t *testing.T) {
				if tc.Name == "" {
					tc.Name = "test"
				}
				err := tc.Validate()
				if expectedSuccess {
					assert.NoError(t, err)
				} else {
					assert.Error(t, err)
				}
			})
		}
	}
}

func TestValidationCreatePolicy_Actions(t *testing.T) {
	t.Run("duplicate actions", func(t *testing.T) {
		duplicateClasses := map[bool][][]string{
			true: {
				{"*", "iam:users:create"}, // semantic overlap is not validated
			},
			false: {
				{"this:isA:duplicate", "this:isA:duplicate"},
				{"*", "*"},
			},
		}
		for expectedSuccess, cases := range duplicateClasses {
			t.Run(fmt.Sprintf("%v", expectedSuccess), func(t *testing.T) {
				for _, actions := range cases {
					t.Run(strings.Join(actions, ";"), func(t *testing.T) {
						req := validCreatePolicyReq()
						req.Statements[0].Actions = actions
						err := req.Validate()
						if expectedSuccess {
							assert.NoError(t, err)
						} else {
							assert.Error(t, err)
						}
					})
				}
			})
		}
	})

	// simple checks
	classes := map[bool][]string{
		true: {
			"a:b:c",
			"*",
			"ingest:camelCaseThings:create",
			"no:camels:here",
			"*:mark-for-deletion", // legacy
			"infra:*:create",
			"ingest:camelCaseThings:*",
			// v1 actions that were previously invalid
			"*:foo",
			"*",
			"a:*",
			"a:b:*",
		},
		false: {
			"",
			"nodes::foo",
			"nodes:foo:",
			"nodes:",
			"nodes::",
			":foo",
			"::foo",
			"Abc:z",
			"0a:z",
			"-a:z",
			"_a:z",
			"nodes:fo*",
			"nodes:f*o",
			"nodes:*oo",
			"**",
			"*:*",
			"%nodes:foo",
			"a::c",
			"a:b::",
			"*:*:*",
			"infra:*:*",
			"infra:**",
			"*:*:create",
			"**:create",
			"*:camelCaseThings:*",
			"*:camelCaseThings:create",
			":camelCaseThings:create",
			"infra::create",
			"ingest:camelCaseThings:",
			"Ingest:camelCaseThings:create",
			"ingest:CamelCaseThings:create",
			"ingest:camelCaseThings:Create",
			"Ingest:camelCaseThings:*",
			"ingest:CamelCaseThings:*",
			"infra:*:Create",
			"*:Create",
			// no longer valid v1 actions
			"a1a:z",
			"nodes#:foo",
			"a:test@foo.com:bar",
			"a_a:z",
			"a-a:z",
			"a:_",
			"a:-",
			"a:run_list",
			"a:run-list",
			"a:test@foo.com",
			"nodes:#oo",
			"nodes:f#o",
			"nodes:fo#",
			"cfgmgmt:nodes:f9047a75-44c5-4626-8b1a-b08a67d4270d:runs:d9047a75-44c5-4626-8b1a-b08a67d4270f",
			"a:0",
			"aAa:z",
			"nodes",
			"n",
			"a:Z",
			"a:b",
			"compliance:profiles:storage:OWNER:NAME:VERSION",
			"a:b:c:*",
			"a:b:c:d:*",
		},
	}

	for expectedSuccess, cases := range classes {
		t.Run(fmt.Sprintf("%v", expectedSuccess), func(t *testing.T) {
			for _, action := range cases {
				t.Run(action, func(t *testing.T) {
					req := validCreatePolicyReq()
					req.Statements[0].Actions = []string{action}
					err := req.Validate()
					if expectedSuccess {
						assert.NoError(t, err)
					} else {
						assert.Error(t, err)
					}
				})
			}
		})
	}
}

func TestValidationCreatePolicy_Resources(t *testing.T) {
	t.Run("duplicate resources", func(t *testing.T) {
		duplicateClasses := map[bool][][]string{
			true: {
				{"*", "iam:users"}, // semantic overlap is not validated
			},
			false: {
				{"this:isA:duplicate", "this:isA:duplicate"},
				{"*", "*"},
			},
		}
		for expectedSuccess, cases := range duplicateClasses {
			t.Run(fmt.Sprintf("%v", expectedSuccess), func(t *testing.T) {
				for _, resources := range cases {
					t.Run(strings.Join(resources, ";"), func(t *testing.T) {
						req := validCreatePolicyReq()
						req.Statements[0].Resources = resources
						err := req.Validate()
						if expectedSuccess {
							assert.NoError(t, err)
						} else {
							assert.Error(t, err)
						}
					})
				}
			})
		}
	})

	// simple checks
	classes := map[bool][]string{
		true: {
			"nodes#:foo",
			"a:test@foo.com:bar",
			"a_a:z",
			"a-a:z",
			"a1a:z",
			"aAa:z",
			"nodes",
			"n",
			"a:b",
			"a:b:c",
			"a:Z",
			"a:0",
			"a:_",
			"a:-",
			"a:run_list",
			"a:run-list",
			"a:test@foo.com",
			"nodes:#oo",
			"nodes:f#o",
			"nodes:fo#",
			"cfgmgmt:nodes:f9047a75-44c5-4626-8b1a-b08a67d4270d:runs:d9047a75-44c5-4626-8b1a-b08a67d4270f",
			"compliance:profiles:storage:OWNER:NAME:VERSION",
			"auth:user:${a2:username}",
			"made:up:${a2:username}:foo:*",
			// Note 2: This is only testing that _what we want to allow_ is OK. The
			// validation does not exclude other inputs that look similar, do any
			// semantic validation. To stress that, note the following, completely
			// made up resources names:
			"foo:user:${a3:petname}",
			"foo:${a2:something}:${a2:petname}",
			"foo:${any}",
			"*",
			"a:*",
			"a:b:*",
			"a:b:c:*",
			"a:b:c:d:*",
		},
		false: {
			"",
			"nodes::foo",
			"nodes:foo:",
			"nodes:",
			"nodes::",
			":foo",
			"::foo",
			"Abc:z",
			"0a:z",
			"-a:z",
			"_a:z",
			"*:foo",
			"nodes:fo*",
			"nodes:f*o",
			"nodes:*oo",
			"**",
			"*:*",
			"%nodes:foo",
			"a::c",
			"a:b::",
		},
	}

	for expectedSuccess, cases := range classes {
		t.Run(fmt.Sprintf("%v", expectedSuccess), func(t *testing.T) {
			for _, resource := range cases {
				t.Run(resource, func(t *testing.T) {
					req := validCreatePolicyReq()
					req.Statements[0].Resources = []string{resource}
					err := req.Validate()
					if expectedSuccess {
						assert.NoError(t, err)
					} else {
						assert.Error(t, err)
					}
				})
			}
		})
	}
}

func TestValidationCreatePolicy_Members(t *testing.T) {
	classes := map[bool][][]string{
		true: {
			[]string{"team:local:admins"},
			[]string{"team:local:admins", "team:local:ops"},
			[]string{"team:local:admins", "team:local:ops", "team:local:local1"},
			[]string{"team:ldap:admins"},
			[]string{"team:ldap:admins", "team:ldap:ops"},
			[]string{"team:ldap:admins", "team:ldap:ops", "team:ldap:ldap2"},
			[]string{"team:saml:admins"},
			[]string{"team:saml:admins", "team:saml:ops"},
			[]string{"team:saml:admins", "team:saml:ops", "team:saml:saml3"},
			[]string{"team:local:schmocal", "team:ldap:ldap2", "team:saml:saml3"},
			[]string{"user:local:local-user-id"},
			[]string{"user:local:local-user-id", "user:local:other-local-user-id"},
			[]string{"user:local:local-user-id", "team:local:ops"},
			[]string{"user:saml:CiQzMGY5ODgzNC1iNGFiLTQxZTMtODgzNC00Njc0M2YyNGNkN2YSBWxvY2Fs"},
			[]string{"user:ldap:CiQzMGY5ODgzNC1iNGFiLTQxZTMtODgzNC00Njc0M2YyNGNkN2YSBWxvY2Fs"},
			[]string{"team:ldap:foo!@#$=+"},
			[]string{"team:ldap:üßë mïñë"},
			[]string{"team:ldap:    "},
			[]string{"token:9c2a3dae-cef1-4eb1-853a-79849de14bd1"},
			[]string{"tls:service:automate-deployment:65dda02fcf830b3bcab2b96517d020ade2ede626f8b5aea3258b75acc8caccd0"},
			[]string{"*"},
			[]string{"token:*"},
			[]string{"user:*"},
			[]string{"user:local:*"},
			[]string{"user:ldap:*"},
			[]string{"user:saml:*"},
			[]string{"team:*"},
			[]string{"team:local:*"},
			[]string{"team:ldap:*"},
			[]string{"team:saml:*"},
			[]string{"tls:service:automate-deployment:*"},
			[]string{"tls:service:*"},
		},
		false: {
			[]string{""}, // this is the same as "not provided"
			[]string{"user::"},
			[]string{"user:local:"},
			[]string{"user:ldap:"},
			[]string{"user:saml:"},
			[]string{"user:mammel:alice"},
			[]string{"user:mammel:"},
			[]string{"team:local:"},
			[]string{"team:ldap:"},
			[]string{"team:saml:"},
			[]string{"team::"},
			[]string{"team::admins"},
			[]string{"muppet:kermit"},
			[]string{"user:muppet:kermit"},
			[]string{"team:muppets:ops"},
			[]string{"team:ldap:muppets:ops"},
			[]string{"team:local:muppets:ops"},
			[]string{"team:*:ops"},
			[]string{"*:local:ops"},
			[]string{"user:local:fo*"},
			[]string{"user:ldap:f*o"},
			[]string{"user:saml:*oo"},
			[]string{"token"},
			[]string{"token:"},
			[]string{"token:middle:*"},
			[]string{"token:fo*"},
			// no longer valid v1 subjects
			[]string{"service:*"},
		},
	}

	for expectedSuccess, cases := range classes {
		t.Run(fmt.Sprintf("%v", expectedSuccess), func(t *testing.T) {
			for _, members := range cases {
				t.Run(strings.Join(members, ","), func(t *testing.T) {
					req := validCreatePolicyReq()
					req.Members = members
					err := req.Validate()
					if expectedSuccess {
						assert.NoError(t, err)
					} else {
						assert.Error(t, err)
					}
				})
			}
		})
	}
}

func validCreatePolicyReq() *v2.CreatePolicyReq {
	return &v2.CreatePolicyReq{
		Id:   "test-id",
		Name: "test",
		Statements: []*v2.Statement{{
			Effect:    v2.Statement_DENY,
			Actions:   []string{"iam:users:delete"},
			Resources: []string{"iam:users"},
		}},
	}
}

func TestValidationCreateRole(t *testing.T) {
	negativeCases := map[string]*v2.CreateRoleReq{
		"empty ID": &v2.CreateRoleReq{
			Id:       "",
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"invalid ID": &v2.CreateRoleReq{
			Id:       "i d !",
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"whitespace ID": &v2.CreateRoleReq{
			Id:       "      ",
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"missing ID": &v2.CreateRoleReq{
			Name:     "name of my team",
			Projects: []string{"test"},
		},
		"empty Name": &v2.CreateRoleReq{
			Id:       "test",
			Name:     "",
			Projects: []string{"test"},
		},
		"whitespace Name": &v2.CreateRoleReq{
			Id:       "test",
			Name:     "          ",
			Projects: []string{"test"},
		},
		"missing Name": &v2.CreateRoleReq{
			Id:       "test",
			Projects: []string{"test"},
		},
		"ID contains invalid characters": &v2.CreateRoleReq{
			Id:       "invalid~~~",
			Name:     "this is valid ~ fun characters",
			Projects: []string{"test", "test-2"},
		},
		"ID contains whitespace": &v2.CreateRoleReq{
			Id:       "invalid id",
			Name:     "this is valid ~ fun characters",
			Projects: []string{"test", "test-2"},
		},
		"whitespace projects list": &v2.CreateRoleReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"     ", "test"},
		},
		"repeated projects in list": &v2.CreateRoleReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"repeat", "repeat"},
		},
		"Project has invalid characters": &v2.CreateRoleReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"valid", "wrong~"},
		},
		"Project has spaces": &v2.CreateRoleReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"valid", "wrong space"},
		},
		"project has uppercase characters": &v2.CreateRoleReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{"valid", "PROJECT1"},
		},
	}

	positiveCases := map[string]*v2.CreateRoleReq{
		"alphanumeric IDs allowed": &v2.CreateRoleReq{
			Id:       "asdf-123-fun",
			Name:     "this is valid ~ fun characters",
			Projects: []string{"test", "test-2"},
		},
		"empty projects list": &v2.CreateRoleReq{
			Id:       "this-is-valid-1",
			Name:     "name of my team ~ fun characters 1 %",
			Projects: []string{},
		},
		"missing projects list": &v2.CreateRoleReq{
			Id:   "this-is-valid-1",
			Name: "name of my team ~ fun characters 1 %",
		},
	}

	classes := map[bool]map[string]*v2.CreateRoleReq{
		true:  positiveCases,
		false: negativeCases,
	}

	for expectedSuccess, cases := range classes {
		for name, tc := range cases {
			t.Run(name, func(t *testing.T) {
				err := tc.Validate()
				if expectedSuccess {
					assert.NoError(t, err)
				} else {
					assert.Error(t, err)
				}
			})
		}
	}
}
