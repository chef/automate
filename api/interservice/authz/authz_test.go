package authz_test

import (
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/components/authz-service/engine"
	"github.com/chef/automate/lib/grpc/grpctest"

	// to make init func register proto looked for in TestGeneratedProtobufUpToDate
	_ "github.com/chef/automate/api/interservice/authz/common"
	_ "github.com/chef/automate/api/interservice/authz/v2"
)

func TestGeneratedProtobufUpToDate(t *testing.T) {
	grpctest.AssertCompiledInUpToDate(t, "api/interservice/authz/")
}

type validatable interface {
	Validate() error
}

type inputTestSuite struct {
	negative, positive, variable, wildcards map[string]string // description => input
}

type inputSubjectsTestSuite struct {
	negative, positive, wildcards map[string][]string // description => inputs
}

var (
	// Per new v2 guidelines, we have adopted lowerCamelCase for IAM v2 actions and resources.
	// Thus, the validation regexes (in authz.proto) are now identical
	// so actions and resources may use the same set of tests here.
	// However, those regexes at present are also supporting v1,
	// which permitted underscores in resources and hyphens in actions.
	// So rather than allowing just letters, the regexes allow all characters,
	// which is reflected in these tests.
	actionAndResourceTests = inputTestSuite{
		negative: map[string]string{
			"empty":                            "",
			"with empty subsection":            "nodes::foo",
			"with empty last section":          "nodes:foo:",
			"with last section, one colon":     "nodes:",
			"with empty sub- and last section": "nodes::",
			"with empty first section":         ":foo",
			"with empty first two sections":    "::foo",
			"starts with A-Z":                  "Abc:z",
			"starts with 0-9":                  "0a:z",
			"starts with hyphen":               "-a:z",
			"starts with underscore":           "_a:z",
			"misplaced wildcard":               "*:foo",
			"prefixed wildcard":                "nodes:fo*",
			"non-separated wildcard":           "nodes:f*o",
			"suffixed wildcard":                "nodes:*oo",
			"double adjacent asterisk":         "**",
			"double asterisk per section":      "*:*",
			"starts with special character":    "%nodes:foo",
			"has empty section":                "a::c",
			"ends with empty section":          "a:b::",
		},
		positive: map[string]string{
			"contains special character in prefix": "nodes#:foo",
			"has email in the middle section":      "a:test@foo.com:bar",
			"prefix contains underscore":           "a_a:z",
			"prefix contains hyphen":               "a-a:z",
			"prefix contains 0-9":                  "a1a:z",
			"prefix contains A-Z":                  "aAa:z",
			"without colon":                        "nodes",
			"short word":                           "n",
			"two sections":                         "a:b",
			"three sections":                       "a:b:c",
			"ends with A-Z":                        "a:Z",
			"ends with 0-9":                        "a:0",
			"ends with underscore":                 "a:_",
			"ends with hyphen":                     "a:-",
			"contains an underscore":               "a:run_list",
			"contains a hyphen":                    "a:run-list",
			"is an email":                          "a:test@foo.com",
			"starts with special character":        "nodes:#oo",
			"contains special character":           "nodes:f#o",
			"ends with special character":          "nodes:fo#",
			"contains two ids":                     "cfgmgmt:nodes:f9047a75-44c5-4626-8b1a-b08a67d4270d:runs:d9047a75-44c5-4626-8b1a-b08a67d4270f",
			"contains many colons":                 "compliance:profiles:storage:OWNER:NAME:VERSION",
		},
		// Note: these are valid for CreatePolicyReq, and contrary to wildcards
		// below, they MAY also be valid for IsAuthorizedReq. However, with that,
		// they make no sense at all. So we're only testing these with
		// CreatePolicyReq.
		variable: map[string]string{
			"contains variable":              "auth:user:${a2:username}",
			"contains variable and wildcard": "made:up:${a2:username}:foo:*",
			// Note 2: This is only testing that _what we want to allow_ is OK. The
			// validation does not exclude other inputs that look similar, do any
			// semantic validation. To stress that, note the following, completely
			// made up resources names:
			"contains another variable": "foo:user:${a3:petname}",
			"contains two variables":    "foo:${a2:something}:${a2:petname}",
			"contains any variable":     "foo:${any}",
		},
		// Note: these wildcards (and those below) are valid for CreatePolicyReq
		// but invalid for IsAuthorizedReq.
		wildcards: map[string]string{
			"only wildcard":               "*",
			"one section and wildcard":    "a:*",
			"two sections and wildcard":   "a:b:*",
			"three sections and wildcard": "a:b:c:*",
			"four sections and wildcard":  "a:b:c:d:*",
		},
	}

	subjectsTests = inputSubjectsTestSuite{
		negative: map[string][]string{
			"empty string":                  engine.Subject(""), // this it the same as "not provided"
			"empty connector id":            engine.Subject("user::"),
			"empty local user id":           engine.Subject("user:local:"),
			"empty ldap user id":            engine.Subject("user:ldap:"),
			"empty saml user id":            engine.Subject("user:saml:"),
			"unknown user type":             engine.Subject("user:mammel:alice"),
			"unknown user type, empty id":   engine.Subject("user:mammel:"),
			"empty local team id":           engine.Subject("team:local:"),
			"empty ldap team id":            engine.Subject("team:ldap:"),
			"empty saml team id":            engine.Subject("team:saml:"),
			"empty team section and id":     engine.Subject("team::"),
			"missing team section":          engine.Subject("team::admins"),
			"unknown namespace":             engine.Subject("muppet:kermit"),
			"too many (two) colons in user": engine.Subject("user:muppet:kermit"),
			"unknown namespace for teams":   engine.Subject("team:muppets:ops"),
			"no colon allowed in team name": engine.Subject("team:ldap:muppets:ops"),
			"not restricting to ldap":       engine.Subject("team:local:muppets:ops"),
			"misplaced wildcard/middle":     engine.Subject("team:*:ops"),
			"misplaced wildcard/start":      engine.Subject("*:local:ops"),
			"prefixed wildcard":             engine.Subject("user:local:fo*"),
			"middle wildcard":               engine.Subject("user:ldap:f*o"),
			"suffixed wildcard":             engine.Subject("user:saml:*oo"),
			"only token":                    engine.Subject("token"),
			"empty token id":                engine.Subject("token:"),
			"token with section":            engine.Subject("token:middle:*"),
			"prefixed token wildcard":       engine.Subject("token:fo*"),
		},
		positive: map[string][]string{
			"one local team":              engine.Subject("team:local:admins"),
			"two local teams":             engine.Subject("team:local:admins", "team:local:ops"),
			"three local teams":           engine.Subject("team:local:admins", "team:local:ops", "team:local:local1"),
			"one ldap team":               engine.Subject("team:ldap:admins"),
			"two ldap teams":              engine.Subject("team:ldap:admins", "team:ldap:ops"),
			"three ldap teams":            engine.Subject("team:ldap:admins", "team:ldap:ops", "team:ldap:ldap2"),
			"one saml team":               engine.Subject("team:saml:admins"),
			"two saml teams":              engine.Subject("team:saml:admins", "team:saml:ops"),
			"three saml teams":            engine.Subject("team:saml:admins", "team:saml:ops", "team:saml:saml3"),
			"three mixed teams":           engine.Subject("team:local:schmocal", "team:ldap:ldap2", "team:saml:saml3"),
			"one user":                    engine.Subject("user:local:local-user-id"),
			"two users":                   engine.Subject("user:local:local-user-id", "user:local:other-local-user-id"),
			"one user and one local team": engine.Subject("user:local:local-user-id", "team:local:ops"),
			"an opaque saml id":           engine.Subject("user:saml:CiQzMGY5ODgzNC1iNGFiLTQxZTMtODgzNC00Njc0M2YyNGNkN2YSBWxvY2Fs"),
			"an opaque ldap id":           engine.Subject("user:ldap:CiQzMGY5ODgzNC1iNGFiLTQxZTMtODgzNC00Njc0M2YyNGNkN2YSBWxvY2Fs"),
			"any chars for ldap":          engine.Subject("team:ldap:foo!@#$=+"),
			"extended chars":              engine.Subject("team:ldap:üßë mïñë"),
			"whitespace only":             engine.Subject("team:ldap:    "),
			"token with uuid":             engine.Subject("token:9c2a3dae-cef1-4eb1-853a-79849de14bd1"),
			"service name":                engine.Subject("tls:service:automate-deployment:65dda02fcf830b3bcab2b96517d020ade2ede626f8b5aea3258b75acc8caccd0"),
		},
		wildcards: map[string][]string{
			"wildcard":                   engine.Subject("*"),
			"token wildcard":             engine.Subject("token:*"),
			"user wildcard":              engine.Subject("user:*"),
			"local user wildcard":        engine.Subject("user:local:*"),
			"ldap user wildcard":         engine.Subject("user:ldap:*"),
			"saml user wildcard":         engine.Subject("user:saml:*"),
			"team wildcard":              engine.Subject("team:*"),
			"local team wildcard":        engine.Subject("team:local:*"),
			"ldap team wildcard":         engine.Subject("team:ldap:*"),
			"saml team wildcard":         engine.Subject("team:saml:*"),
			"service name with wildcard": engine.Subject("tls:service:automate-deployment:*"),
			"service tls wildcard":       engine.Subject("tls:service:*"),
			"service wildcard":           engine.Subject("service:*"),
		},
	}
)

func TestValidateCreatePolicyReq(t *testing.T) {
	t.Run("resource", func(t *testing.T) {
		cases := map[string]struct {
			cases    map[string]string
			testFunc func(validatable) func(*testing.T)
		}{
			"negative": {actionAndResourceTests.negative, expectFailure},
			"positive": {actionAndResourceTests.positive, expectSuccess},
			"variable": {actionAndResourceTests.variable, expectSuccess},
			"wildcard": {actionAndResourceTests.wildcards, expectSuccess},
		}
		for class, test := range cases {
			t.Run(class, func(t *testing.T) {
				for name, tc := range test.cases {
					t.Run(name, test.testFunc(&authz.CreatePolicyReq{
						Subjects: engine.Subject("team:local:admins"),
						Resource: tc,
						Action:   "read",
					}))
				}
			})
		}
	})

	t.Run("action", func(t *testing.T) {
		cases := map[string]struct {
			cases    map[string]string
			testFunc func(validatable) func(*testing.T)
		}{
			"negative": {actionAndResourceTests.negative, expectFailure},
			"positive": {actionAndResourceTests.positive, expectSuccess},
			"wildcard": {actionAndResourceTests.wildcards, expectSuccess},
		}
		for class, test := range cases {
			t.Run(class, func(t *testing.T) {
				for name, tc := range test.cases {
					t.Run(name, test.testFunc(&authz.CreatePolicyReq{
						Subjects: engine.Subject("team:local:admins"),
						Resource: "nodes:foo",
						Action:   tc,
					}))
				}
			})
		}
	})

	t.Run("subject", func(t *testing.T) {
		cases := map[string]struct {
			cases    map[string][]string
			testFunc func(validatable) func(*testing.T)
		}{
			"negative": {subjectsTests.negative, expectFailure},
			"positive": {subjectsTests.positive, expectSuccess},
			"wildcard": {subjectsTests.wildcards, expectSuccess},
		}
		for class, test := range cases {
			t.Run(class, func(t *testing.T) {
				for name, tc := range test.cases {
					t.Run(name, test.testFunc(&authz.CreatePolicyReq{
						Subjects: tc,
						Resource: "nodes:foo",
						Action:   "read",
					}))
				}
			})
		}
	})
}

func TestValidateDeletePolicyReq(t *testing.T) {
	negativeCases := map[string]string{
		"empty":                      "",
		"not a UUIDv4":               "IAmaUuidV4pleaseBelieveMe",
		"same in length as a UUIDv4": "11111111-1111-1111-1111-111111111111",
	}
	positiveCases := map[string]string{
		"proper UUIDv4":                "1d36c4ff-d0cb-4ad1-90ed-55a892a0d9c8",
		"proper UUIDv4 with uppercase": "1D36C4FF-D0CB-4AD1-90ED-55A892A0D9C8",
		"looks odd but is UUIDv4":      "00000000-0000-4000-8000-000000000000",
	}

	tests := map[string]struct {
		cases    map[string]string
		testFunc func(validatable) func(*testing.T)
	}{
		"negative": {negativeCases, expectFailure},
		"positive": {positiveCases, expectSuccess},
	}

	for class, test := range tests {
		t.Run(class, func(t *testing.T) {
			for name, tc := range test.cases {
				t.Run(name, test.testFunc(&authz.DeletePolicyReq{Id: tc}))
			}
		})
	}
}

func TestValidateIsAuthorizedReq(t *testing.T) {
	// Note that wildcards are used only for defining policies,
	// not when checking authorization. Thus, it is useful to ensure
	// that wildcards are disallowed in the input in this set of tests.
	t.Run("resource", func(t *testing.T) {
		cases := map[string]struct {
			cases    map[string]string
			testFunc func(validatable) func(*testing.T)
		}{
			"negative": {actionAndResourceTests.negative, expectFailure},
			"positive": {actionAndResourceTests.positive, expectSuccess},
			"wildcard": {actionAndResourceTests.wildcards, expectFailure},
		}
		for class, test := range cases {
			t.Run(class, func(t *testing.T) {
				for name, tc := range test.cases {
					t.Run(name, test.testFunc(&authz.IsAuthorizedReq{
						Subjects: engine.Subject("team:local:admins"),
						Resource: tc,
						Action:   "read",
					}))
				}
			})
		}
	})

	t.Run("action", func(t *testing.T) {
		cases := map[string]struct {
			cases    map[string]string
			testFunc func(validatable) func(*testing.T)
		}{
			"negative": {actionAndResourceTests.negative, expectFailure},
			"positive": {actionAndResourceTests.positive, expectSuccess},
			"wildcard": {actionAndResourceTests.wildcards, expectFailure},
		}
		for class, test := range cases {
			t.Run(class, func(t *testing.T) {
				for name, tc := range test.cases {
					t.Run(name, test.testFunc(&authz.IsAuthorizedReq{
						Subjects: engine.Subject("team:local:admins"),
						Resource: "nodes:foo",
						Action:   tc,
					}))
				}
			})
		}
	})

	t.Run("subject", func(t *testing.T) {
		cases := map[string]struct {
			cases    map[string][]string
			testFunc func(validatable) func(*testing.T)
		}{
			"negative": {subjectsTests.negative, expectFailure},
			"positive": {subjectsTests.positive, expectSuccess},
			"wildcard": {subjectsTests.wildcards, expectFailure},
		}
		for class, test := range cases {
			t.Run(class, func(t *testing.T) {
				for name, tc := range test.cases {
					t.Run(name, test.testFunc(&authz.IsAuthorizedReq{
						Subjects: tc,
						Resource: "nodes:foo",
						Action:   "read",
					}))
				}
			})
		}
	})
}

func TestValidateFilterAuthorizedPairsReq(t *testing.T) {
	t.Run("subject", func(t *testing.T) {
		cases := map[string]struct {
			cases    map[string][]string
			testFunc func(validatable) func(*testing.T)
		}{
			"negative": {subjectsTests.negative, expectFailure},
			"positive": {subjectsTests.positive, expectSuccess},
			"wildcard": {subjectsTests.wildcards, expectFailure},
		}
		for class, test := range cases {
			t.Run(class, func(t *testing.T) {
				for name, tc := range test.cases {
					t.Run(name, test.testFunc(&authz.FilterAuthorizedPairsReq{
						Subjects: tc,
						Pairs: []*authz.Pair{
							{
								Resource: "nodes:foo",
								Action:   "read",
							},
						},
					}))
				}
			})
		}
	})

	t.Run("pairs", func(t *testing.T) {
		t.Run("empty list is ok", expectSuccess(&authz.FilterAuthorizedPairsReq{
			Subjects: []string{"user:local:someid"},
			Pairs:    []*authz.Pair{},
		}))
	})

	t.Run("pairs/resource", func(t *testing.T) {
		cases := map[string]struct {
			cases    map[string]string
			testFunc func(validatable) func(*testing.T)
		}{
			"negative": {actionAndResourceTests.negative, expectFailure},
			"positive": {actionAndResourceTests.positive, expectSuccess},
			"wildcard": {actionAndResourceTests.wildcards, expectFailure},
		}
		for class, test := range cases {
			t.Run(class, func(t *testing.T) {
				for name, tc := range test.cases {
					t.Run(name, test.testFunc(&authz.FilterAuthorizedPairsReq{
						Subjects: []string{"user:local:someid"},
						Pairs: []*authz.Pair{
							{
								Resource: tc,
								Action:   "read",
							},
						},
					}))
				}
			})
		}
	})

	t.Run("pairs/action", func(t *testing.T) {
		cases := map[string]struct {
			cases    map[string]string
			testFunc func(validatable) func(*testing.T)
		}{
			"negative": {actionAndResourceTests.negative, expectFailure},
			"positive": {actionAndResourceTests.positive, expectSuccess},
			"wildcard": {actionAndResourceTests.wildcards, expectFailure},
		}
		for class, test := range cases {
			t.Run(class, func(t *testing.T) {
				for name, tc := range test.cases {
					t.Run(name, test.testFunc(&authz.FilterAuthorizedPairsReq{
						Subjects: []string{"user:local:someid"},
						Pairs: []*authz.Pair{
							{
								Resource: "cfgmgmt:nodes",
								Action:   tc,
							},
						},
					}))
				}
			})
		}
	})
}

func TestValidatePurgeSubjectFromPolicies(t *testing.T) {
	t.Run("subject", func(t *testing.T) {
		cases := map[string]struct {
			cases    map[string][]string
			testFunc func(validatable) func(*testing.T)
		}{
			"negative": {subjectsTests.negative, expectFailure},
			"positive": {subjectsTests.positive, expectSuccess},
			"wildcard": {subjectsTests.wildcards, expectSuccess},
		}
		for class, test := range cases {
			t.Run(class, func(t *testing.T) {
				for name, tc := range test.cases {
					t.Run(name, func(t *testing.T) {
						for _, sub := range tc {
							t.Run(sub, test.testFunc(&authz.PurgeSubjectFromPoliciesReq{
								Subject: sub,
							}))
						}
					})
				}
			})
		}
	})
}

// HELPERS

func expectFailure(req validatable) func(*testing.T) {
	return func(t *testing.T) {
		assert.Error(t, req.Validate())
	}
}

func expectSuccess(req validatable) func(*testing.T) {
	return func(t *testing.T) {
		assert.NoError(t, req.Validate())
	}
}
