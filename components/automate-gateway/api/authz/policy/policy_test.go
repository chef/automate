// Note that best practices dictate the package for the unit tests should be <foo>_test
// as used here to provide proper encapsulation of the system under test (SUT).
// Unfortunately, in this particular case that means the SUT needs to expose
// a public Reset() function.
// The alternative would be to put these tests in the same package ("policy")
// but that then makes it all too tempting to manipulate internals of the SUT.
package policy_test

import (
	"fmt"
	"regexp"
	"testing"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/components/automate-gateway/api/authz/policy"
)

func TestAuthzPolicy(t *testing.T) {
	t.Run("GetInfoMap", func(t *testing.T) {

		t.Run("returns empty output map when input map is empty",
			func(t *testing.T) {
				setup()

				result := policy.GetInfoMap()

				assert.Zero(t, len(result))
			})

		t.Run("returns ALL items when there are ONLY UNPARAMETERIZED paths",
			func(t *testing.T) {
				setup()
				policy.MapMethodTo("ListFoo", "resource", "action", "GET", "/auth/foo/bar", generateSimpleFunction("any"))
				policy.MapMethodTo("PutFoo", "resource", "action", "PUT", "/auth/foo", generateSimpleFunction("any"))

				result := policy.GetInfoMap()

				assert.Equal(t, 2, len(result))
			})

		t.Run("returns ALL items when there are ONLY PARAMETERIZED paths",
			func(t *testing.T) {
				setup()
				policy.MapMethodTo("ListFoo", "resource", "action", "GET", "/auth/foo/{bar}", generateSimpleFunction("any"))
				policy.MapMethodTo("PutFoo", "resource", "action", "PUT", "/auth/{foo}", generateSimpleFunction("any"))

				result := policy.GetInfoMap()

				assert.Equal(t, 2, len(result))
			})

		t.Run("returns ALL items when there is a MIX of parameterized and unparameterized paths",
			func(t *testing.T) {
				setup()
				policy.MapMethodTo("ListBar", "resource", "action", "GET", "/auth/foo/ar", generateSimpleFunction("any"))
				policy.MapMethodTo("PutBar", "resource", "action", "PUT", "/auth/bar", generateSimpleFunction("any"))
				policy.MapMethodTo("ListFoo", "resource", "action", "GET", "/auth/foo/{baz}", generateSimpleFunction("any"))
				policy.MapMethodTo("PutFoo", "resource", "action", "PUT", "/auth/{foo}", generateSimpleFunction("any"))

				result := policy.GetInfoMap()

				assert.Equal(t, 4, len(result))
			})
	})

	t.Run("Policy", func(t *testing.T) {

		t.Run("passes through ACTION",
			func(t *testing.T) {
				setup()
				policy.MapMethodTo("ListFoo", "resource", "action", "", "", generateSimpleFunction("any"))

				result := policy.InfoForMethod("ListFoo", nil)

				assert.Equal(t, "action", result.Action)
			})

		t.Run("passes through FUNCTION RESULT",
			func(t *testing.T) {
				setup()
				policy.MapMethodTo("ListFoo", "resource", "action", "", "", generateSimpleFunction("any"))

				result := policy.InfoForMethod("ListFoo", nil)

				assert.Equal(t, "any", result.Resource)
			})

		t.Run("supports multiple mapped methods",
			func(t *testing.T) {
				setup()
				policy.MapMethodTo("GetFoo", "any:*", "read", "", "", generateSimpleFunction("foo"))
				policy.MapMethodTo("GetBar", "any:*", "read", "", "", generateSimpleFunction("bar"))
				policy.MapMethodTo("GetBaz", "any:*", "read", "", "", generateSimpleFunction("baz"))

				barResult := policy.InfoForMethod("GetBar", nil)
				fooResult := policy.InfoForMethod("GetFoo", nil)
				bazResult := policy.InfoForMethod("GetBaz", nil)

				assert.Equal(t, "foo", fooResult.Resource)
				assert.Equal(t, "bar", barResult.Resource)
				assert.Equal(t, "baz", bazResult.Resource)
			})

		t.Run("expands resource when ExpansionFunc IS supplied",
			func(t *testing.T) {
				setup()
				action := "read"
				resource := "teams:{id}"
				matcher := `\{id\}` // portion of resource to find and then replace

				policy.MapMethodTo("GetTeam", resource, action, "", "", generateExpandFunction(matcher))
				result := policy.InfoForMethod("GetTeam", container{Id: "xyz"})

				assert.Equal(t, action, result.Action)
				assert.Equal(t, "teams:"+"xyz", result.Resource)
			})

		t.Run("returns resource unmodified when ExpansionFunc is NOT supplied",
			func(t *testing.T) {
				setup()
				action := "read"
				resource := "teams:{id}"

				policy.MapMethodTo("GetTeam", resource, action, "", "", nil)
				result := policy.InfoForMethod("GetTeam", container{Id: "123456"})

				assert.Equal(t, action, result.Action)
				assert.Equal(t, resource, result.Resource)
			})

		t.Run("when method is NOT mapped",
			func(t *testing.T) {
				tests := []struct {
					expandFunc  policy.ExpansionFunc
					description string
				}{
					{generateSimpleFunction("abc"), "expandFunc IS supplied"},
					{nil, "expandFunc is NOT supplied"},
				}
				for _, test := range tests {

					t.Run(fmt.Sprintf("and %s, returns nil", test.description),
						func(t *testing.T) {
							setup()
							policy.MapMethodTo("GetTeam", "teams:{id}", "read", "", "", test.expandFunc)
							result := policy.InfoForMethod("GetOther", container{Id: "123456"})

							assert.Nil(t, result)
						})
				}
			})

		t.Run("returns resource unmodified when resource does NOT have place holder",
			func(t *testing.T) {
				setup()
				matcher := `\{id\}`
				resource := "resource:id" // just a literal, no place holder

				policy.MapMethodTo("GetTeam", resource, "read", "", "", generateExpandFunction(matcher))
				result := policy.InfoForMethod("GetTeam", container{Id: "42"})

				assert.Equal(t, resource, result.Resource)
			})

		t.Run("for spacing variations",
			func(t *testing.T) {
				tests := []struct {
					defMethodName  string
					callMethodName string
					description    string
				}{
					{"GetTeam", "GetTeam", "exact match"},
					{"   GetTeam", "GetTeam", "leading spaces in definition"},
					{"GetTeam   ", "GetTeam", "trailing spaces in definition"},
					{"   GetTeam    ", "GetTeam", "leading and trailing spaces in definition"},
					{"GetTeam", "   GetTeam", "leading spaces in invocation"},
					{"GetTeam", "GetTeam   ", "trailing spaces in invocation"},
					{"GetTeam", "   GetTeam    ", "leading and trailing spaces in invocation"},
				}
				for _, test := range tests {

					t.Run("finds method even with "+test.description,
						func(t *testing.T) {
							setup()
							policy.MapMethodTo(test.defMethodName, "team:*", "action", "", "", generateSimpleFunction("any"))

							result := policy.InfoForMethod(test.callMethodName, nil)

							if assert.NotNil(t, result) {
								assert.Equal(t, "action", result.Action)
								assert.Equal(t, "any", result.Resource)
							}
						})
				}
			})

		t.Run("differentiates cases on DEFINITIONS",
			func(t *testing.T) {
				setup()
				policy.MapMethodTo("GetTeam", "resource", "read", "", "", generateSimpleFunction("uppercase"))
				policy.MapMethodTo("getTeam", "resource", "read", "", "", generateSimpleFunction("lowercase"))

				result := policy.InfoForMethod("GetTeam", nil)

				assert.Equal(t, "uppercase", result.Resource)
			})

		t.Run("differentiates cases on ACCESS",
			func(t *testing.T) {
				setup()
				policy.MapMethodTo("GetTeam", "resource", "read", "", "", generateSimpleFunction("uppercase"))

				result := policy.InfoForMethod("GetTeam", nil)
				assert.Equal(t, "uppercase", result.Resource)

				result = policy.InfoForMethod("getTeam", nil)
				assert.Nil(t, result)
			})

		t.Run("passes through HTTP METHOD",
			func(t *testing.T) {
				setup()
				policy.MapMethodTo("ListFoo", "resource", "action", "GET", "/auth/foo/bar", generateSimpleFunction("any"))

				result := policy.InfoForMethod("ListFoo", nil)

				assert.Equal(t, "GET", result.HTTPMethod)
			})

		t.Run("passes through HTTP ENDPOINT",
			func(t *testing.T) {
				setup()
				policy.MapMethodTo("ListFoo", "resource", "action", "GET", "/auth/foo/bar", generateSimpleFunction("any"))

				result := policy.InfoForMethod("ListFoo", nil)

				assert.Equal(t, "/auth/foo/bar", result.HTTPEndpoint)
			})

	})
}

// ********************************** SUPPORT FUNCTIONS **************************************

type container struct {
	Id string
}

// Arrghh! No support for "before each" in go testing so have to call this manually
// for every test!
func setup() {
	policy.Reset()
}

func generateSimpleFunction(text string) policy.ExpansionFunc {
	return func(pat string, input interface{}) string {
		return text
	}
}

// While not strictly necessary to use this more elaborate generator for unit tests here,
// it more closely models real-world results so one can get a better sense of
// what the code under test is doing.
func generateExpandFunction(matcher string) policy.ExpansionFunc {
	return func(pat string, input interface{}) string {
		item := input.(container)
		return regexp.
			MustCompile(matcher).
			ReplaceAllString(pat, item.Id)
	}
}
