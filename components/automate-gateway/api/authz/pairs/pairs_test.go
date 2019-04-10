package pairs_test

import (
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"github.com/chef/automate/components/automate-gateway/api/authz/pairs"
)

func TestAuthzPairs(t *testing.T) {

	t.Run("InvertMapNonParameterized", func(t *testing.T) {

		t.Run("returns empty output map when the input map is empty",
			func(t *testing.T) {
				inputMap := make(map[string]pairs.Info)

				result := pairs.InvertMapNonParameterized(inputMap)

				assert.Zero(t, len(result))
			})

		t.Run("returns ALL items when there are only UNPARAMETERIZED paths",
			func(t *testing.T) {
				inputMap := getSimpleMethodInfoMap()

				result := pairs.InvertMapNonParameterized(inputMap)

				assert.Equal(t, len(inputMap), len(result))
			})

		t.Run("returns ALL items when there are only PARAMETERIZED paths",
			func(t *testing.T) {
				inputMap := map[string]pairs.Info{
					"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
						"/path/one/{id}", "resource1"),
					"/chef.automate.api.Foo/GetFoo2": getInfoWithPathAndResource(
						"/path/two/{id}", "resource2"),
				}

				result := pairs.InvertMapNonParameterized(inputMap)

				assert.Equal(t, len(inputMap), len(result))
			})

		t.Run("returns NO items when there are only PARAMETERIZED resources",
			func(t *testing.T) {
				inputMap := map[string]pairs.Info{
					"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
						"/path/one", "resource1:{id}"),
					"/chef.automate.api.Foo/GetFoo2": getInfoWithPathAndResource(
						"/path/two", "resource2:{id}"),
				}
				result := pairs.InvertMapNonParameterized(inputMap)

				assert.Zero(t, len(result))
			})

		t.Run("returns ONLY UNPARAMETERIZED items when there is a MIX of parameterized and unparameterized paths",
			func(t *testing.T) {
				inputMap := map[string]pairs.Info{
					"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
						"/some/path", "resource1:{id}"),
					"/chef.automate.api.Foo/GetFoo2": getInfoWithPathAndResource(
						"/some/path", "resource1:{id}:{code}"),
					"/chef.automate.api.Foo/GetFoo3": getInfoWithPathAndResource(
						"/some/path", "compliance:nodes"),
				}

				result := pairs.InvertMapNonParameterized(inputMap)

				assert.Equal(t, 1, len(result))
			})

		t.Run("returns output map with VALUES corresponding to keys of input map",
			func(t *testing.T) {
				inputMap := getSimpleMethodInfoMap()

				result := pairs.InvertMapNonParameterized(inputMap)

				for _, value := range result {
					if _, ok := inputMap[value[0]]; !ok {
						assert.Fail(t, "InvertMapNonParameterized value is not a key in original map")
					}
				}
			})

		t.Run("returns output map with KEYS corresponding to resources/actions of input map",
			func(t *testing.T) {
				inputMap := getSimpleMethodInfoMap()

				result := pairs.InvertMapNonParameterized(inputMap)

				for key, value := range result {
					assert.Equal(t, inputMap[value[0]].Resource, key.Resource)
					assert.Equal(t, inputMap[value[0]].Action, key.Action)
					if _, ok := inputMap[value[0]]; !ok {
						assert.Fail(t, "Output map value is not a key in input map")
					}
				}
			})

		t.Run("returns output map that COVERS entire input map",
			func(t *testing.T) {
				inputMap := getSimpleMethodInfoMap()
				keyList := make([]string, len(inputMap))

				result := pairs.InvertMapNonParameterized(inputMap)

				for _, value := range result {
					keyList = append(keyList, value[0])
				}
				for key := range inputMap {
					assert.Contains(t, keyList, key)
				}
			})

		t.Run("with duplicate resources/actions, cardinality of output map is less",
			func(t *testing.T) {
				inputMap := getMethodInfoMapWithDuplicates()

				result := pairs.InvertMapNonParameterized(inputMap)

				assert.Equal(t, 4, len(inputMap))
				// 3 duplicates folded into 1 slot, plus 1 non-duplicate == 2
				assert.Equal(t, 2, len(result))
			})

		t.Run("cardinality of value array matches duplicate resources/actions count",
			func(t *testing.T) {
				inputMap := getMethodInfoMapWithDuplicates()

				result := pairs.InvertMapNonParameterized(inputMap)

				for key, dupList := range result {
					if key.Resource == "resource1" {
						assert.Equal(t, 3, len(dupList)) // this is the duplicate collection
					} else {
						assert.Equal(t, 1, len(dupList)) // this has the unique one, so array of 1
					}
				}
			})

		t.Run("with duplicate resources/actions, value array elements match input map methods",
			func(t *testing.T) {
				inputMap := getMethodInfoMapWithDuplicates()

				result := pairs.InvertMapNonParameterized(inputMap)

				for key, dupList := range result {
					if key.Resource == "resource1" {
						assert.Contains(t, dupList, "/chef.automate.api.Foo/GetFoo1", dupList)
						assert.Contains(t, dupList, "/chef.automate.api.Other/Other1", dupList)
						assert.Contains(t, dupList, "/chef.automate.api.Other/Other2", dupList)
					} else {
						assert.Contains(t, dupList, "/chef.automate.api.Foo/GetFoo2", dupList)
					}
				}
			})

	})

	t.Run("InvertMapParameterized", func(t *testing.T) {

		t.Run("with NO parameterized endpoints", func(t *testing.T) {

			t.Run("returns empty map when the input map is empty",
				func(t *testing.T) {
					inputMap := make(map[string]pairs.Info)

					result, _ := pairs.InvertMapParameterized(inputMap, "any", nil)

					assert.Zero(t, len(result))
				})

			t.Run("returns empty map when no endpoints match specified path",
				func(t *testing.T) {
					inputMap := getSimpleMethodInfoMap()

					result, _ := pairs.InvertMapParameterized(inputMap, "/auth/users/foo@bar.com", nil)

					assert.Zero(t, len(result))
				})

			t.Run("returns output map reflecting endpoints matching specified path",
				func(t *testing.T) {
					inputMap := getSimpleMethodInfoMap()

					result, _ := pairs.InvertMapParameterized(inputMap, "/some/path/getfoo1", nil)
					assert.Equal(t, 1, len(result))
					result, _ = pairs.InvertMapParameterized(inputMap, "/some/path/getfoo2", nil)
					assert.Equal(t, 1, len(result))
				})

			t.Run("returns output map reflecting endpoints matching specified path with multiple methods",
				func(t *testing.T) {
					inputMap := getMethodInfoMapWithDuplicates()

					result, _ := pairs.InvertMapParameterized(inputMap, "/some/path/getfoo1", nil)
					assert.Equal(t, 1, len(result))
					result, _ = pairs.InvertMapParameterized(inputMap, "/some/path/getfoo2", nil)
					assert.Equal(t, 1, len(result))
				})
		})

		t.Run("with SOME parameterized endpoints", func(t *testing.T) {

			t.Run("returns empty map when no endpoints match path",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/some/path", "resource1:{id}"),
					}
					result, _ := pairs.InvertMapParameterized(inputMap, "/auth/users/foo@bar.com", nil)

					assert.Zero(t, len(result))
				})

			t.Run("reflects endpoints matching path without params",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo4": getInfoWithPathAndResource(
							"/target/path", "resource4"),
					}

					result, _ := pairs.InvertMapParameterized(inputMap, "/target/path", nil)

					verifyResource(t, result, "resource4")
					epList := result[pairs.Pair{Resource: "resource4", Action: "action1"}]
					assert.Equal(t, 1, len(epList))
					assert.Contains(t, epList, "/chef.automate.api.Foo/GetFoo4")
				})

			t.Run("reflects endpoints matching path with param IN MIDDLE and MATCHING PARAM in resource",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo3": getInfoWithPathAndResource(
							"/target/path/{email}/runs", "compliance:resource3:{email}"),
					}

					result, _ := pairs.InvertMapParameterized(inputMap, "/target/path/12345@chef.io/runs", nil)

					verifyResource(t, result, "compliance:resource3:12345@chef.io")
				})

			t.Run("reflects endpoints matching resource with param AT END and two methods",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/target/some/{id}", "resource1:{id}"),
						"/chef.automate.api.Foo/GetFoo2": getInfoWithPathAndResource(
							"/target/some/{id}", "resource1:{id}"),
					}

					result, _ := pairs.InvertMapParameterized(inputMap, "/target/some/path", nil)

					verifyResource(t, result, "resource1:path")
					epList := result[pairs.Pair{Resource: "resource1:path", Action: "action1"}]
					assert.Equal(t, 2, len(epList))
					assert.Contains(t, epList, "/chef.automate.api.Foo/GetFoo1")
					assert.Contains(t, epList, "/chef.automate.api.Foo/GetFoo2")
				})

			t.Run("returns error with one param in resource and DIFFERENT PARAM in endpoint",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo5": getInfoWithPathAndResource(
							"/target/{id}/runs", "compliance:resource5:{email}"),
					}

					_, err := pairs.InvertMapParameterized(inputMap, "/target/9999999/runs", nil)

					require.Error(t, err)
					assert.Regexp(t, "missing|excess", err.Error()) // aborts at first error
				})

			t.Run("returns error when parameter name includes regex meta-characters",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/target/some/{id(}", "resource1:{id}"),
					}
					_, err := pairs.InvertMapParameterized(inputMap, "/target/some/path", nil)

					require.Error(t, err)
					assert.Regexp(t, `invalid meta-char.*\bid\(`, err.Error())
				})

			t.Run("reflects endpoints matching path with TWO PARAMS in resource",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo6": getInfoWithPathAndResource(
							"/target/path/{email}/runs/{id}", "compliance:nodes:{email}:runs:{id}"),
					}

					result, _ := pairs.InvertMapParameterized(inputMap, "/target/path/bob@chef.io/runs/888", nil)

					verifyResource(t, result, "compliance:nodes:bob@chef.io:runs:888")
				})
		})

		t.Run("with some parameterized POST endpoints", func(t *testing.T) {

			t.Run("reflects matching endpoints with NO inputs and NO param",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/target/some/path", "nodes"),
					}
					params := []string{}

					result, _ := pairs.InvertMapParameterized(inputMap, "/target/some/path", params)

					verifyResource(t, result, "nodes")
				})

			t.Run("reflects matching endpoints with ONE param",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/target/some/path", "resource1:{id}"),
					}
					params := []string{"id=12345"}

					result, _ := pairs.InvertMapParameterized(inputMap, "/target/some/path", params)

					verifyResource(t, result, "resource1:12345")
				})

			t.Run("reflects matching endpoints with MULTIPLE params",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/target/some/path", "resource1:{id}:runs:{code}"),
					}
					params := []string{"id=12345", "code=abc"}

					result, _ := pairs.InvertMapParameterized(inputMap, "/target/some/path", params)

					verifyResource(t, result, "resource1:12345:runs:abc")
				})

			t.Run("reflects matching endpoints with MULTIPLE params with inputs REVERSED",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/target/some/path", "resource1:{id}:runs:{code}"),
					}
					params := []string{"code=abc", "id=12345"}

					result, _ := pairs.InvertMapParameterized(inputMap, "/target/some/path", params)

					verifyResource(t, result, "resource1:12345:runs:abc")
				})

			t.Run("returns error when parameter name includes regex meta-characters",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/target/some/path", "resource1:{id}"),
					}
					params := []string{"id(=12345"} // extra parentheses in there for the test!

					_, err := pairs.InvertMapParameterized(inputMap, "/target/some/path", params)

					require.Error(t, err)
					assert.Regexp(t, `invalid input.*\bid\(`, err.Error())
				})

			t.Run("returns no error with ALL excess inputs and thus no params in resource",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/target/some/path", "resource1:nodes"),
					}
					params := []string{"id=12345", "email=foo@bar.com"}
					_, err := pairs.InvertMapParameterized(inputMap, "/target/some/path", params)
					require.NoError(t, err)
				})

			t.Run("returns no error with ONE excess input and ONE satisfied param in resource",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/target/some/path", "resource1:{id}"),
					}
					params := []string{"id=12345", "email=foo@bar.com"}
					_, err := pairs.InvertMapParameterized(inputMap, "/target/some/path", params)
					require.NoError(t, err)
				})

			t.Run("POST inputs takes precedence over inputs inferred from URL",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/cfgmgmt/nodes/{node_id}/runs/{run_id}", "cfgmgmt:nodes:{node_id}:runs:{run_id}"),
					}

					params := []string{} // no POST input as baseline
					result, _ := pairs.InvertMapParameterized(inputMap, "/cfgmgmt/nodes/node_23/runs/run_688", params)
					verifyResource(t, result, "cfgmgmt:nodes:node_23:runs:run_688")

					params = []string{"node_id=99", "run_id=abcde"}
					result, _ = pairs.InvertMapParameterized(inputMap, "/cfgmgmt/nodes/node_23/runs/run_688", params)
					verifyResource(t, result, "cfgmgmt:nodes:99:runs:abcde")
				})

			t.Run("POST inputs takes precedence over inputs inferred from URL even if faulty inputs",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/cfgmgmt/nodes/{node_id}/runs/{run_id}", "cfgmgmt:nodes:{node_id}:runs:{run_id}"),
					}

					params := []string{"node_id=99"} // missing run_id
					_, err := pairs.InvertMapParameterized(inputMap, "/cfgmgmt/nodes/node_23/runs/run_688", params)
					require.Error(t, err)
					assert.Regexp(t, `missing.*run_id`, err.Error())
				})

			t.Run("returns error due to ALL unsatisfied params",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/target/some/path", "resource1:{id}:{code}"),
					}
					params := []string{}

					_, err := pairs.InvertMapParameterized(inputMap, "/target/some/path", params)

					require.Error(t, err)
					assert.Regexp(t, `missing.*id.*code`, err.Error())
				})

			t.Run("returns error with ONE satisfied param and one unsatisfied param",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/target/some/path", "resource1:{id}:{email}"),
					}
					params := []string{"id=12345"}

					_, err := pairs.InvertMapParameterized(inputMap, "/target/some/path", params)

					require.Error(t, err)
					assert.Regexp(t, "missing.*email", err.Error())
				})

			t.Run("returns error due to input with EMPTY VALUE",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/target/some/path", "resource1:{id}"),
					}
					params := []string{"id="}

					_, err := pairs.InvertMapParameterized(inputMap, "/target/some/path", params)

					require.Error(t, err)
					assert.Regexp(t, `\bid\b.*empty`, err.Error())
				})

			t.Run("returns error due to input with EMPTY NAME",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/target/some/path", "resource1:{id}"),
					}
					params := []string{"=42"}

					_, err := pairs.InvertMapParameterized(inputMap, "/target/some/path", params)

					require.Error(t, err)
					assert.Regexp(t, "missing.*id", err.Error())
				})

			t.Run("returns error due to EMPTY input",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/target/some/path", "resource1:{id}"),
					}
					params := []string{""}

					_, err := pairs.InvertMapParameterized(inputMap, "/target/some/path", params)

					require.Error(t, err)
					assert.Regexp(t, "missing.*id", err.Error())
				})

			t.Run("returns error due to INVALID input",
				func(t *testing.T) {
					inputMap := map[string]pairs.Info{
						"/chef.automate.api.Foo/GetFoo1": getInfoWithPathAndResource(
							"/target/some/path", "resource1:{id}"),
					}
					params := []string{"id123"} // missing '='

					_, err := pairs.InvertMapParameterized(inputMap, "/target/some/path", params)

					require.Error(t, err)
					assert.Regexp(t, "missing.*id", err.Error())
				})

		})
	})

	t.Run("GetKeys", func(t *testing.T) {

		t.Run("returns empty output map when the input map is empty",
			func(t *testing.T) {
				inputMap := make(map[pairs.Pair][]string)

				result := pairs.GetKeys(inputMap)

				assert.Zero(t, len(result))
			})

		t.Run("returns output map of SAME CARDINALITY of input map",
			func(t *testing.T) {
				inputMap := getPairMethodMap()

				result := pairs.GetKeys(inputMap)

				assert.Equal(t, len(inputMap), len(result))
			})

		t.Run("returns output map with keys of input map",
			func(t *testing.T) {
				inputMap := getPairMethodMap()

				result := pairs.GetKeys(inputMap)

				for key := range inputMap {
					assert.Contains(t, result, &key)
				}
			})
	})

	t.Run("GetEndpointMapFromResponse", func(t *testing.T) {

		for _, testcase := range []bool{true, false} {

			t.Run("response cardinality matches request so hydration does not matter",
				func(t *testing.T) {
					methodInfoMap := getSimpleMethodInfoMap()
					endptsInMap := []string{"/some/path/getfoo1", "/some/path/getfoo2"}

					result, err := pairs.GetEndpointMapFromResponse(
						getSimpleResponse(),
						methodInfoMap,
						pairs.InvertMapNonParameterized(methodInfoMap),
						testcase)

					require.NoError(t, err)
					assert.Equal(t, 2, len(result))
					for httpEndpoint := range result {
						assert.Contains(t, endptsInMap, httpEndpoint)
					}
					for _, httpEndpoint := range endptsInMap {
						assert.NotNil(t, result[httpEndpoint])
					}
				})
		}
		t.Run("returns entry ONLY for endpoints in response when not hydrated",
			func(t *testing.T) {
				endptsInMap := []string{"/some/path/getfoo1"}
				methodInfoMap := getSimpleMethodInfoMap() // requests 2 endpoints
				response := []*pairs.Pair{                // ... but responds with just 1
					{
						Resource: "resource1",
						Action:   "action1",
					},
				}
				result, err := pairs.GetEndpointMapFromResponse(
					response,
					methodInfoMap,
					pairs.InvertMapNonParameterized(methodInfoMap),
					false)

				require.NoError(t, err)
				assert.Equal(t, 1, len(result))
				for httpEndpoint := range result {
					assert.Contains(t, endptsInMap, httpEndpoint)
				}
				for _, httpEndpoint := range endptsInMap {
					assert.NotNil(t, result[httpEndpoint])
				}
			})

		t.Run("returns entry for ALL requested endpoints when hydrated",
			func(t *testing.T) {
				endptsInMap := []string{"/some/path/getfoo1", "/some/path/getfoo2"}
				methodInfoMap := getSimpleMethodInfoMap() // requests 2 endpoints
				response := []*pairs.Pair{                // ... but responds with just 1
					{
						Resource: "resource1",
						Action:   "action1",
					},
				}
				result, err := pairs.GetEndpointMapFromResponse(
					response,
					methodInfoMap,
					pairs.InvertMapNonParameterized(methodInfoMap),
					true)

				require.NoError(t, err)
				assert.Equal(t, 2, len(result))
				for httpEndpoint := range result {
					assert.Contains(t, endptsInMap, httpEndpoint)
				}
				for _, httpEndpoint := range endptsInMap {
					assert.NotNil(t, result[httpEndpoint])
				}
			})

		t.Run("returns supported methods for each endpoint with ALL unique resource/action pairs",
			func(t *testing.T) {
				methodInfoMap := getSimpleMethodInfoMap()

				result, err := pairs.GetEndpointMapFromResponse(
					getSimpleResponse(),
					methodInfoMap,
					pairs.InvertMapNonParameterized(methodInfoMap),
					false)

				require.NoError(t, err)
				assert.Equal(t, 2, len(result))
				methodsAllowed := result["/some/path/getfoo1"]
				assert.False(t, methodsAllowed.Get)
				assert.True(t, methodsAllowed.Put)
				assert.False(t, methodsAllowed.Post)
				assert.False(t, methodsAllowed.Delete)
				methodsAllowed = result["/some/path/getfoo2"]
				assert.True(t, methodsAllowed.Get)
				assert.False(t, methodsAllowed.Put)
				assert.False(t, methodsAllowed.Post)
				assert.False(t, methodsAllowed.Delete)
			})

		t.Run("returns supported methods for each endpoint with NON-unique resource/action pairs",
			func(t *testing.T) {
				methodInfoMap := getMethodInfoMapWithDuplicates()

				result, err := pairs.GetEndpointMapFromResponse(
					getSimpleResponse(),
					methodInfoMap,
					pairs.InvertMapNonParameterized(methodInfoMap),
					false)

				require.NoError(t, err)
				assert.Equal(t, 2, len(result))
				methodsAllowed := result["/some/path/getfoo1"]
				assert.True(t, methodsAllowed.Get)
				assert.True(t, methodsAllowed.Put)
				assert.True(t, methodsAllowed.Post)
				assert.False(t, methodsAllowed.Delete)
				methodsAllowed = result["/some/path/getfoo2"]
				assert.True(t, methodsAllowed.Get)
				assert.False(t, methodsAllowed.Put)
				assert.False(t, methodsAllowed.Post)
				assert.False(t, methodsAllowed.Delete)
			})

		t.Run("returns supported methods for each endpoint even when http paths differ for same resource/action pair",
			func(t *testing.T) {
				methodInfoMap := getMethodInfoMapWithDuplicates()
				methodInfoMap["/chef.automate.api.Other/Other1"] = pairs.Info{
					Resource:     methodInfoMap["/chef.automate.api.Other/Other1"].Resource,
					Action:       methodInfoMap["/chef.automate.api.Other/Other1"].Action,
					HTTPMethod:   methodInfoMap["/chef.automate.api.Other/Other1"].HTTPMethod,
					HTTPEndpoint: "/some/other/path",
				}

				result, _ := pairs.GetEndpointMapFromResponse(
					getSimpleResponse(),
					methodInfoMap,
					pairs.InvertMapNonParameterized(methodInfoMap),
					false)

				assert.Equal(t, 3, len(result))
				methodsAllowed := result["/some/path/getfoo1"]
				assert.False(t, methodsAllowed.Get) // moved off of here...
				assert.True(t, methodsAllowed.Put)
				assert.True(t, methodsAllowed.Post)
				assert.False(t, methodsAllowed.Delete)
				assert.False(t, methodsAllowed.Patch)
				methodsAllowed = result["/some/other/path"]
				assert.True(t, methodsAllowed.Get) // ... to here
				assert.False(t, methodsAllowed.Put)
				assert.False(t, methodsAllowed.Post)
				assert.False(t, methodsAllowed.Delete)
				assert.False(t, methodsAllowed.Patch)
				methodsAllowed = result["/some/path/getfoo2"]
				assert.True(t, methodsAllowed.Get)
				assert.False(t, methodsAllowed.Put)
				assert.False(t, methodsAllowed.Post)
				assert.False(t, methodsAllowed.Delete)
				assert.False(t, methodsAllowed.Patch)
			})

		t.Run("returns empty output list for empty input map",
			func(t *testing.T) {
				methodInfoMap := getMethodInfoMapWithDuplicates()

				result, _ := pairs.GetEndpointMapFromResponse(
					[]*pairs.Pair{},
					methodInfoMap,
					pairs.InvertMapNonParameterized(methodInfoMap),
					false)

				assert.Zero(t, len(result))
			})

		t.Run("returns error for unsupported HTTP method",
			func(t *testing.T) {
				methodInfoMap := map[string]pairs.Info{
					"/chef.automate.api.Foo/GetFoo1": {
						Resource:     "resource1",
						Action:       "action1",
						HTTPEndpoint: "/some/path/getfoo1",
						HTTPMethod:   "UPDATE",
					},
				}

				result, err := pairs.GetEndpointMapFromResponse(
					getSimpleResponse(),
					methodInfoMap,
					pairs.InvertMapNonParameterized(methodInfoMap),
					false)

				assert.Error(t, err)
				assert.Nil(t, result)
			})

		t.Run("ignores methods not exposed via HTTP",
			func(t *testing.T) {
				methodInfoMap := map[string]pairs.Info{
					"/chef.automate.api.Foo/GetFoo1": {
						Resource: "resource1",
						Action:   "action1",
					},
				}

				result, err := pairs.GetEndpointMapFromResponse(
					getSimpleResponse(),
					methodInfoMap,
					pairs.InvertMapNonParameterized(methodInfoMap),
					false)

				assert.NoError(t, err)
				assert.Zero(t, len(result))
			})
	})
}

func verifyResource(t *testing.T, result map[pairs.Pair][]string, resource string) {
	t.Helper()
	assert.Equal(t, 1, len(result))
	for key := range result { // just 1 item in map
		assert.Equal(t, resource, key.Resource)
	}
}

func getSimpleResponse() []*pairs.Pair {
	return []*pairs.Pair{
		{
			Resource: "resource1",
			Action:   "action1",
		},
		{
			Resource: "resource2",
			Action:   "action2",
		},
	}
}

func getSimpleMethodInfoMap() map[string]pairs.Info {
	inputMap := map[string]pairs.Info{
		"/chef.automate.api.Foo/GetFoo1": {
			Resource:     "resource1",
			Action:       "action1",
			HTTPEndpoint: "/some/path/getfoo1",
			HTTPMethod:   "PUT",
		},
		"/chef.automate.api.Foo/GetFoo2": {
			Resource:     "resource2",
			Action:       "action2",
			HTTPEndpoint: "/some/path/getfoo2",
			HTTPMethod:   "GET",
		},
	}
	return inputMap
}

func getMethodInfoMapWithDuplicates() map[string]pairs.Info {
	inputMap := getSimpleMethodInfoMap()
	inputMap["/chef.automate.api.Other/Other1"] = pairs.Info{
		Resource:     "resource1",
		Action:       "action1",
		HTTPEndpoint: "/some/path/getfoo1",
		HTTPMethod:   "GET",
	}
	inputMap["/chef.automate.api.Other/Other2"] = pairs.Info{
		Resource:     "resource1",
		Action:       "action1",
		HTTPEndpoint: "/some/path/getfoo1",
		HTTPMethod:   "POST",
	}
	return inputMap
}

func getPairMethodMap() map[pairs.Pair][]string {
	pairMap := make(map[pairs.Pair][]string)
	pairMap[pairs.Pair{Resource: "res1", Action: "act1"}] = []string{"method1"}
	pairMap[pairs.Pair{Resource: "res2", Action: "act2"}] = []string{"method2A", "method2B", "method2C"}
	return pairMap
}

func getInfoWithPathAndResource(path, resource string) pairs.Info {
	return pairs.Info{
		Resource:     resource,
		Action:       "action1",
		HTTPEndpoint: path,
		HTTPMethod:   "POST",
	}
}
