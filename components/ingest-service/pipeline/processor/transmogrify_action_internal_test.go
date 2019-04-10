package processor

import (
	"fmt"
	"io/ioutil"
	"testing"

	"github.com/stretchr/testify/assert"
)

var rawactions map[string][]byte

func init() {
	// load chef_action's for reuse
	rawactions = make(map[string][]byte)
	actions := []string{
		"bag_create",
		"bag_create",
		"bag_delete",
		"client_create",
		"cookbookartifactversion_update",
		"environment_create",
		"environment_delete",
		"environment_update",
		"group_create",
		"group_update",
		"item_bag_create",
		"item_bag_update",
		"node_create",
		"node_delete",
		"org_create",
		"permission_update_container",
		"permission_update_cookbook",
		"permission_update_environment",
		"policy_update",
		"user_associate",
		"user_create",
		"user_invite",
		"user_update",
		"version_cookbook_create",
		"version_cookbook_update",
	}
	for _, a := range actions {
		content, err := ioutil.ReadFile(fmt.Sprintf("../../examples/actions/%s.json", a))
		if err != nil {
			panic(err)
		}
		rawactions[a] = content
	}
}

func TestTransmogrifyActionGetActionDataFieldInBytesExamples(t *testing.T) {
	for r, content := range rawactions {
		t.Run("get data field in bytes "+r, func(t *testing.T) {
			actual, err := getActionDataFieldInBytes(content)
			assert.Nil(t, err)
			assert.IsType(t, actual, []byte{})
		})
	}
}

func TestTransmogrifyActionPolicyRevisionFromDataBytesExamples(t *testing.T) {
	dataBytes, err := getActionDataFieldInBytes(rawactions["policy_update"])
	assert.Nil(t, err)
	revisionID, err := policyRevisionFromDataBytes(dataBytes)
	assert.Nil(t, err)
	assert.Equal(t, revisionID, "e775abb4fa02004c5d0cbb7f73f082e09ebf77f1a613a6c2cbde646735b68a4c")
}

func TestTransmogrifyActionGetActionDataFieldInBytesSamples(t *testing.T) {
	cases := []struct {
		description string
		content     []byte
		expected    []byte
		throwError  bool
		errMessage  string
	}{
		{
			description: "Simple data field extraction as String",
			content: []byte(`{
        "id": "210d5d6a-5df5-4b48-8998-476e23269ce3",
        "message_type": "action",
	      "entity_name": "ename",
        "data": "{\"data\":\"info\"}"
	    }`),
			expected: []byte(`{"data":"info"}`),
		},
		{
			description: "Simple data field extraction as JSON object",
			content: []byte(`{
        "id": "11234567890",
        "message_type": "action",
	      "entity_name": "awesomeness",
        "data": {"data":"info"}
	    }`),
			expected: []byte(`{"data":"info"}`),
		},
		{
			description: "Complex data field extraction",
			content: []byte(`{
        "id": "not-really-an-id-but-it-doesnt-matter-here",
        "message_type": "action",
	      "entity_name": "awesomeness",
        "data": {
          "revision_id": "6c215da3266a20fd7a56ae9f1e3073e47c124f713a0e1eb74619a035325cd482",
          "name": "cd-infrastructure-base",
          "run_list": [
            "recipe[audit::default]"
          ],
          "cookbook_locks": {
            "insights-test": {
              "version": "0.1.38",
              "identifier": "9b2832d8802559fd9611ae1e9bc6ee20bf258a40",
              "dotted_decimal_identifier": "43672820235904345.71378171787647942.261824413272640",
              "cache_key": "chefops-base-0.1.38-ops-supermarket.chef.co",
              "origin": "https://ops-supermarket.chef.co:443/api/v1/cookbooks/chefops-base/versions/0.1.38/download",
              "source_options": {
                "artifactserver": "https://ops-supermarket.chef.co:443/api/v1/cookbooks/chefops-base/versions/0.1.38/download",
                "version": "0.1.38"
              }
            },
            "sensu": {
              "version": "4.0.6",
              "identifier": "8722f1798facc34dafaefd6bcbde3b8462b6a1b4",
              "dotted_decimal_identifier": "38037542377860291.21866739317591006.65439777857972",
              "cache_key": "sensu-4.0.6-supermarket.chef.io",
              "origin": "https://supermarket.chef.io:443/api/v1/cookbooks/sensu/versions/4.0.6/download",
              "source_options": {
                "artifactserver": "https://supermarket.chef.io:443/api/v1/cookbooks/sensu/versions/4.0.6/download",
                "version": "4.0.6"
              }
            },
          },
          "default_attributes": {},
          "override_attributes": {},
          "solution_dependencies": {}
        }
	    }`),
			expected: []byte(`{
          "revision_id": "6c215da3266a20fd7a56ae9f1e3073e47c124f713a0e1eb74619a035325cd482",
          "name": "cd-infrastructure-base",
          "run_list": [
            "recipe[audit::default]"
          ],
          "cookbook_locks": {
            "insights-test": {
              "version": "0.1.38",
              "identifier": "9b2832d8802559fd9611ae1e9bc6ee20bf258a40",
              "dotted_decimal_identifier": "43672820235904345.71378171787647942.261824413272640",
              "cache_key": "chefops-base-0.1.38-ops-supermarket.chef.co",
              "origin": "https://ops-supermarket.chef.co:443/api/v1/cookbooks/chefops-base/versions/0.1.38/download",
              "source_options": {
                "artifactserver": "https://ops-supermarket.chef.co:443/api/v1/cookbooks/chefops-base/versions/0.1.38/download",
                "version": "0.1.38"
              }
            },
            "sensu": {
              "version": "4.0.6",
              "identifier": "8722f1798facc34dafaefd6bcbde3b8462b6a1b4",
              "dotted_decimal_identifier": "38037542377860291.21866739317591006.65439777857972",
              "cache_key": "sensu-4.0.6-supermarket.chef.io",
              "origin": "https://supermarket.chef.io:443/api/v1/cookbooks/sensu/versions/4.0.6/download",
              "source_options": {
                "artifactserver": "https://supermarket.chef.io:443/api/v1/cookbooks/sensu/versions/4.0.6/download",
                "version": "4.0.6"
              }
            },
          },
          "default_attributes": {},
          "override_attributes": {},
          "solution_dependencies": {}
        }`),
		},
		{
			description: "Action without data field returns an error",
			content: []byte(`{
        "id": "abcdefghijklmnopqrstuvwxyz",
        "message_type": "action",
	      "entity_name": "coolz"
	    }`),
			expected:   nil,
			throwError: true,
			errMessage: "rpc error: code = Internal desc = Unable to retrieve 'data' field from action message: Key path not found",
		},
	}

	for _, test := range cases {
		t.Run(test.description, func(t *testing.T) {
			actual, err := getActionDataFieldInBytes(test.content)
			if test.throwError {
				assert.NotNil(t, err)
				assert.Equal(t, test.errMessage, err.Error())
			} else {
				assert.Nil(t, err)
			}
			assert.IsType(t, actual, []byte{})
			assert.Equal(t, actual, test.expected)
		})
	}
}
