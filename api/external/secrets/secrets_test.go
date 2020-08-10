package secrets

import (
	"testing"

	"github.com/chef/automate/api/external/common/query"
	"github.com/stretchr/testify/assert"
)

var exampleValidationFailures = []struct {
	in  Secret
	out string
}{
	{Secret{}, "Invalid secret, 'name' is a required parameter"},
	//ssh
	{Secret{
		Name: "name",
		Type: "ssh",
	}, "Invalid data content for secret type 'ssh'. A 'username' field is required"},
	{Secret{
		Name: "name",
		Type: "ssh",
		Data: appendKvs(&query.Kv{Key: "username", Value: ""}),
	}, "Invalid data content for secret type 'ssh'. A 'username' field is required"},
	{Secret{
		Name: "name",
		Type: "ssh",
	}, "Invalid data content for secret type 'ssh'. A 'username' field is required"},
	{Secret{
		Name: "name",
		Type: "ssh",
		Data: appendKvs(&query.Kv{Key: "username", Value: "name"}, &query.Kv{Key: "password", Value: ""}),
	}, "Invalid data content for secret type 'ssh'. A 'password' or 'key' field is required"},
	{Secret{
		Name: "name",
		Type: "ssh",
		Data: appendKvs(&query.Kv{Key: "username", Value: "name"}, &query.Kv{Key: "key", Value: ""}),
	}, "Invalid data content for secret type 'ssh'. A 'password' or 'key' field is required"},
	{Secret{
		Name: "name",
		Type: "ssh",
		Data: appendKvs(&query.Kv{Key: "username", Value: "name"}),
	}, "Invalid data content for secret type 'ssh'. A 'password' or 'key' field is required"},
	{Secret{
		Name: "name",
		Type: "ssh",
		Data: appendKvs(&query.Kv{Key: "username", Value: "name"}, &query.Kv{Key: "key", Value: "my_key"}, &query.Kv{Key: "password", Value: "my_password"}),
	}, "Invalid data content for secret type 'ssh'. 'password' and 'key' fields are mutually exclusive"},
	//winrm
	{Secret{
		Name: "name",
		Type: "winrm",
		Data: appendKvs(&query.Kv{Key: "username", Value: ""}),
	}, "Invalid data content for secret type 'winrm'. A 'username' field is required"},
	{Secret{
		Name: "name",
		Type: "winrm",
		Data: appendKvs(&query.Kv{Key: "username", Value: "name"}),
	}, "Invalid data content for secret type 'winrm'. A 'password' field is required"},
	{Secret{
		Name: "name",
		Type: "winrm",
		Data: appendKvs(&query.Kv{Key: "username", Value: "name"}, &query.Kv{Key: "password", Value: ""}),
	}, "Invalid data content for secret type 'winrm'. A 'password' field is required"},
	//sudo
	{Secret{
		Name: "name",
		Type: "sudo",
		Data: appendKvs(&query.Kv{Key: "password", Value: ""}, &query.Kv{Key: "options", Value: ""}),
	}, "Invalid data content for secret type 'sudo'. A 'password' or 'options' field is required"},
	{Secret{
		Name: "name",
		Type: "sudo",
	}, "Invalid data content for secret type 'sudo'. A 'password' or 'options' field is required"},
	//service now
	{Secret{
		Name: "name",
		Type: "service_now",
		Data: appendKvs(&query.Kv{Key: "username", Value: "name"})},
		"Invalid data content for secret type 'service_now'. A 'password' field is required"},
	{Secret{
		Name: "name",
		Type: "service_now",
		Data: appendKvs(&query.Kv{Key: "password", Value: "name"})},
		"Invalid data content for secret type 'service_now'. A 'username' field is required"},
	//aws
	{Secret{
		Name: "name",
		Type: "aws",
		Data: appendKvs(&query.Kv{Key: "ARN_ROLE", Value: ""}, &query.Kv{Key: "AWS_ACCESS_KEY_ID", Value: "ID"})},
		"Invalid data content for secret type 'aws'. AWS_SECRET_ACCESS_KEY not provided",
	},
	{Secret{
		Name: "name",
		Type: "aws",
		Data: appendKvs(&query.Kv{Key: "ARN_ROLE", Value: ""})},
		"Invalid data content for secret type 'aws'. AWS_ACCESS_KEY_ID not provided",
	},
	//azure
	{Secret{
		Name: "name",
		Type: "azure",
		Data: appendKvs(&query.Kv{Key: "AZURE_CLIENT_ID", Value: "AZURE_CLIENT_ID"}, &query.Kv{Key: "AZURE_CLIENT_SECRET", Value: "AZURE_CLIENT_SECRET"})},
		"Invalid data content for secret type 'azure'. AZURE_TENANT_ID not provided",
	},
	{Secret{
		Name: "name",
		Type: "azure",
		Data: appendKvs(&query.Kv{Key: "AZURE_CLIENT_ID", Value: "AZURE_CLIENT_ID"}, &query.Kv{Key: "AZURE_TENANT_ID", Value: "AZURE_TENANT_ID"})},
		"Invalid data content for secret type 'azure'. AZURE_CLIENT_SECRET not provided",
	},
	{Secret{
		Name: "name",
		Type: "azure",
		Data: appendKvs(&query.Kv{Key: "AZURE_CLIENT_SECRET", Value: "AZURE_CLIENT_SECRET"}, &query.Kv{Key: "AZURE_TENANT_ID", Value: "AZURE_TENANT_ID"})},
		"Invalid data content for secret type 'azure'. AZURE_CLIENT_ID not provided",
	},
	//chef-server
	{Secret{
		Name: "name",
		Type: "chef-server",
	}, "Invalid data content for secret type 'chef-server'. A 'key' field is required"},
}

var exampleValidationSuccesses = []struct {
	in Secret
}{
	{Secret{
		Name: "name",
		Type: "ssh",
		Data: appendKvs(&query.Kv{Key: "username", Value: "name"}, &query.Kv{Key: "password", Value: "password"})}},
	{Secret{
		Name: "name",
		Type: "ssh",
		Data: appendKvs(&query.Kv{Key: "username", Value: "key"}, &query.Kv{Key: "password", Value: "private_key"})}},
	//winrm
	{Secret{
		Name: "name",
		Type: "winrm",
		Data: appendKvs(&query.Kv{Key: "username", Value: "name"}, &query.Kv{Key: "password", Value: "password"})}},
	//sudo
	{Secret{
		Name: "name",
		Type: "sudo",
		Data: appendKvs(&query.Kv{Key: "password", Value: "pass"})}},
	{Secret{
		Name: "name",
		Type: "sudo",
		Data: appendKvs(&query.Kv{Key: "options", Value: "my opts"})}},
	//service now
	{Secret{
		Name: "name",
		Type: "service_now",
		Data: appendKvs(&query.Kv{Key: "username", Value: "name"}, &query.Kv{Key: "password", Value: "password"})}},
	//aws
	{Secret{
		Name: "name",
		Type: "aws",
		Data: appendKvs(&query.Kv{Key: "ARN_ROLE", Value: ""}, &query.Kv{Key: "AWS_ACCESS_KEY_ID", Value: "ID"}, &query.Kv{Key: "AWS_SECRET_ACCESS_KEY", Value: "key"})}},
	{Secret{
		Name: "name",
		Type: "aws",
		Data: appendKvs(&query.Kv{Key: "ARN_ROLE", Value: "arn role"})}},
	//azure
	{Secret{
		Name: "name",
		Type: "azure",
		Data: appendKvs(&query.Kv{Key: "AZURE_CLIENT_ID", Value: "AZURE_CLIENT_ID"},
			&query.Kv{Key: "AZURE_CLIENT_SECRET", Value: "AZURE_CLIENT_SECRET"},
			&query.Kv{Key: "AZURE_TENANT_ID", Value: "AZURE_TENANT_ID"})}},
	//chef-server
	{Secret{
		Name: "name",
		Type: "chef-server",
		Data: appendKvs(&query.Kv{Key: "key", Value: "--KEY--"})}},
}

func appendKvs(kvs ...*query.Kv) []*query.Kv {
	array := make([]*query.Kv, len(kvs))

	for i, kv := range kvs {
		array[i] = kv
	}

	return array
}

func TestValidateSecretFailures(t *testing.T) {
	for _, test := range exampleValidationFailures {
		err := test.in.Validate()
		if err == nil {
			t.Errorf("Sprintf(%q, validateSecret()) => nil, want %q", &test.in, test.out)
		} else if err.Error() != test.out {
			t.Errorf("Sprintf(%q, validateSecret()) => %q, want %q", &test.in, err.Error(), test.out)
		}
	}
}

func TestValidateSecretPasses(t *testing.T) {
	for _, test := range exampleValidationSuccesses {
		err := test.in.Validate()
		if err != nil {
			t.Errorf("Sprintf(%q, validateSecret()) => %q, want nil", &test.in, err.Error())
		}
	}
}

func TestMergeReplacesName(t *testing.T) {
	existing := Secret{
		Name: "Name",
		Type: "ssh",
		Tags: appendKvs(&query.Kv{Key: "test tag", Value: "tag value"}),
		Data: appendKvs(&query.Kv{Key: "username", Value: "user"}, &query.Kv{Key: "password", Value: "pass"}),
	}
	expected := existing
	expected.Name = "New Name"

	existing.Merge(&Secret{Name: "New Name"})

	assert.ElementsMatch(t, expected.Data, existing.Data)
	assert.ElementsMatch(t, expected.Tags, existing.Tags)
	assert.Equal(t, expected.Name, existing.Name)
	assert.Equal(t, expected.Type, existing.Type)
}

func TestMergeReplacesType(t *testing.T) {
	existing := Secret{
		Name: "Name",
		Type: "ssh",
		Tags: appendKvs(&query.Kv{Key: "test tag", Value: "tag value"}),
		Data: appendKvs(&query.Kv{Key: "username", Value: "user"}, &query.Kv{Key: "password", Value: "pass"}),
	}
	expected := existing
	expected.Type = "winrm"

	existing.Merge(&Secret{Type: "winrm"})

	assert.ElementsMatch(t, expected.Data, existing.Data)
	assert.ElementsMatch(t, expected.Tags, existing.Tags)
	assert.Equal(t, expected.Name, existing.Name)
	assert.Equal(t, expected.Type, existing.Type)
}

func TestMergeReplacesTags(t *testing.T) {
	existing := Secret{
		Name: "Name",
		Type: "ssh",
		Tags: appendKvs(&query.Kv{Key: "test tag", Value: "tag value"}),
		Data: appendKvs(&query.Kv{Key: "username", Value: "user"}, &query.Kv{Key: "password", Value: "pass"}),
	}
	expected := existing
	expected.Tags = appendKvs(&query.Kv{Key: "new tag", Value: "new tag value"})

	existing.Merge(&Secret{Tags: appendKvs(&query.Kv{Key: "new tag", Value: "new tag value"})})

	assert.ElementsMatch(t, expected.Data, existing.Data)
	assert.ElementsMatch(t, expected.Tags, existing.Tags)
	assert.Equal(t, expected.Name, existing.Name)
	assert.Equal(t, expected.Type, existing.Type)
}

func TestMergeReplacesUsernameInData(t *testing.T) {
	existing := Secret{
		Name: "Name",
		Type: "ssh",
		Tags: appendKvs(&query.Kv{Key: "test tag", Value: "tag value"}),
		Data: appendKvs(&query.Kv{Key: "username", Value: "user"}, &query.Kv{Key: "password", Value: "pass"}),
	}
	expected := existing
	expected.Data = appendKvs(&query.Kv{Key: "username", Value: "new user"}, &query.Kv{Key: "password", Value: "pass"})

	existing.Merge(&Secret{Data: appendKvs(&query.Kv{Key: "username", Value: "new user"})})

	assert.ElementsMatch(t, expected.Data, existing.Data)
	assert.ElementsMatch(t, expected.Tags, existing.Tags)
	assert.Equal(t, expected.Name, existing.Name)
	assert.Equal(t, expected.Type, existing.Type)
}

func TestMergeAddsUsernameInData(t *testing.T) {
	existing := Secret{
		Name: "Name",
		Type: "ssh",
		Tags: appendKvs(&query.Kv{Key: "test tag", Value: "tag value"}),
		Data: appendKvs(&query.Kv{Key: "password", Value: "pass"}),
	}
	expected := existing
	expected.Data = appendKvs(&query.Kv{Key: "username", Value: "new user"}, &query.Kv{Key: "password", Value: "pass"})

	existing.Merge(&Secret{Data: appendKvs(&query.Kv{Key: "username", Value: "new user"})})

	assert.ElementsMatch(t, expected.Data, existing.Data)
	assert.ElementsMatch(t, expected.Tags, existing.Tags)
	assert.Equal(t, expected.Name, existing.Name)
	assert.Equal(t, expected.Type, existing.Type)
}

func TestMergeReplacesPasswordInData(t *testing.T) {
	existing := Secret{
		Name: "Name",
		Type: "ssh",
		Tags: appendKvs(&query.Kv{Key: "test tag", Value: "tag value"}),
		Data: appendKvs(&query.Kv{Key: "username", Value: "user"}, &query.Kv{Key: "password", Value: "pass"}),
	}
	expected := existing
	expected.Data = appendKvs(&query.Kv{Key: "password", Value: "new password"}, &query.Kv{Key: "username", Value: "user"})

	existing.Merge(&Secret{Data: appendKvs(&query.Kv{Key: "password", Value: "new password"})})

	assert.ElementsMatch(t, expected.Data, existing.Data)
	assert.ElementsMatch(t, expected.Tags, existing.Tags)
	assert.Equal(t, expected.Name, existing.Name)
	assert.Equal(t, expected.Type, existing.Type)
}

func TestMergeAddsPasswordInData(t *testing.T) {
	existing := Secret{
		Name: "Name",
		Type: "ssh",
		Tags: appendKvs(&query.Kv{Key: "test tag", Value: "tag value"}),
		Data: appendKvs(&query.Kv{Key: "username", Value: "user"}),
	}
	expected := existing
	expected.Data = appendKvs(&query.Kv{Key: "password", Value: "new password"}, &query.Kv{Key: "username", Value: "user"})

	existing.Merge(&Secret{Data: appendKvs(&query.Kv{Key: "password", Value: "new password"})})

	assert.ElementsMatch(t, expected.Data, existing.Data)
	assert.ElementsMatch(t, expected.Tags, existing.Tags)
	assert.Equal(t, expected.Name, existing.Name)
	assert.Equal(t, expected.Type, existing.Type)
}

func TestMergeReplacesKeyInData(t *testing.T) {
	existing := Secret{
		Name: "Name",
		Type: "ssh",
		Tags: appendKvs(&query.Kv{Key: "test tag", Value: "tag value"}),
		Data: appendKvs(&query.Kv{Key: "username", Value: "user"}, &query.Kv{Key: "key", Value: "old key"}),
	}
	expected := existing
	expected.Data = appendKvs(&query.Kv{Key: "key", Value: "new key"}, &query.Kv{Key: "username", Value: "user"})

	existing.Merge(&Secret{Data: appendKvs(&query.Kv{Key: "key", Value: "new key"})})

	assert.ElementsMatch(t, expected.Data, existing.Data)
	assert.ElementsMatch(t, expected.Tags, existing.Tags)
	assert.Equal(t, expected.Name, existing.Name)
	assert.Equal(t, expected.Type, existing.Type)
}

func TestMergeAddsKeyInData(t *testing.T) {
	existing := Secret{
		Name: "Name",
		Type: "ssh",
		Tags: appendKvs(&query.Kv{Key: "test tag", Value: "tag value"}),
		Data: appendKvs(&query.Kv{Key: "username", Value: "user"}),
	}
	expected := existing
	expected.Data = appendKvs(&query.Kv{Key: "key", Value: "new key"}, &query.Kv{Key: "username", Value: "user"})

	existing.Merge(&Secret{Data: appendKvs(&query.Kv{Key: "key", Value: "new key"})})

	assert.ElementsMatch(t, expected.Data, existing.Data)
	assert.ElementsMatch(t, expected.Tags, existing.Tags)
	assert.Equal(t, expected.Name, existing.Name)
	assert.Equal(t, expected.Type, existing.Type)
}

func TestMergeReplacesSudoOptionsInData(t *testing.T) {
	existing := Secret{
		Name: "Name",
		Type: "sudo",
		Tags: appendKvs(&query.Kv{Key: "test tag", Value: "tag value"}),
		Data: appendKvs(&query.Kv{Key: "username", Value: "user"}, &query.Kv{Key: "options", Value: "opts"}),
	}
	expected := existing
	expected.Data = appendKvs(&query.Kv{Key: "username", Value: "user"}, &query.Kv{Key: "options", Value: "my new opts"})

	existing.Merge(&Secret{Data: appendKvs(&query.Kv{Key: "options", Value: "my new opts"})})

	assert.ElementsMatch(t, expected.Data, existing.Data)
	assert.ElementsMatch(t, expected.Tags, existing.Tags)
	assert.Equal(t, expected.Name, existing.Name)
	assert.Equal(t, expected.Type, existing.Type)
}

func TestMergeAddsSudoOptionsInData(t *testing.T) {
	existing := Secret{
		Name: "Name",
		Type: "sudo",
		Tags: appendKvs(&query.Kv{Key: "test tag", Value: "tag value"}),
		Data: appendKvs(&query.Kv{Key: "username", Value: "user"}),
	}
	expected := existing
	expected.Data = appendKvs(&query.Kv{Key: "username", Value: "user"}, &query.Kv{Key: "options", Value: "my new opts"})

	existing.Merge(&Secret{Data: appendKvs(&query.Kv{Key: "options", Value: "my new opts"})})

	assert.ElementsMatch(t, expected.Data, existing.Data)
	assert.ElementsMatch(t, expected.Tags, existing.Tags)
	assert.Equal(t, expected.Name, existing.Name)
	assert.Equal(t, expected.Type, existing.Type)
}

func TestMergeIgnoresOptionsInData(t *testing.T) {
	existing := Secret{
		Name: "Name",
		Type: "ssh",
		Tags: appendKvs(&query.Kv{Key: "test tag", Value: "tag value"}),
		Data: appendKvs(&query.Kv{Key: "username", Value: "user"}),
	}
	expected := existing

	existing.Merge(&Secret{Data: appendKvs(&query.Kv{Key: "options", Value: "my new opts"})})

	assert.ElementsMatch(t, expected.Data, existing.Data)
	assert.ElementsMatch(t, expected.Tags, existing.Tags)
	assert.Equal(t, expected.Name, existing.Name)
	assert.Equal(t, expected.Type, existing.Type)
}

func TestMergeReplacesKeyWithPasswordInData(t *testing.T) {
	existing := Secret{
		Name: "Name",
		Type: "ssh",
		Tags: appendKvs(&query.Kv{Key: "test tag", Value: "tag value"}),
		Data: appendKvs(&query.Kv{Key: "username", Value: "user"}, &query.Kv{Key: "key", Value: "my_key"}),
	}
	expected := existing
	expected.Data = appendKvs(&query.Kv{Key: "password", Value: "my_password"}, &query.Kv{Key: "username", Value: "user"})

	existing.Merge(&Secret{Data: appendKvs(&query.Kv{Key: "password", Value: "my_password"})})

	assert.ElementsMatch(t, expected.Data, existing.Data)
	assert.ElementsMatch(t, expected.Tags, existing.Tags)
	assert.Equal(t, expected.Name, existing.Name)
	assert.Equal(t, expected.Type, existing.Type)
}

func TestMergeReplacesPasswordWithKeyInData(t *testing.T) {
	existing := Secret{
		Name: "Name",
		Type: "ssh",
		Tags: appendKvs(&query.Kv{Key: "test tag", Value: "tag value"}),
		Data: appendKvs(&query.Kv{Key: "username", Value: "user"}, &query.Kv{Key: "password", Value: "my_password"}),
	}
	expected := existing
	expected.Data = appendKvs(&query.Kv{Key: "key", Value: "my_key"}, &query.Kv{Key: "username", Value: "user"})

	existing.Merge(&Secret{Data: appendKvs(&query.Kv{Key: "key", Value: "my_key"})})

	assert.ElementsMatch(t, expected.Data, existing.Data)
	assert.ElementsMatch(t, expected.Tags, existing.Tags)
	assert.Equal(t, expected.Name, existing.Name)
	assert.Equal(t, expected.Type, existing.Type)
}
