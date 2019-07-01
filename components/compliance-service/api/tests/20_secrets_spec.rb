require 'api/external/secrets/secrets_services_pb'

describe File.basename(__FILE__) do
  Secrets = Chef::Automate::Api::Secrets unless defined?(Secrets)

  def secrets ; Secrets::SecretsService ; end

  def cleanup
    return if ENV['NO_DELETE']

    SS_GRPC(secrets, :list, Secrets::Query.new())['secrets'].each do |s|
      SS_GRPC secrets, :delete, Secrets::Id.new(id: s.id)
    end
  end

  before(:all) { cleanup }
  after(:all) { cleanup }

  it "works" do
    ##### GRPC Failure tests #####
    assert_grpc_error("Not found for id: missing", 5) do
      SS_GRPC secrets, :read, Secrets::Id.new(id: 'missing')
    end

    assert_grpc_error("Invalid data content for secret type \'ssh\'. A \'username\' field is required", 3) do
      SS_GRPC secrets, :create, Secrets::Secret.new(
        data: [
          Secrets::Kv.new(key: "user", value: "bobby"),
          Secrets::Kv.new(key: "password", value: "x")
        ],
        name: "zoe",
        tags: [],
        type: "ssh"
      )
    end

    assert_grpc_error("Invalid data content for secret type 'aws'. AWS_ACCESS_KEY_ID not provided", 3) do
      SS_GRPC secrets, :create, Secrets::Secret.new(
        data: [
          Secrets::Kv.new(
              key: "AWS_ACCESS_ID",
              value: "123"
          ),
          Secrets::Kv.new(
              key: "AWS_SECRET_ACCESS_KEY",
              value: "123"
          )
        ],
        name: "aws cred",
        tags: [],
        type: "aws"
      )
    end

    assert_grpc_error("Invalid data content for secret type 'azure'. AZURE_CLIENT_SECRET not provided", 3) do
      SS_GRPC secrets, :create, Secrets::Secret.new(
        data: [
          Secrets::Kv.new(
              key: "AZURE_CLIENT_ID",
              value: "123"
          ),
          Secrets::Kv.new(
              key: "AZURE_CLIENT",
              value: "123"
          ),
          Secrets::Kv.new(
            key: "TENANT_ID",
            value: "123"
          )
        ],
        name: "azure cred",
        tags: [],
        type: "azure"
      )
    end

    assert_grpc_error("Invalid data content for secret type \'ssh\'. A \'password\' or \'key\' field is required", 3) do
      SS_GRPC secrets, :create, Secrets::Secret.new(
        data: [
          Secrets::Kv.new(key: "username", value: "bobby"),
          Secrets::Kv.new(key: "assword", value: "x")
        ],
        name: "bob",
        tags: [],
        type: "ssh"
      )
    end

    assert_grpc_error("Invalid data content for secret type \'winrm\'. A \'password\' field is required", 3) do
      SS_GRPC secrets, :create, Secrets::Secret.new(
        data: [
          Secrets::Kv.new(key: "username", value: "bobby"),
          Secrets::Kv.new(key: "assword", value: "x")
        ],
        name: "bob",
        tags: [],
        type: "winrm"
      )
    end

    assert_grpc_error("Invalid data content for secret type \'sudo\'. A \'password\' or \'options\' field is required", 3) do
      SS_GRPC secrets, :create, Secrets::Secret.new(
        data: [
          Secrets::Kv.new(key: "ok", value: "doky")
        ],
        name: "dok",
        tags: [],
        type: "sudo"
      )
    end

    assert_grpc_error("Invalid secret, \'name\' is a required parameter", 3) do
      SS_GRPC secrets, :create, Secrets::Secret.new(
        data: [
          Secrets::Kv.new(key: "ok", value: "doky")
        ],
        name: "",
        tags: [],
        type: "bla"
      )
    end

    assert_grpc_error("Invalid sort field, valid ones are: [last_modified name type]", 3) do
      SS_GRPC secrets, :list, Secrets::Query.new(sort: "wrong")
    end

    ##### GRPC Success tests #####
    actual = SS_GRPC secrets, :list, Secrets::Query.new()
    assert_equal Secrets::Secrets.new(secrets: [], total: 0), actual

    # Add a secret without tags or type
    secret1 = SS_GRPC secrets, :create, Secrets::Secret.new(
      data: [
        Secrets::Kv.new(key: "username", value: "bobby"),
        Secrets::Kv.new(key: "password", value: "b055y-th3_w1nn3r")
      ],
      name: "betasec",
      tags: [],
      type: ""
    )
    secret_id1 = secret1['id']
    assert_uuid(secret_id1)

    # need this to ensure that the created credentials have different timestamp
    sleep 1

    secret2 = SS_GRPC secrets, :create, Secrets::Secret.new(
      data: [
        Secrets::Kv.new(key: "username", value: "myuser"),
        Secrets::Kv.new(key: "password", value: "sup3rs3cr3t")
      ],
      name: "Alphasec",
      tags: [
        Secrets::Kv.new(key: "department", value: "marketing"),
        Secrets::Kv.new(key: "reason", value: "How do I know?")
      ],
      type: "ssh"
    )
    secret_id2 = secret2['id']
    assert_equal(36, secret_id2.size)

    # need this to ensure that the created credentials have different timestamp
    sleep 1

    secret3 = SS_GRPC secrets, :create, Secrets::Secret.new(
      data: [
        Secrets::Kv.new(key: "password", value: "qwerty")
      ],
      name: "SUDO rocks",
      tags: [],
      type: "sudo"
    )
    secret_id3 = secret3['id']
    assert_equal(36, secret_id3.size)

    # need this to ensure that the created credentials have different timestamp
    sleep 1

    secret4 = SS_GRPC secrets, :create, Secrets::Secret.new(
      data: [
        Secrets::Kv.new(key: "username", value: "administratore"),
        Secrets::Kv.new(key: "password", value: "qwerty")
      ],
      name: "Windows $tuff",
      tags: [],
      type: "winrm"
    )
    secret_id4 = secret4['id']
    assert_equal(36, secret_id4.size)

    # Get all secrets
    actual_secrets = SS_GRPC secrets, :list, Secrets::Query.new()
    assert_equal(Google::Protobuf::Timestamp, actual_secrets['secrets'][0]['last_modified'].class)
    actual_secrets['secrets'].each { |s|
      s['last_modified'] = nil
    }

    expected_secrets = {
      "secrets": [
        {
          "id": secret_id2,
          "name": "Alphasec",
          "type": "ssh",
          "tags": [
            {
              "key": "department",
              "value": "marketing"
            },
            {
              "key": "reason",
              "value": "How do I know?"
            }
          ]
        },
        {
          "id": secret_id1,
          "name": "betasec"
        },
        {
          "id": secret_id3,
          "name": "SUDO rocks",
          "type": "sudo"
        },
        {
          "id": secret_id4,
          "name": "Windows $tuff",
          "type": "winrm"
        }
      ],
      "total": 4
    }
    assert_equal_json_sorted(expected_secrets.to_json, actual_secrets.to_json)
    # Test the total count
    assert_equal(4, actual_secrets['total'])

    # Testing the default sorting(name ASC)
    assert_equal( ["Alphasec", "betasec", "SUDO rocks", "Windows $tuff"],
                  extract_grpc_field(actual_secrets['secrets'], 'name'))

    # Get all secrets sorted DESC by last_modified
    query_params = {"sort": "last_modified", "order": "DESC"}
    last_modified_sorted_secrets_json = SS_GRPC secrets, :list, Secrets::Query.new(
      sort: "last_modified",
      order: 1
    )
    assert_equal( ["Windows $tuff", "SUDO rocks", "Alphasec", "betasec"],
                  extract_grpc_field(last_modified_sorted_secrets_json['secrets'], 'name'))

    # Get all secrets sorted DESC by last_modified, first three on page 1
    res = SS_GRPC secrets, :list, Secrets::Query.new(
      sort: "last_modified",
      order: 1,
      page: 1,
      per_page: 3
    )
    assert_equal( ["Windows $tuff", "SUDO rocks", "Alphasec"],
                  extract_grpc_field(res['secrets'], 'name'))

    # Test the total count
    assert_equal(4, res['total'])

    # Get all secrets sorted DESC by last_modified, first three(or how many are left) on page 2
    res = SS_GRPC secrets, :list, Secrets::Query.new(
      sort: "last_modified",
      order: 1,
      page: 2,
      per_page: 3
    )
    page_2_secrets_json = res
    assert_equal(["betasec"], extract_grpc_field(page_2_secrets_json['secrets'], 'name'))
    # Test the total count
    assert_equal(4, page_2_secrets_json['total'])

    # Get secret by id with all details
    secret1_json = SS_GRPC secrets, :read, Secrets::Id.new(id: secret_id1)
    assert_equal(Google::Protobuf::Timestamp, secret1_json['last_modified'].class)
    if array_contains_elem(secret1_json['data'], Secrets::Kv.new(key: "password", value: "b055y-th3_w1nn3r")) &&
      array_contains_elem(secret1_json['data'], Secrets::Kv.new(key: "username", value: "bobby"))
      data = true
    end
    assert_equal(true, data)
    assert_equal(secret_id1, secret1_json['id'])
    assert_equal('betasec', secret1_json['name'])
    assert_equal('', secret1_json['type'])

    # Get secret by id with all details
    secret2_json = SS_GRPC secrets, :read, Secrets::Id.new(id: secret_id2)
    assert_equal(Google::Protobuf::Timestamp, secret2_json['last_modified'].class)
    if array_contains_elem(secret2_json['data'], Secrets::Kv.new(key: "password", value: "sup3rs3cr3t")) &&
      array_contains_elem(secret2_json['data'], Secrets::Kv.new(key: "username", value: "myuser")) &&
      array_contains_elem(secret2_json['tags'], Secrets::Kv.new(key: "department", value: "marketing")) &&
      array_contains_elem(secret2_json['tags'], Secrets::Kv.new(key: "reason", value: "How do I know?"))
      data = true
      tags = true
    end
    assert_equal(true, data)
    assert_equal(true, tags)
    assert_equal(secret_id2, secret2_json['id'])
    assert_equal('Alphasec', secret2_json['name'])
    assert_equal('ssh', secret2_json['type'])

    # Add a secret
    secret1 = SS_GRPC secrets, :create, Secrets::Secret.new(
      data: [
        Secrets::Kv.new(key: "username", value: "updatable-username"),
        Secrets::Kv.new(key: "password", value: "updatable-password")
      ],
      name: "updatable-sec",
      tags: [
        Secrets::Kv.new(key: "updatable-tag", value: "before-update"),
        Secrets::Kv.new(key: "non-updatable-tag", value: "should-stay-the-same")
      ],
      type: "ssh"
    )
    secret_id = secret1['id']

    SS_GRPC secrets, :update, Secrets::Secret.new(
      id: secret_id,
      data: [
        Secrets::Kv.new(key: "username", value: "new-username")
      ],
      name: "updatable-sec",
      tags: [
        Secrets::Kv.new(key: "updatable-tag", value: "after-update"),
        Secrets::Kv.new(key: "non-updatable-tag", value: "should-stay-the-same")
      ],
      type: "ssh"
    )

    # Get secret by id with all details to compare
    secret2 = SS_GRPC secrets, :read, Secrets::Id.new(id: secret_id)
    assert_equal(Google::Protobuf::Timestamp, secret2['last_modified'].class)
    if array_contains_elem(secret2['data'], Secrets::Kv.new(key: "password", value: "updatable-password")) &&
      array_contains_elem(secret2['data'], Secrets::Kv.new(key: "username", value: "new-username")) &&
      array_contains_elem(secret2['tags'], Secrets::Kv.new(key: "updatable-tag", value: "after-update")) &&
      array_contains_elem(secret2['tags'], Secrets::Kv.new(key: "non-updatable-tag", value: "should-stay-the-same"))
      data = true
      tags = true
    end
    assert_equal(true, data)
    assert_equal(true, tags)
    assert_equal(secret_id, secret2['id'])
    assert_equal('updatable-sec', secret2['name'])
    assert_equal('ssh', secret2['type'])

    SS_GRPC secrets, :update, Secrets::Secret.new(
      id: secret_id,
      data: [
        Secrets::Kv.new(key: "password", value: "new-password")
      ],
      name: "newname",
      tags: [
        Secrets::Kv.new(key: "updatable-tag", value: "after-update"),
        Secrets::Kv.new(key: "a new tag", value: "should-stay-the-same")
      ],
      type: "winrm"
    )

    # Get secret by id with all details to compare
    secret3 = SS_GRPC secrets, :read, Secrets::Id.new(id: secret_id)
    assert_equal(Google::Protobuf::Timestamp, secret3['last_modified'].class)
    if array_contains_elem(secret3['data'], Secrets::Kv.new(key: "password", value: "new-password")) &&
      array_contains_elem(secret3['data'], Secrets::Kv.new(key: "username", value: "new-username")) &&
      array_contains_elem(secret3['tags'], Secrets::Kv.new(key: "updatable-tag", value: "after-update")) &&
      array_contains_elem(secret3['tags'], Secrets::Kv.new(key: "a new tag", value: "should-stay-the-same"))
      data = true
      tags = true
    end
    assert_equal(true, data)
    assert_equal(true, tags)
    assert_equal(secret_id, secret3['id'])
    assert_equal('newname', secret3['name'])
    assert_equal('winrm', secret3['type'])

    SS_GRPC secrets, :update, Secrets::Secret.new(
      id: secret_id,
      name: "super-new-name",
      data: [
        Secrets::Kv.new(key: "username", value: "super-new-username"),
        Secrets::Kv.new(key: "password", value: "super-updatable-password")
      ],
      type: "winrm"
    )

    # Get secret by id with all details to compare
    secret4 = SS_GRPC secrets, :read, Secrets::Id.new(id: secret_id)
    assert_equal(Google::Protobuf::Timestamp, secret4['last_modified'].class)
    if array_contains_elem(secret4['data'], Secrets::Kv.new(key: "password", value: "super-updatable-password")) &&
      array_contains_elem(secret4['data'], Secrets::Kv.new(key: "username", value: "super-new-username"))
      data = true
    end
    assert_equal(true, data)
    assert_equal(secret_id, secret4['id'])
    assert_equal('super-new-name', secret4['name'])
    assert_equal('winrm', secret4['type'])

    # Get all secrets, filtered
    res = SS_GRPC secrets, :list, Secrets::Query.new(filters:[Secrets::Filter.new(key: "type", values: ["ssh"])])
    assert_equal(1, res.total)

    # Add another secret
    SS_GRPC secrets, :create, Secrets::Secret.new(
      data: [
        Secrets::Kv.new(key: "username", value: "updatable-username"),
        Secrets::Kv.new(key: "password", value: "updatable-password")
      ],
      name: "updatable-sec",
      tags: [],
      type: "ssh"
    )

    # Get filtered secrets again, expect 2
    res = SS_GRPC secrets, :list, Secrets::Query.new(filters:[Secrets::Filter.new(key: "type", values: ["ssh"])])
    assert_equal(2, res.total)

    # Test filtering with a different type
    res = SS_GRPC secrets, :list, Secrets::Query.new(filters:[Secrets::Filter.new(key: "type", values: ["winrm"])])
    assert_equal(2, res.total)
  end
end
