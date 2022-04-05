+++
title = "Chef Sever Migration"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Chef Sever Migration"
    parent = "automate/infrastructure"
    identifier = "automate/infrastructure/chef-server-migration.md Chef Server Migration"
    weight = 40
+++

## Knife EC Backup

*knife-ec-backup* is the command to take backup of Chef Server/Backend during Chef Server Migration. *knife-ec-backup* can backup and restore the data in a Chef Infra Server installation and preserves the data in an intermediate and editable text mode. It is similar to the `knife download` and `knife upload` commands and uses the same underlying libraries, but also includes workarounds for objects not yet supported by those tools and various Infra Server API deficiencies.

### Prerequisites

* Chef Infra Client 11.8 and above.

### Installation

#### Chef Infra Server (Recommended)

This gem is installed with Chef Infra Server 12 and later and the sub-commands are available with embedded copy of `knife`. Refer to the command shown below:

```cmd
sudo /opt/opscode/bin/knife ec backup ~/chef-server-backup-directory
```

If you need a newer version of `knife-ec-backup` you can install it using the embedded `gem` command as shown below:

```cmd
/opt/opscode/embedded/bin/gem install knife-ec-backup --no-doc
```

#### Chef WorkStation Install (Unsupported)

On systems other than the Chef Infra Server, installation of the `gem` is not tested or supported. However, if you attempt to do so you will need the PostgreSQL libraries installed.

For example, on macOS:

```cmd
brew install libpq
gem install knife-ec-backup -- --with-pg-config=/usr/local/Cellar/libpq/9.2/bin/pg_config
```

The current location of `pg_config` can be determined with brew info `libpq`.

### Run the `knife-ec-backup` Command

The `knife-ec-backup` command can run remotely and locally. To run the command remotely, provide the PostgreSQL and expose the port to access the remote machine.

The above process won’t work for the **Manage EOL Migration** as you need the detailed database for it.

For example: `/opt/opscode/bin/knife ec backup --server-url https://ec2-18-117-112-129.us-east-2.compute.amazonaws.com ./backup/ --with-user-sql --with-key-sql -c /etc/opscode/pivotal.rb`

### Output of the Command

The command takes the backup of the CS in the allotted folder. The folder structure is: Here is the folder structure: `drwxr-xr-x 5 root root 4096 Dec 28 14:26`.

* drwxr-xr-x 6 ubuntu ubuntu 4096 Dec 28 15:54 ..
* -rw-r--r-- 1 root root 10642 Dec 28 14:26 key_dump.json
* -rw-r--r-- 1 root root 19423 Dec 28 14:26 key_table_dump.json
* drwxr-xr-x 10 root root 4096 Dec 28 14:26 organizations
* drwxr-xr-x 2 root root 4096 Dec 28 14:26 user_acls
* drwxr-xr-x 2 root root 4096 Dec 28 14:26 users

### Data Usage in EOL Migrations

In this section, we will discuss about the the usage of data in EOL migration. Automate strictly needs to comress and upload the directory to achieve the following steps:

* Get the list of all organizations.
    * Read through organizations folder.
    * Every folder is named for every organization and has information for the `org root@ec2-18-117-112-129:/home/ubuntu/backup# ls organizations/admin-org demoorg empty-org new_demoorg not-admin-org test-org test-org-new testo`
    * Read through `org.json` in each of the folder. It has details of the organization as shown below:
    
    ```json
    {
        "name": "demoorg",
        "full_name": "Edit Demo Org",
        "guid": "a7a8d58509e87644fe2d5d3cfe934149"
    }
    ```
    
    * Automate can add the organization details from the above mentioned files.

* Get the list and details of all the users in a server. You can get it from the `key_dump.json` file. The list of details are as follows:

    * Username
    *  Email
    *  First Name
    *  Last Name
    *  Middle Name
    *  Ldap User Name
    * Hashed Password

Refer to the code given below

```json
{
    "id": "00000000000018192ac9b81d31e2130f",
    "authz_id": "d2ace138fbb7fba13ee42fc4f87259db",
    "username": "kallol",
    "email": "kallol.roy@progress.com",
    "pubkey_version": 0,
    "public_key": "this_is_not_a_key",
    "serialized_object": "{\"display_name\":\"Kallol Roy\",\"first_name\":\"Kallol\",\"last_name\":\"Roy\",\"middle_name\":\"\"}",
    "last_updated_by": "d2ace138fbb7fba13ee42fc4f87259db",
    "created_at": "2021-06-12 04:57:21 +0000",
    "updated_at": "2021-07-15 09:49:04 +0000",
    "external_authentication_uid": null,
    "recovery_authentication_enabled": false,
    "admin": false,
    "hashed_password": "$2a$12$YKLkbaY5M5kwSbj7/riTRuinRvPDOsKFL4hlObH2dccjFEZO3gx8e",
    "salt": "$2a$12$YKLkbaY5M5kwSbj7/riTRu",
    "hash_type": "bcrypt"
}
```

* Get all the members of an organization by following the steps below:
* Getting list of users in an organization.
* Read through organizations folder **C**. Every folder is an organization.
* Read **members.json** in the code below:

```json
[{
    "user": {
    "username": "kallol"
    }

}, {
    "user": {
    "username": "new_user"
    }
}, {
    "user": {
    "username": "test-new-user"
    }
}]
```
The above two maps can be used as an organization in Automate.

* To get the admins of an organization, follow the steps below:
    * Read through organizations folder.
    * Every folder is an organization.
    * Read `groups/admins.json` as shown below:
    
```json
{
    "name": "admins",
    "users": [
        "kallol",
        "pivotal"
    ]
}
```
