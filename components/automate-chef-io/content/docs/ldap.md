+++
title = "LDAP Integration"
description = "LDAP configuration details"
date = 2018-05-11T09:27:09+00:00
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "configuring_automate"
    weight = 30
+++

## Overview

This is documentation for using Chef Automate's LDAP and Microsoft Active Directory integrations.
First, we detail how the login process works when logging in via LDAP.  Then,
we describe common directory layouts, and what Chef Automate config would be
used for them. At the end, you will find a troubleshooting section collecting
common cases.

### Login with LDAP

Once the user has provided a username and password at the login screen, Chef
Automate goes through a sequence of operations to complete the login: Connect,
Bind, User Search, Login Bind, and Group Search.

#### Authorization with LDAP

Chef Automate supports defining permissions for [LDAP users and their groups]({{< ref "authorization-overview.md#subjects" >}}).

#### Connect

Chef Automate first needs to establish a TCP connection to your LDAP service,
secured by TLS.  It will connect to the host configured in your TOML config, for
example

```toml
host = "ldap.corp.com"
```

Automate uses port `636` by default.  To override the port, append it to the
host setting, e.g.

```toml
host = "ldap.corp.com:10636"
```

Whether the validity of the server's TLS certificate will be enforced depends on
the TLS setup: if you provide a certificate authority's (CA) certificate(s),
Chef Automate will only communicate with the LDAP service if the certificate
provided by the host can be validated using the CA certificate(s).

The configuration line would look like this:

```toml
ca_contents = """-----BEGIN CERTIFICATE-----
MIICsDCCAhmgAwIBAgIJAJxMopMJbhPkMA0GCSqGSIb
...
X0uRzUPlpttd5tYFs43nkqxJT6s=
-----END CERTIFICATE-----"""
```

Note: you can provide multiple CA certs, and they have to be PEM encoded.

{{% warning %}}
Connecting to an LDAP service without TLS is not recommended.
{{% /warning %}}

However, if you wish to integrate with an LDAP server with TLS disabled:

```toml
insecure_no_ssl = true
```

#### Bind

Chef Automate then authenticates with (or "binds to") the LDAP service using
_bind credentials_.  In your config TOML file, these would be (for example):

```toml
bind_dn = "cn=service_account,dc=corp,dc=com"
bind_password = "i<3ldap"
```

If your LDAP server supports _anonymous bind_, and you want to use that, unset
both bind DN and password:

```toml
bind_dn = ""
bind_password = ""
```

Wrap special characters in a bind_password in triple single quotes.

```toml
bind_password = '''$p3c"i'@l ! %#'''
```

#### User Search

After binding successfully, Chef Automate will try to obtain the directory name
of the user that is trying to log in.

To do so, it will search, using the configured _base_ `base_user_search_dn`,
for an entry such that `username_attr` equals the username that attempted to
login.

If configured, it will retrieve additional attributes, using the configured
names (`user_id_attr`, `email_attr`, and `user_display_name_attr`). See
[Configuration: LDAP]({{< relref "configuration.md#ldap" >}}) for an overview.

{{% warning %}}
If the LDAP search fails to retrieve the configured attributes, the login process will fail.
{{% /warning %}}

##### Filtering Which Users Can Log In

You can further restrict the user search by providing a valid LDAP filter to `user_query_filter`.
For example,

```toml
user_query_filter = "(objectClass=person)"
```

which will be concatenated with the search filter constructed from the provided
username in the login screen. The contents of `user_query_filter` gets expanded
to `(&<user_query_filter_value>)` so you can pass in multiple filters.

For example, if you wanted to only allow people that were members of a specific Active Directory
group to log into Chef Automate, you could define a `user_query_filter` with multiple filters like:

```toml
user_query_filter = "(objectClass=person)(memberof=CN=YourGroupToFilterOn,OU=Users,DC=YourDomain,DC=com)"
```

This filter says "only allow people who are members of YourGroupToFilterOn to log into Chef Automate".
When a user tries to log in, they would only be authorized if they were found after the filter is applied:

```LDIF
(&(objectClass=person)(memberof=CN=YourGroupToFilterOn,OU=Users,DC=YourDomain,DC=com))
```

You can test out your filter by using `ldapsearch` to see if the users you expect are being returned:

```shell
ldapsearch -H ldap://ldap-server:636/ \  # host
  -D cn=service_account,dc=corp,dc=com \ # bind_dn
  -w admin \                             # bind_password
  -b ou=People,dc=corp,dc=com \          # base_user_search_dn
  -s sub \
  '(&(objectClass=person)(memberof=CN=YourGroupToFilterOn,OU=Users,DC=YourDomain,DC=com))'
```

See [Troubleshoot LDAP Search Queries]({{< relref "ldap.md#troubleshoot-ldap-search-queries" >}}) for more info on debugging
your LDAP integration via `ldapsearch`.

#### Login Bind

When the search for a user directory entry has succeeded, the LDAP connector
will attempt to bind as the user entry, using the supplied password.

For example, if the login using `jane:janespassword` has resulted in a
successful user search, returning `cn=jane,ou=People,dc=corp,dc=com`, the next
step will be to _bind again_ using that DN, and the password `janespassword`.

#### Group Search

Finally, after the user has been authenticated, their internal record is
enriched with LDAP-provided groups. This happens by executing another search
using the same bind DN and password that was used for user search.

Similar to user search, a base DN has to be provided; and the result can be
restricted by providing an additional filter:

 ```toml
base_group_search_dn = "ou=Groups,dc=corp,dc=com"
group_query_filter = "(objectClass=group)"
```

The correct configuration settings again depend on your directory server's schema;
see the example configs below.

{{% warning %}}
The `base_group_search_dn` setting is optional. However, if it's not provided,
users authenticating via LDAP (or MSAD) will not be members of any teams.
{{% /warning %}}

#### Configuration Overview

See below for the full config and additional details about all LDAP configuration options.

```toml
[dex.v1.sys.connectors.ldap]
  ###
   # Configuration for querying your LDAP server
   ###
   ca_contents = "<your ca contents>"
   host = "<your host>"

   # The DN and password you wish to bind to your LDAP server to search for
   # users to authenticate for Chef Automate (and also to search for their group membership).
   # Example: "uid=seviceaccount,cn=users,dc=example,dc=com"
   bind_dn = "<your bind_dn>"
   bind_password = "<your bind_password>"

   ###
   # User Query (search for LDAP users to authenticate for Chef Automate)
   ###
   # The base DN to start the user query.
   # Chef Automate will use this as the base DN on which to search for users to authenticate against your LDAP server.
   # Example: "cn=users,dc=example,dc=com"
   base_user_search_dn = "<your base user search DN>"

   # The LDAP field used to filter the query for users to authenticate for Chef Automate.
   # Example: Setting this to "uid" would result in a filter of "(uid=<username_for_user_trying_to_authenticate>)".
   username_attr = "<your username attribute>"

   # Optional: LDAP query filter to apply when searching for users to authenticate.
   # This will be combined with username_attr filter above.
   # Example: Setting this to "(objectClass=person)" will filter on human actors only.
   user_query_filter = "<your user query filter>"

   ###
   # Populating the Chef Automate User via LDAP
   ###
   # Determines which LDAP field populates the username in a user's Chef Automate session on successful authentication.
   user_id_attr = "<your userid attribute>"

   # Optional: determines which LDAP field populates the email in a user's Chef Automate session on successful authentication.
   # Defaults to "user_id_attr" if not specified.
   email_attr = "<your email attribute>"

   # Optional: determines which LDAP field populates the display name in a user's Chef Automate session on successful authentication.
   # Defaults to "name" if not specified.
   user_display_name_attr = "<your user display name attribute>"

   ###
   # Group Query (search for LDAP group membership for an authenticated user)
   ###
   # The base DN to start the group membership query.
   # Chef Automate will use this as the base DN on which to search for LDAP group membership for a specific LDAP user.
   # Example: "cn=groups,dc=freeipa,dc=example,dc=com"
   base_group_search_dn = "<your base group search DN>"

   # The following two fields are used to match a user to a group.
   # If the defaults are used, then you end up with a group membership
   # filter of "(&(objectClass=group)(member=<user's DN>))".
   # Optional: The LDAP field by which you wish to filter group membership.
   # Defaults to "member".
   filter_groups_by_user_attr = "<groups to filter by user attribute>"
   # Optional: The LDAP field from the authenticated user you wish to use as input to the above filter.
   # Defaults to "DN".
   filter_groups_by_user_value = "<groups to filter by user value>"

   # Optional: Additional LDAP filter you can define to further filter group membership results.
   group_query_filter = "<your group query filter>"

   # The LDAP field on the group you wish to use as the Chef Automate Team name for the group.
   # Defaults to "name".
   group_display_name_attr = "<group display name attribute>"
```

##### Example Configs

Depending on your directory's schema, different Group Search settings are
required:

If your directory looks like this

```LDIF
dn: dc=corp,dc=com
objectClass: dcObject
objectClass: organization
o: Example Company
dc: corp

dn: ou=People,dc=corp,dc=com
objectClass: organizationalUnit
ou: People

dn: cn=jane,ou=People,dc=corp,dc=com
objectClass: person
objectClass: inetOrgPerson
sn: doe
cn: jane

dn: cn=john,ou=People,dc=corp,dc=com
objectClass: person
objectClass: inetOrgPerson
sn: doe
cn: john

# Groups
dn: ou=Groups,dc=corp,dc=com
objectClass: organizationalUnit
ou: Groups

dn: cn=admins,ou=Groups,dc=corp,dc=com
objectClass: groupOfNames
cn: admins
member: cn=john,ou=People,dc=corp,dc=com
member: cn=jane,ou=People,dc=corp,dc=com

dn: cn=developers,ou=Groups,dc=corp,dc=com
objectClass: groupOfNames
cn: developers
member: cn=jane,ou=People,dc=corp,dc=com
```

then the following would be required:

```toml
base_user_search = "ou=People,dc=corp,dc=com"
username_attr = "cn"
user_id_attr = "cn"
user_display_name_attr = "cn"

base_group_search = "ou=Groups,dc=corp,dc=com"
filter_groups_by_user_value = "DN"
filter_groups_by_user_attr = "member" # default
group_display_name_attr = "cn"
```

However, if your schema looks like this -- with no list of members in your group entries:

```LDIF
dn: dc=corp,dc=com
objectClass: dcObject
objectClass: organization
o: Example Company
dc: corp

dn: ou=People,dc=corp,dc=com
objectClass: organizationalUnit
ou: People

dn: cn=jane,ou=People,dc=corp,dc=com
objectClass: person
objectClass: inetOrgPerson
sn: doe
cn: jane
departmentNumber: 1000
departmentNumber: 1001

dn: cn=john,ou=People,dc=corp,dc=com
objectClass: person
objectClass: inetOrgPerson
sn: doe
cn: john
departmentNumber: 1000
departmentNumber: 1002

dn: ou=Groups,dc=corp,dc=com
objectClass: organizationalUnit
ou: Groups

dn: cn=admins,ou=Groups,dc=corp,dc=com
objectClass: posixGroup
cn: admins
gidNumber: 1000

dn: cn=developers,ou=Groups,dc=corp,dc=com
objectClass: posixGroup
cn: developers
gidNumber: 1001

dn: cn=designers,ou=Groups,dc=corp,dc=com
objectClass: posixGroup
cn: designers
gidNumber: 1002
```

you'll need different settings to tie users and groups together:

```toml
base_user_search = "ou=People,dc=corp,dc=com"
username_attr = "cn"
user_id_attr = "cn"
user_display_name_attr = "cn"

base_group_search = "ou=Groups,dc=corp,dc=com"
filter_groups_by_user_value = "departmentNumber"
filter_groups_by_user_attr = "gidNumber"
group_display_name_attr = "cn"
```

### Troubleshooting

The following section will lay down some indicators to determine which step of
the login process has failed.

#### Troubleshoot your Connection

If the host or port was wrong, or Chef Automate was not able to reach the LDAP
service, the login screen will display

> Internal Server Error
>
> Login error.

In the logs (`journalctl -u chef-automate`), you'll find a line from
`automate-dex.default` like this -- note that for readability, the timestamp
and service name has been removed from this example log):

```text
level=error msg="Failed to login user: failed to connect: LDAP Result Code 200 \"\": dial tcp 192.168.33.223:10637: getsockopt: connection refused"
```

Note that the log contains the _IP address_ even when the LDAP server was
configured via hostname.  Double-checking that can be helpful to exclude issues
in domain-name resolution.

Issues in TLS verification manifest in the same way, but the log indicates that:

```text
level=error msg="Failed to login user: failed to connect: LDAP Result Code 200 \"\": x509: certificate is valid for localhost, not dex-dev.test"
```

#### Troubleshoot Bind

Issues in bind manifest in the same way ("Internal Server Error") as Connect issues.
However, they differ in what gets logged:

```text
level=error msg="Failed to login user: ldap: initial bind for user \"cn=service_account,dc=corp,dc=com\" failed: LDAP Result Code 49 \"Invalid Credentials\": "
```

#### Troubleshoot User Search

There's two main ways the user search could fail, and they lead to different
login failures: One is queries that can't be executed at all, leading to

> Internal Server Error
>
> Login error.

in the browser and a line like

```text
level=info msg="performing ldap search ou=Peoples,dc=example,dc=org sub (cn=jane)"
level=error msg="Failed to login user: ldap: search with filter \"(cn=jane)\" failed: LDAP Result Code 32 \"No Such Object\": "
```

in the logs.

One possible cause (whose logs you see here) is a misconfigured
`base_user_search_dn`.

When the user search is executed successfully, but fails to return a useful user
record, the browser will show the login prompt with an error banner saying

> Username or password is incorrect.

In the logs, you'll find more information. There's a line informing you about
the actual _search_ query,

```text
level=info msg="performing ldap search ou=People,dc=corp,dc=com sub (cnn=jane)"
```

together with an entry saying that nothing was returned by the attempted query:

```text
level=error msg="ldap: no results returned for filter: \"(cnn=jane)\""
```

In this example output, the `username_attr` was set to `cnn` (not `cn`).

Since there's no way for the LDAP integration to determine whether a
configuration was _wrong_ or the provided user does not exist, the login UI can
only assume that the credentials were invalid.

Note that invalid entries for `user_query_filter` will lead to queries that
return no entries, too. Setting

```toml
user_query_filter = "(objectClass=person)"
```

will lead to the following logs:

```text
level=info msg="performing ldap search ou=People,dc=example,dc=org sub (&(objectClass=person(cn=jane))" connector=LDAP
level=error msg="ldap: no results returned for filter: \"(&(objectClass=person(cn=jane))\"" connector=LDAP
```

{{% warning %}}
User search also fails if more than one user is returned.
{{% /warning %}}

Ensure that a search for `username_attr` with the given search base can only
return one user.  Something like this could happen (simplified for
demonstration):

```LDIF
dn: cn=jane,ou=Denver,ou=People,dc=corp,dc=com
sn: doe
cn: jane
username: jdoe

dn: cn=john,ou=Boston,ou=People,dc=corp,dc=com
sn: doe
cn: john
username: jdoe
```

with

```toml
base_user_search_dn = "ou=People,dc=corp,dc=com"
username_attr = "username"
```

neither Jane Doe nor her brother could login to Chef Automate. There would be a
log indicating that multiple users have been returned.

This situation would be averted by setting `username_attr = "cn"`; or by
restricting `base_user_search_dn`, if you only want to allow people from one of
either cities to use Chef Automate.

{{% warning %}}
Attributes that have been configured, but are not found in the results, lead to
user search failures, too. Note that this also affects default values.
{{% /warning %}}

Finally, a successful user search logs a line like the following:

```text
level=info msg="username \"jane\" mapped to entry cn=jane,ou=People,dc=corp,dc=com"
```

#### Troubleshoot Login Bind

Failures in login bind that are not caused by invalid credentials will lead to

> Internal Server Error
>
> Login error.

accompanied by a log line with more details, starting with `Failed to login user`.

#### Troubleshoot Group Search

Failures in retrieving a user's groups will inhibit their login with

> Internal Server Error
>
> Login error.

and logs like

```text
level=info msg="performing ldap search ou=Groups,dc=example,dc=org sub (member=cn=jane,ou=People,dc=example,dc=org)"
level=error msg="Failed to login user: ldap: failed to query groups: ldap: search failed: LDAP Result Code 32 \"No Such Object\": "
```

This, for example, is what you see when the `base_group_search_dn` does not
exist ("ou=Groups,dc=...").

However, contrary to how _User Search_ works, an empty result from _Group
Search_ will not inhibit login, it will merely not populate the user's
internal record with any groups.

A successful login causes log entries like the following:

```text
level=info msg="performing ldap search ou=People,dc=corp,dc=com sub (cn=jane)"
level=info msg="username \"jane\" mapped to entry cn=jane,ou=People,dc=corp,dc=com"
level=info msg="performing ldap search ou=Groups,dc=corp,dc=com sub (member=cn=jane,ou=People,dc=corp,dc=com)"
level=info msg="login successful: connector \"ldap\", username=\"jane\", email=\"janedoe@example.com\", groups=[\"admins\" \"developers\"]"
```

and subsequent API authorization request logs containing the user's _subjects_:

```text
level=info msg="Authorization Query" action=search resource="compliance:profiles" result=true subject="[team:ldap:admins team:ldap:developers user:ldap:jane]"
```

#### Troubleshoot LDAP search Queries

For debugging purposes it can be useful to execute LDAP queries manually using
the `ldapsearch` utility. On Ubuntu, it's provided via `ldap-utils` (i.e.,
`sudo apt-get install ldap-utils`).

The _User Search_ query looks like this, with comments referencing the
configurables for LDAP integration:

```shell
ldapsearch -H ldap://ldap-server:636/ \  # host
  -D cn=service_account,dc=corp,dc=com \ # bind_dn
  -w admin \                             # bind_password
  -b ou=People,dc=corp,dc=com \          # base_user_search_dn
  -s sub \
  '(cn=jane)'                            # (username_attr=what-was-provided-via-login-form)
```

When using anonymous bind:

```shell
ldapsearch -H ldap://ldap-server:636/ \ # host
  -b ou=People,dc=corp,dc=com \         # base_user_search_dn
  -s sub \
  '(cn=jane)'                           # (username_attr=what-was-provided-via-login-form)
```

If you've configured a `user_query_filter`, it's wrapped into the filter
argument:

```shell
  '(&(objectClass=person)(cn=jane))'    # (&user_query_filter(username_attr=what-was-provided-via-login-form))
```

Once a user directory entry has been retrieved, the password can be verified,
and the group query can be constructed from it:

Let's assume we've gotten the entry for user `jane`:

```LDIF
# jane, People, corp.com
dn: cn=jane,ou=People,dc=corp,dc=com
objectClass: person
objectClass: inetOrgPerson
sn: doe
cn: jane
```

then the password verification can be simulated by

```shell
ldapsearch -H ldap://ldap-server:636/ \ # host
  -b cn=jane,ou=People,dc=corp,dc=com \ # always the entry's DN
  -w janespassword                      # as provided via login from
```

where any non-failure result (such as `32 No such object`) would indicate valid
credentials.

Finally, the group search query for that user entry looks like

```shell
ldapsearch -H ldap://ldapserver:636/ \        # host
  -D cn=service_account,dc=corp,dc=com \      # bind_dn
  -w admin \                                  # bind_password
  -b ou=Groups,dc=corp,dc=com \               # base_group_search_dn
  -s sub \
  '(member=cn=jane,ou=People,dc=corp,dc=com)' # (filter_groups_by_user_attr=[that attr of user entry])
```

With an additional `group_query_filter`, the final filter is

```shell
  '(&(objectClass=group)(member=cn=jane,ou=People,dc=corp,dc=com))' # (&group_query_filter(filter_groups_by_user_attr=[...])
```

Note: if the user entry contains more than one `filter_groups_by_user_attr`
attribute, multiple queries will be executed, and their results combined.

#### Other Common Issues

If a user, following a login through LDAP or SAML, sees a

> 502 Bad Gateway

error page, the group information collected for the user exceeds some
internal limits.

This can have two causes: the user having too many groups, or referencing LDAP
groups by distinguished names (DN). The latter can cause little information
(e.g. the group name _"admins"_) to grow out of proportion (e.g.
_"cn=admins,ou=DeptA,ou=CityB,ou=StateWA,dc=subcorp,dc=corp,dc=com"_). This can
be mitigated by changing the `group_display_name_attr` from `DN` to `cn`
(common name).  Note that for authorization purposes, that is also advisable.
LDAP-provided groups are referenced in policies using `team:ldap:<group-name>`.
Thus `team:ldap:admins` is handier than
`team:ldap:cn=admins,ou=DeptA,ou=CityB,ou=StateWA,dc=subcorp,dc=corp,dc=com`.

The other cause, having too many groups for a user, can be addressed by using
the `group_query_filter` to restrict the group results (for all users).
Anything expressible in an LDAP search query and supported by the LDAP service
can be configured there. For example, given a flat list of groups in a
directory service like

```LDIF
cn=group1,ou=Groups,dc=corp,dc=com
cn=group2,ou=Groups,dc=corp,dc=com
cn=group3,ou=Groups,dc=corp,dc=com
cn=group4,ou=Groups,dc=corp,dc=com
cn=group5,ou=Groups,dc=corp,dc=com
```

a `group_query_filter` of `(|(cn=group1)(cn=group2))` would restrict the
group search results to either one of those groups. Note that this has no
implications on which users get authenticated; it only affects the groups
recognized by Chef Automate. For example, given users Jane and Jack, where
Jane is a member of `group1` and `group3`, and Jack of `group3` and `group4`:
Jane's groups would resolve to `group1` only, and Jack would have no groups
-- but still be able to access Chef Automate.
In a similar manner, selected groups could be excluded from the results
explicitly, by using a filter like `(!cn=group2)`

Given a more structured directory service layout, including multiple trees of
groups, further options become possible:
Assuming the layout is like

```LDIF
cn=group1,ou=AGroups,dc=corp,dc=com
cn=group2,ou=AGroups,dc=corp,dc=com
cn=group3,ou=BGroups,dc=corp,dc=com
cn=group4,ou=BGroups,dc=corp,dc=com
cn=group5,ou=CGroups,dc=corp,dc=com
```

you can use your directory server's query capabilities to restrict the
results to a subtree. The concrete details depend on the product in use; for
example in servers supporting _extensible match_, all group entries below
`AGroups` and `BGroups` could be retrieved using a `group_query_filter` of
`(|(ou:dn:=AGroups)(ou:dn:=BGroups))`.

See [LDAP Wiki Extensible Match Search Filter](http://ldapwiki.com/wiki/ExtensibleMatch) for details.
