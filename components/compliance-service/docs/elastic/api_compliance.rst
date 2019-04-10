=====================================================
|api compliance|
=====================================================

The |api compliance| is a REST-based API that is designed to be easy and predictable and to have resource-oriented URL endpoints. It uses common HTTP verbs and response codes to indicate API errors. Therefore the API can be understood by standard HTTP clients and libraries. In general the API uses |json| as data input and output format.

The |api compliance| is located at ``https://hostname/api/`` for on-premises installations of the |chef compliance| server.

About API Requests
=====================================================
Some notes about API requests:

* There are two kinds of tokens involved: ``refresh tokens`` and ``access tokens``. A ``refresh token`` is a long-lived token that can be used to initially identify with the service, in exchange for an ``access token``. The ``access token`` is short-lived (12 hours) and used for every request against the API. It is mostly referred to as "API token" below.
* Examples in this document use ``-H "Authorization: Bearer $API_TOKEN"`` to represent the retrieved API (access) token. A retrieved API token is a |json jwt| and quite large. See the examples below for how to get an access token in |bash|.
* When running commands as an administrator and if the ``API_TOKEN`` is not used, some requests to the |api compliance| will return ``403`` (forbidden) if the user making the requests does not have appropriate permissions.
* Any time a |json| block is part of a request to the |api compliance|, the content type ``application/json`` must also be specified. Use the ``-H`` option: ``-H "Content-Type: application/json"``.

  |chef compliance| uses the API token to allow access to the |api compliance|. The API token must be included as part of ``every HTTP request`` to the |api compliance| with the API token included as part of the header:

  .. code-block:: javascript

     Authorization: Bearer API_TOKEN

  where the ``API_TOKEN`` is a valid |company_name| |api compliance| token similar to ``eyJhbGciOiJSUzI1NiIsImtpZCI6InJFZi1DUVZQYi1xTXY3WF9CdXZNZ3B5bnc2R3J0OW1adlN3NVhOY2VISjBB ...``.

  .. code-block:: bash

     API_URL="https://example.com/api"
     API_TOKEN="eyJhbGciOiJSUzI1NiIsImtpZCI6InJFZi1DUVZQYi1xTXY3WF9CdXZNZ3B5bnc2R3J0OW1adlN3NVhOY2VISjBBZzBaVVFUZTZCYVNROW91UWRob0JsemRvLV93V0VXd3ZJVEU4SS1KMk81enljRVhoZlFvU2JaeThfMVZTekt6SVN6LXFiYVZtUElqZHZiU1hneTNvY3Rla3RKRkYtWWNUa3lXbVhSaTd4OEVNSU9EVFFnVEplMV8zODhTZGt0MEdub0xJUEVnWXp..."
     curl -X GET "$API_URL/users" -H "Authorization: Bearer $API_TOKEN"

  For readability, most API examples on this page reference |chef compliance| object names (or ``login`` in the case of users and organizations). You can also use object **UUIDs** instead. For example, both these calls retrieve the ``Dev Ops`` environment:

  .. code-block:: bash

     $ export AUTH="Authorization: Bearer $API_TOKEN"   # used in examples below
     $ curl -X GET "$API_URL/owners/john/envs/Dev%20Ops" -H "$AUTH"
     {"owner":"1654fe61-d15b-4fbe-5fa9-67859135fc7e","name":"Dev Ops","lastScan":"0001-01-01T00:00:00Z","complianceStatus":-1,"patchlevelStatus":-1,"unknownStatus":0,"id":"53a7f189-4417-44ba-57f4-f3d397589973"}

     $ curl -X GET "$API_URL/owners/1654fe61-d15b-4fbe-5fa9-67859135fc7e/envs/53a7f189-4417-44ba-57f4-f3d397589973" -H "$AUTH"
     {"owner":"1654fe61-d15b-4fbe-5fa9-67859135fc7e","name":"Dev Ops","lastScan":"0001-01-01T00:00:00Z","complianceStatus":-1,"patchlevelStatus":-1,"unknownStatus":0,"id":"53a7f189-4417-44ba-57f4-f3d397589973"}


Obtaining an API token
=====================================================
There are three ways of obtaining an API token:

1. Using the "About" dialogue of |chef compliance|
2. Using a ``refresh token`` (which can also be obtained from the "About" dialogue, but can be reused).
3. By sending username and password to the login endpoint.

Note that the direct exchange of username and password for an access token is not possible for users logging in using Chef Server credentials. Options 1) and 2) are available to every user.

The API can be used to obtain an ``access token`` from a ``refresh token`` as follows:

  .. code-block:: bash

     $ export REFRESH_TOKEN="1/eT0ic04N4M10Z8kBB6XkJVPwDBFxDs9Z9bMkgzE5tLgpWZqYEnulXjNTeZx23v8pIrethF--egktQSKJTM_T7w=="   # an example
     $ curl -X POST "$API_URL/login" -d "{\"token\": \"$REFRESH_TOKEN\"}"
     {"access_token":"eyJhbGciOiJSUzI1NiIsImtpZCI6InRySE ...abbreviated..."}

It is convenient to save the API token for further use:

  .. code-block:: bash

     $ export API_TOKEN=$(curl -X POST $API_URL/access_token -d "{\"token\": \"$REFRESH_TOKEN\"}" | sed -e "s/.*access_token\":\"\([^\"]*\)\".*/\1/")

Since refresh tokens do not expire, it is possible to revoke them:

  .. code-block:: bash

     $ curl -X DELETE $API_URL/login -d "{\"token\": \"$REFRESH_TOKEN\"}"
     {"status":"success"}

Revoked refresh tokens can no longer be used to obtain access tokens:

  .. code-block:: bash

     $ curl -X POST $API_URL/login -d "{\"token\": \"$REFRESH_TOKEN\"}"
     unable to trade refresh token for access token with issuer: invalid_request

To get an access token given |chef compliance| user credentials, use the ``/login`` endpoint:

  .. code-block:: bash

     $ curl -X POST $API_URL/login -d "{\"userid\": \"admin\", \"password\": \"nimda\"}"
     eyJhbGciOiJSUzI1NiIsImtpZCI6InRySE ...abbreviated...


Response Codes
=====================================================
The |api compliance| uses conventional HTTP response codes to highlight a request success or failure. The following codes are used:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Response Code
     - Description
   * - ``200``
     - OK. Everything worked as expected.
   * - ``400``
     - Bad Request. In most cases a required parameter is missing.
   * - ``401``
     - Unauthorized. No valid API key provided.
   * - ``402``
     - Request Failed. Parameters were valid but request failed.
   * - ``403``
     - Forbidden. You do not have the permission to execute the request.
   * - ``404``
     - Not Found. The specified resource could not be found.
   * - ``429``
     - Too Many Requests. You reached the rate limit.
   * - ``500``, ``501``, ``502``, ``503``
     - Server Error. Something went wrong.

In general, ``2xx`` codes indicate success, ``4xx`` indicate a request error (e.g. data is missing) and ``5xx`` indicate an error with the |api compliance|.

/version
=====================================================
The ``/version`` endpoint has the following method: ``GET``.

GET
-----------------------------------------------------
Use this method to get the version of the |api compliance| without authentication.

**Request**

.. code-block:: xml

   GET /api/version

Example tested in ``bash``:

.. code-block:: bash

   # Define a variable for the hostname of the |chef compliance| server
   API_URL="https://example.com/api"
   curl -X GET "$API_URL/version"

.. note:: If you don't have a trusted SSL certificate and would like to turn off curl's verification of the certificate, use the ``-k`` (or ``--insecure``) option.

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "api": "chef-compliance",
     "version": "0.14.3"
   }

/compliance
=====================================================
The ``/compliance`` endpoint has the following methods: ``GET`` and ``POST``. The ``GET`` method may be used to return information about owners, all users, a named user, to download a profile as a |tar gz| file, and to upload profiles (including as |tar gz| or |zip| files).

GET (all users)
-----------------------------------------------------
Use to return the compliance profiles for all users.

**Request**

.. code-block:: xml

   GET /api/user/compliance

For example:

.. code-block:: bash

   curl -X GET "$API_URL/user/compliance" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "cis": {
       "cis-ubuntu-level1": {
         "id": "cis-ubuntu-level1",
         "owner": "cis",
         "name": "cis-ubuntu-level1",
         "title": "CIS Ubuntu 14.04 LTS Server Benchmark Level 1",
         "version": "1.0.0",
         "summary": "CIS Ubuntu 14.04 LTS Server Benchmark",
         "description": "# CIS Ubuntu 14.04 LTS Server Benchmark\n\ncopyright",
         "license": "Proprietary, All rights reserved",
         "copyright": "Chef Software, Inc.",
         "copyright_email": "grantmc@chef.io"
        }
     },
     "john": {
       "linux": {
         "id": "linux",
         "owner": "john",
         "name": "linux",
         "title": "Basic Linux",
         "version": "1.0.0",
         "summary": "Verify that Linux nodes are configured securely",
         "description": "# Basic Linux Compliance Profile\n\ncopyright",
         "license": "Proprietary, All rights reserved",
         "copyright": "Chef Software, Inc.",
         "copyright_email": "grantmc@chef.io"
       },
     ...
     }
   }

GET (named user)
-----------------------------------------------------
Use to return profile details about the named user.

**Request**

.. code-block:: xml

   GET /api/owners/OWNER/compliance/PROFILE

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/compliance/ssh" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "id": "ssh",
     "owner": "base",
     "name": "ssh",
     "title": "Basic SSH",
     "version": "1.0.0",
     "summary": "Verify that SSH Server and SSH Client are configured securely",
     "description": "# Basic SSH Compliance Profile\n\ncopyright",
     "license": "Proprietary, All rights reserved",
     "copyright": "Chef Software, Inc.",
     "copyright_email": "grantmc@chef.io",
     "rules": {
       "spec/ssh_folder_spec": {
         "title": "SSH folder configuration",
           "rules": {
             "chef/ssh/basic-1": {
               "impact": 1,
               "title": "/etc/ssh should be a directory",
               "desc": "In order for OpenSSH to function correctly..."
             },
           ...
         }
       }
     }
   }

GET (owner)
-----------------------------------------------------
This method returns a list of all compliance profiles for the named owner.

**Request**

.. code-block:: xml

   GET /api/owners/OWNER/compliance

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/compliance" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "linux": {
       "id": "linux",
       "owner": "chef",
       "name": "chef/linux",
       "title": "Basic Linux",
       "version": "1.0.0",
       "summary": "Verify that Linux nodes are configured securely",
       "description": "# Basic Linux Compliance Profile\n\ncopyright",
       "license": "Proprietary, All rights reserved",
       "copyright": "Chef Software, Inc.",
       "copyright_email": "grantmc@chef.io"
       },
     "mysql": {
       "id": "mysql",
       "owner": "chef",
       "name": "chef/mysql",
       "title": "Basic MySQL",
       "version": "1.0.0",
       "summary": "Verify that MySQL Server is configured securely",
       "description": "# Basic MySQL Compliance Profile\n\ncopyright",
       "license": "Proprietary, All rights reserved",
       "copyright": "Chef Software, Inc.",
       "copyright_email": "grantmc@chef.io"
     },
     ...
   }

It contains the following attributes:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Parameter
     - Description
   * - ``id``
     - String. The profile identifier.
   * - ``owner``
     - String. The profile owner.
   * - ``version``
     - String. The version of the profile.
   * - ``title``
     - String. A human-readable title for the profile.
   * - ``summary``
     - String. A description of the primary purpose of the profile.
   * - ``description``
     - String. A description for the profile.
   * - ``license``
     - String. The license for the profile.
   * - ``copyright``
     - String. The individual or organization that holds the copyright.
   * - ``copyright_email``
     - String. The email for the ``copyright`` holder.}


GET (profile as tar.gz)
-----------------------------------------------------
Use to download a profile as |tar gz| file. A profile, once downloaded, may be edited locally, and then re-uploaded back to the |chef compliance| server using the ``POST`` method.

**Request**

.. code-block:: xml

   GET /api/owners/OWNER/compliance/PROFILE/tar

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/compliance/ssh/tar" -H "$AUTH" > /tmp/profile.tar.gz
   tar -zxvf /tmp/profile.tar.gz

**Response**

TAR STREAM

POST
-----------------------------------------------------
Use to upload a compliance profile as a |tar gz| or |zip|. This process will extract the owner and identifier, and then use that information to place the profile into the correct location on the |chef compliance| server.

**Request**

.. code-block:: xml

   POST /api/owners/OWNER/compliance

For example:

.. code-block:: bash

   tar -cvzf /tmp/newprofile.tar.gz newprofile
   curl -X POST "$API_URL/owners/john/compliance?contentType=application/x-gtar" \
   -H "$AUTH" --form "file=@/tmp/newprofile.tar.gz"

   zip -r /tmp/newprofile.zip newprofile
   curl -X POST "$API_URL/owners/john/compliance?contentType=application/zip" \
   -H "$AUTH" --form "file=@/tmp/newprofile.zip"

**Response**

No Content

POST (profile as tar.gz)
-----------------------------------------------------
Use to upload a profile using a |tar gz| file.

**Request**

.. code-block:: xml

   POST /api/owners/OWNER/compliance/PROFILE/tar

For example:

.. code-block:: bash

   tar -cvzf /tmp/newprofile.tar.gz newprofile
   curl -X POST "$API_URL/owners/john/compliance/newprofile/tar" \
   -H "$AUTH" --data-binary "@/tmp/newprofile.tar.gz"

**Response**

No Content

POST (profile as Zip)
-----------------------------------------------------
Use to upload a profile using a |zip| file. A |zip| file may be created with a command similar to:

.. code-block:: bash

   $ zip -r /tmp/newprofile.zip profile_directory

or it may be created from the context menus in the |windows| and/or |mac os x| graphical user interfaces.

**Request**

.. code-block:: xml

   POST /api/owners/OWNER/compliance/PROFILE/zip

For example:

.. code-block:: bash

   zip -r /tmp/newprofile.zip newprofile
   curl -X POST "$API_URL/owners/john/compliance/newprofile/zip" \
   -H "$AUTH" --data-binary "@/tmp/newprofile.zip"

.. The example above seems to be a mix of API request + command line stuff. What does the actual request look like?

**Response**

No Content

DELETE
-----------------------------------------------------
Use to delete a profile.

**Request**

.. code-block:: xml

   DELETE /api/owners/OWNER/compliance/PROFILE

For example:

.. code-block:: bash

   curl -w "%{http_code}" -X DELETE "$API_URL/owners/john/compliance/ssh" -H "$AUTH"


*** Response ***

No Content

/envs
=====================================================
The ``/envs`` endpoint has the following methods: ``DELETE``, ``GET`` (for both all environments or for a single, named environment), and ``POST`` .

GET (named environment)
-----------------------------------------------------
Use to return details about the named environment.

**Request**

.. code-block:: xml

   GET /api/owners/USER/envs/ENV

where ``/USER`` is the identifier for a user or an organization.

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/envs/Production" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

  {
    "id": "b771e025-6445-4ead-5cac-b466ea725177",
    "owner": "7ae9dd7d-5201-4ae3-4949-60eb4b902e77",
    "name": "Production",
    "lastScan": "0001-01-01T00:00:00Z",
    "complianceStatus": 0,
    "patchlevelStatus": 0,
    "unknownStatus": 0
  }

GET (all environments)
-----------------------------------------------------
Use to get a list of all environments.

**Request**

.. code-block:: xml

   GET /api/owners/USER/envs

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/envs" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   [
     {
       "id": "b771e025-6445-4ead-5cac-b466ea725177",
       "owner": "7ae9dd7d-5201-4ae3-4949-60eb4b902e77",
       "name": "Production",
       "lastScan": "0001-01-01T00:00:00Z",
       "complianceStatus": 0,
       "patchlevelStatus": 0,
       "unknownStatus": 0
     },
     {
       "id": "a1f16feb-d18e-4725-6462-8b296a709d73",
       "owner": "7ae9dd7d-5201-4ae3-4949-60eb4b902e77",
       "name": "Development",
       "lastScan": "0001-01-01T00:00:00Z",
       "complianceStatus": 0,
       "patchlevelStatus": 0,
       "unknownStatus": 0
     }
   ]

POST
-----------------------------------------------------
Use to create an environment.

This method has the following parameters:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Parameter
     - Description
   * - ``name``
     - String. Required. The name of the environment.

**Request**

.. code-block:: xml

   POST /api/owners/USER/envs

For example:

.. code-block:: bash

   curl -X POST "$API_URL/owners/john/envs" \
   -H "Content-Type: application/json" -H "$AUTH" -d '{"name":"Development"}'

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "id": "a1f16feb-d18e-4725-6462-8b296a709d73",
     "owner": "7ae9dd7d-5201-4ae3-4949-60eb4b902e77",
     "name": "Development",
     "lastScan": "0001-01-01T00:00:00Z",
     "complianceStatus": 0,
     "patchlevelStatus": 0,
     "unknownStatus": 0
   }

DELETE
-----------------------------------------------------
Use to delete the named environment.

**Request**

.. code-block:: xml

   DELETE /api/owners/USER/envs/ENV

For example:

.. code-block:: bash

   curl -w "%{http_code}" -X DELETE "$API_URL/owners/john/envs/Production" -H "$AUTH"

**Response**

No Content

/jobs
=====================================================
The ``/jobs`` endpoint has the following methods: ``DELETE``, ``GET`` (for both all jobs or for a single, named job), and ``POST``

GET (all jobs)
-----------------------------------------------------
Use to get a list of all jobs.

**Request**

.. code-block:: xml

   GET /api/owners/USER/jobs

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/jobs" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   [{
     "id": "76fdce4d-0734-441c-b01b-6dd6bfce081a",
     "status": "done",
     "nextRun": "2015-07-21T20:55:00Z",
     "schedule": "2015-07-21T20:55:00Z"
   },
   {
     "id": "c8ba8e88-7e45-4253-9081-cbb17a5f0c76",
     "status": "scheduled",
     "name": "Rec",
     "nextRun": "2015-07-21T23:11:00Z",
     "schedule": {
       "month": "*",
       "day": "21",
       "weekday": "*",
       "hour": "23",
       "minute": "11"
     }
   },
   {
     "id": "e0d5bbf0-a1c4-4c50-ad09-fc1486068e8c",
     "status": "skipped",
     "nextRun": "0001-01-01T00:00:00Z",
     "schedule": "2015-07-21T20:25:00Z"
   }]

It contains the following attributes:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Parameter
     - Description
   * - ``id``
     - UUID. The identifier of the job run.
   * - ``name``
     - String. The name of the job.
   * - ``nextRun``
     - ISO date. The time of the next scheduled run, in UTC. For example: ``2015-07-21T20:50:00Z``.
   * - ``schedule``
     - Cron or ISO date. The schedule for the job run. For example: ``2015-07-21T20:50:00Z`` or ``{ "month": "*", "day": "21", "weekday": "*", "hour": "23", "minute": "11" }``.
   * - ``status``
     - String. The status of the job run: ``done``, ``scheduled``, or ``skipped``.

GET (named job)
-----------------------------------------------------
Use to return details about a specific job.

**Request**

.. code-block:: xml

   GET /api/owners/USER/jobs/JOB_ID

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/jobs/c8ba8e88-7e45-4253-9081-cbb17a5f0c76" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "id": "c8ba8e88-7e45-4253-9081-cbb17a5f0c76",
     "status": "scheduled",
     "name": "Rec",
     "nextRun": "2018-07-21T23:11:00Z",
     "schedule": {
       "month": "*",
       "day": "21",
       "weekday": "*",
       "hour": "23",
       "minute": "11"
     },
     "tasks": [{
       "type": "scan",
       "environments": [{
         "id": "b771e025-6445-4ead-5cac-b466ea725177",
         "nodes": ["d850ba44-7a82-4177-50db-79be1143d632", "33ecfce5-f781-4eb7-6828-beb090ffe9b5"]
       }],
       "compliance": [{
         "owner": "base",
         "profile": "linux"
       }, {
         "owner": "base",
         "profile": "ssh"
       }],
       "patchlevel": [{
         "profile": "default",
         "force": false
       }]
     }]
   }

It contains the following attributes:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Parameter
     - Description
   * - ``id``
     - UUID. The identifier of the job run.
   * - ``name``
     - String. The name of the job.
   * - ``nextRun``
     - ISO date. The time of the next scheduled run, in UTC. For example: ``2015-07-21T20:50:00Z``.
   * - ``schedule``
     - Cron or ISO date. The schedule for the job run. For example: ``2015-07-21T20:50:00Z`` or ``{ "month": "*", "day": "21", "weekday": "*", "hour": "23", "minute": "11" }``.
   * - ``status``
     - String. The status of the job run: ``done``, ``scheduled``, or ``skipped``.
   * - ``tasks``
     - An array of compliance scans or patch runs. Two types of tasks are available: ``scan`` and ``patchrun``. The |json| object for ``tasks`` is similar to:

       .. code-block:: javascript

          "tasks": [{
            "compliance": [{
             "owner": "base",
              "profile": "linux"
            }, {
              "owner": "base",
              "profile": "ssh"
            }],
            "environments": [{
              "id": "b771e025-6445-4ead-5cac-b466ea725177",
              "nodes": ["b771e025-6445-4ead-5cac-b466ea725177", "33ecfce5-f781-4eb7-6828-beb090ffe9b5"]
            }],
            "patchlevel": [{
              "profile": "default"
            }],
            "type": "scan"
          }]

POST
-----------------------------------------------------
Use to create a job.

**Request**

.. code-block:: xml

   POST /api/owners/USER/jobs

The request uses a |json| object similar to:

.. code-block:: javascript

   {
     "id": "c8ba8e88-7e45-4253-9081-cbb17a5f0c76",
     "name": "Rec1",
     "schedule": {
       "hour": "23",
       "minute": "11",
       "day": "21",
       "month": "*",
       "weekday": "*"
     },
     "tasks": [{
       "compliance": [{
        "owner": "base",
         "profile": "linux"
       }, {
         "owner": "base",
         "profile": "ssh"
       }],
       "environments": [{
         "id": "b771e025-6445-4ead-5cac-b466ea725177",
         "nodes": ["d850ba44-7a82-4177-50db-79be1143d632", "33ecfce5-f781-4eb7-6828-beb090ffe9b5"]
       }],
       "patchlevel": [{
         "profile": "default"
       }],
       "type": "scan"
     }]
   }

For example:

.. code-block:: bash

   curl -X POST "$API_URL/owners/john/jobs" \
   -H "Content-Type: application/json" -H "$AUTH" -d '{ JSON_BLOCK }'

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

  {
    "status":"scheduled",
    "name":"Rec1",
    "nextRun":"2016-03-21T23:11:00Z",
    "id":"351f8933-6fd4-47be-7d47-7dbdb0abd306",
    "month":"*","day":"21","weekday":"*","hour":"23","minute":"11","date":"0001-01-01T00:00:00Z","runs":null
  }

DELETE
-----------------------------------------------------
Use to delete a job.

**Request**

.. code-block:: xml

   DELETE /api/owners/USER/jobs/JOB_ID

For example:

.. code-block:: bash

   curl -w "%{http_code}" -X DELETE "$API_URL/owners/john/jobs/c8ba8e88-7e45-4253-9081-cbb17a5f0c76" -H "$AUTH"

**Response**

No Content

/keys
=====================================================
The ``/keys`` endpoint has the following methods: ``DELETE``, ``GET``, ``PATCH``, and ``POST``.

GET
-----------------------------------------------------
Use to get the list of key pairs available to the named user.

**Request**

.. code-block:: xml

   GET /api/owners/USER/keys

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/keys" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   [{
     "owner": "7ae9dd7d-5201-4ae3-4949-60eb4b902e77",
     "name": "vagrant",
     "id": "2bfe1865-d602-4912-5dcb-b037447fae91",
     "public": ""
   }]

PATCH
-----------------------------------------------------
Use to edit the details for the named key pair that is available to the named user.

**Request**

.. code-block:: xml

   PATCH /api/owners/USER/keys/KEY_NAME

For example:

.. code-block:: bash

   curl -w "%{http_code}" -X PATCH "$API_URL/owners/john/keys/vagrant" -H "$AUTH" -d '{ JSON_BLOCK }'

**Response**

No Content

POST
-----------------------------------------------------
Use to add a key pair to be available to the named user.

This method has the following parameters:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Parameter
     - Description
   * - ``name``
     - String. The human-readable name of the key.
   * - ``private``
     - String. The private key, in |open ssh| format.

**Request**

.. code-block:: xml

   POST /api/owners/USER/keys

with a |json| object similar to:

.. code-block:: javascript

   {
     "name": "vagrant",
     "private": "-----BEGIN RSA PRIVATE\
                KEY-----\nMIIEogIBAAKCAQEA6NF8iallvQVp22WDkTkyrtvp9eWW6A8YVr+\
                kz4TjGYe7gHzI\nw+niNltGEFHzD8+v1I2YJ6oXevct1YeS0o9HZyN1Q9qgCg\
                zUFtdOKLv6IedplqoP\nkcmF0aYet2PkEDo3MlTBckFXPITAMzF8dJSIFo9D8\
                HfdOV0IAdx4O7PtixWKn5y2\nhMNG0zQPyUecp4pzC6kivAIhyfHilFR61RGL\
                +GPXQ2MWZWFYbAGjyiYJnAmCP3NO\nTd0jMZEnDkbUvxhMmBYSdETk1rRgm+R\
                4LOzFUGaHqHDLKLX+FIPKcF96hrucXzcW\nyLbIbEgE98OHlnVYCzRdK8jlqm\
                8tehUc9c9WhQIBIwKCAQEA4iqWPJXtzZA68mKd\nELs4jJsdyky+ewdZeNds5\
                tjcnHU5zUYE25K+ffJED9qUWICcLZDc81TGWjHyAqD1\nBw7XpgUwFgeUJwUl\
                zQurAv+/ySnxiwuaGJfhFM1CaQHzfXphgVml+fZUvnJUTvzf\nTK2Lg6EdbUE\
                CZpigBKbKZHNYcelXtTt/nP3r3s=\n-----END RSA PRIVATE KEY-----"
   }

For example:

.. code-block:: bash

   curl -X POST "$API_URL/owners/john/keys" \
   -H "Content-Type: application/json" -H "$AUTH" -d '{ JSON_BLOCK }'

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "id": "85f92d4c-f3c6-4173-72e1-0a7a68cbecde"
   }

DELETE
-----------------------------------------------------
Use to delete the named key pair that is available to the named user.

**Request**

.. code-block:: xml

   DELETE /api/owners/USER/keys/KEY_NAME

For example:

.. code-block:: bash

   curl -w "%{http_code}" -X DELETE "$API_URL/owners/john/keys/vagrant" -H "$AUTH"

**Response**

No Content

/nodes
=====================================================
The ``/nodes`` endpoint has the following methods: ``POST``, ``PATCH`` and ``DELETE``. It is used for bulk operations, potentially across multiple environments.

POST (bulk)
-----------------------------------------------------
Use to create one or multiple nodes.

**Request**

.. code-block:: xml

   POST /api/owners/USER/nodes

with a |json| object similar to:

.. code-block:: javascript

  [
    {
      "hostname": "lb1.example.com",
      "name": "Load Balancer 1",
      "environment": "b771e025-6445-4ead-5cac-b466ea725177",
      "loginUser": "root",
      "loginMethod": "ssh",
      "loginKey": "john/vagrant"
    },
    {
      "hostname": "lb2.example.com",
      "name": "Load Balancer 2",
      "environment": "b771e025-6445-4ead-5cac-b466ea725177",
      "loginUser": "root",
      "loginMethod": "ssh",
      "loginKey": "john/vagrant"
    }
  ]

For example:

.. code-block:: bash

   curl -X POST "$API_URL/owners/john/nodes" -H "Content-Type: application/json" -H "$AUTH" \
   -d '[{"hostname":"lb1.example.com","name":"Load Balancer 1","environment":"b771e025-6445-4ead-5cac-b466ea725177","loginUser":"root","loginMethod":"ssh","loginKey":"john/vagrant"},{"hostname":"lb2.example.com","name":"Load Balancer 2","environment":"b771e025-6445-4ead-5cac-b466ea725177","loginUser":"root","loginMethod":"ssh","loginKey":"john/vagrant"}]'

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   [
    "d850ba44-7a82-4177-50db-79be1143d632",
    "33ecfce5-f781-4eb7-6828-beb090ffe9b5"
   ]

PATCH (bulk)
-----------------------------------------------------
Use to update one or multiple nodes in one request.

**Request**

.. code-block:: xml

   PATCH /api/owners/USER/nodes

with a |json| object similar to:

.. code-block:: javascript

  [
    {
      "hostname": "lb1.example.com",
      "name": "Load Balancer 1 - updated",
      "environment": "b771e025-6445-4ead-5cac-b466ea725177",
      "loginUser": "root",
      "loginMethod": "ssh",
      "loginKey": "john/vagrant"
    },
    {
      "hostname": "lb2.example.com",
      "name": "Load Balancer 2 - updated",
      "environment": "b771e025-6445-4ead-5cac-b466ea725177",
      "loginUser": "root",
      "loginMethod": "ssh",
      "loginKey": "john/vagrant"
    }
  ]

For example:

.. code-block:: bash

   curl -X POST "$API_URL/owners/john/nodes" -H "Content-Type: application/json" -H "$AUTH" \
   -d '[{"hostname":"lb1.example.com","name":"Load Balancer 1 - updated","environment":"b771e025-6445-4ead-5cac-b466ea725177","loginUser":"root","loginMethod":"ssh","loginKey":"john/vagrant"},{"hostname":"lb2.example.com","name":"Load Balancer 2 - updated","environment":"b771e025-6445-4ead-5cac-b466ea725177","loginUser":"root","loginMethod":"ssh","loginKey":"john/vagrant"}]'

**Response**

No Content

DELETE (bulk)
-----------------------------------------------------
Delete one or multiple nodes specified in the payload of the request.

**Request**

.. code-block:: xml

   DELETE /api/owners/USER/nodes

with a |json| array of node ids:

.. code-block:: javascript

  [
    "d850ba44-7a82-4177-50db-79be1143d632",
    "33ecfce5-f781-4eb7-6828-beb090ffe9b5"
  ]

For example:

.. code-block:: bash

   curl -w "%{http_code}" -X DELETE "$API_URL/owners/john/envs/Production/nodes" \
   -H "$AUTH" -d '["d850ba44-7a82-4177-50db-79be1143d632","33ecfce5-f781-4eb7-6828-beb090ffe9b5"]'

**Response**

No Content

/envs/ENV/nodes
=====================================================
The ``/envs/ENV/nodes`` endpoint has the following methods: ``GET``, ``POST`` and ``DELETE``. The ``GET`` method may be used to return information about nodes, including by environment, by named node, node status, connectivity status, lists of installed packages, compliance state, and patch state.

GET (nodes by environment)
-----------------------------------------------------
Use to get a list of all nodes for the named environment.

**Request**

.. code-block:: xml

   GET /api/owners/USER/envs/ENV/nodes

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/envs/Production/nodes" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   [
     {
       "id": "d850ba44-7a82-4177-50db-79be1143d632",
       "environment": "b771e025-6445-4ead-5cac-b466ea725177",
       "owner": "7ae9dd7d-5201-4ae3-4949-60eb4b902e77",
       "name": "192.168.100.200",
       "hostname": "192.168.100.200",
       "loginMethod": "ssh",
       "loginUser": "root",
       "loginPassword": "",
       "loginKey": "john/vagrant",
       "loginPort": 0,
       "disableSudo": false,
       "sudoOptions": "",
       "sudoPassword": "",
       "lastScan": "0001-01-01T00:00:00Z",
       "lastScanID": "",
       "arch": "",
       "family": "",
       "release": "",
       "complianceStatus": 0,
       "patchlevelStatus": 0,
       "unknownStatus": 0
     }
   ]

GET (named node)
-----------------------------------------------------
Use to return details about the named node.

**Request**

.. code-block:: xml

   GET /api/owners/USER/envs/ENV/nodes/NODE_ID

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/envs/Production/nodes/6f7336b5-380e-4e75-4b06-781950c9a1a5" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "id": "6f7336b5-380e-4e75-4b06-781950c9a1a5",
     "environment": "b771e025-6445-4ead-5cac-b466ea725177",
     "owner": "7ae9dd7d-5201-4ae3-4949-60eb4b902e77",
     "name": "192.168.100.200",
     "hostname": "192.168.100.200",
     "loginMethod": "ssh",
     "loginUser": "root",
     "loginPassword": "",
     "loginKey": "john/vagrant",
     "loginPort": 0,
     "disableSudo": false,
     "sudoOptions": "",
     "sudoPassword": "",
     "lastScan": "0001-01-01T00:00:00Z",
     "lastScanID": "",
     "arch": "",
     "family": "",
     "release": "",
     "complianceStatus": 0,
     "patchlevelStatus": 0,
     "unknownStatus": 0
   }

POST
-----------------------------------------------------
Use to create a node.

**Request**

.. code-block:: xml

   POST /api/owners/USER/envs/ENV/nodes

with a |json| object similar to:

.. code-block:: javascript

   {
     "name": "192.168.100.200",
     "hostname": "192.168.100.200",
     "loginUser": "root",
     "loginMethod": "ssh",
     "loginKey": "john/vagrant",
     "loginPort": 22
   }

For example:

.. code-block:: bash

   curl -X POST "$API_URL/owners/john/envs/Production/nodes" \
   -H "Content-Type: application/json" -H "$AUTH" -d '{ JSON_BLOCK }'

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "id":"67243304-0909-4bc3-5ed0-3637a5d0fe93",
     "hostname": "192.168.100.200",
     "name": "192.168.100.200",
     "loginUser": "root",
     "loginMethod": "ssh",
     "loginKey": "john/vagrant"
   }

DELETE
-----------------------------------------------------
Delete a node from an environment.

**Request**

.. code-block:: xml

   DELETE /api/owners/USER/envs/ENV/nodes/NODE_ID

For example:

.. code-block:: bash

   curl -w "%{http_code}" -X DELETE "$API_URL/owners/john/envs/Production/nodes/6f7336b5-380e-4e75-4b06-781950c9a1a5" -H "$AUTH"

**Response**

No Content

PATCH
-----------------------------------------------------
Use to update a node.

**Request**

.. code-block:: xml

   PATCH /api/owners/USER/envs/ENV/nodes/NODE_ID

with a |json| object similar to:

.. code-block:: javascript

  {
    "hostname": "lb1.example.com",
    "name": "Load Balancer 1 - new",
    "loginUser": "root",
    "loginMethod": "ssh",
    "loginKey": "john/vagrant"
  }

For example:

.. code-block:: bash

   curl -w "%{http_code}" -X PATCH "$API_URL/owners/john/envs/ENV/nodes/6f7336b5-380e-4e75-4b06-781950c9a1a5" -H "Content-Type: application/json" -H "$AUTH" \
   -d '{"hostname":"lb1.example.com","name":"Load Balancer 1 - new","environment":"b771e025-6445-4ead-5cac-b466ea725177","loginUser":"root","loginMethod":"ssh","loginKey":"john/vagrant"}'

**Response**

No Content

GET (connectivity)
-----------------------------------------------------
Use to show the connectivity state for the named node.

**Request**

.. code-block:: xml

   GET /api/owners/USER/envs/ENV/nodes/NODE_ID/connectivity

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/envs/Production/nodes/6f7336b5-380e-4e75-4b06-781950c9a1a5/connectivity" -H "$AUTH"

**Response**

The request will return one of the following response code:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Response Code
     - Description
   * - ``200``
     - Success.
   * - ``402``
     - Request Failed. Node is not reachable. A failed response returns one of the following messages:

       Connection timeout:

       .. code-block:: javascript

          {
            "error":"connection timed out",
            "message":"Failed to connect to {destination}, connection timed out."
          }

       Connection refused:

       .. code-block:: javascript

          {
            "error":"connection refused",
            "message":"Failed to connect to {destination}, connection refused."
          }

       Authentication failure:

       .. code-block:: javascript

          {
            "error":"authentication failed",
            "message":"Authentication failed for {destination}"
          }

       Sudo password required:

       .. code-block:: javascript

          {
            "error":"sudo password required",
            "message":"Failed to run commands on {destination}: "+
            "The node is configured to use sudo, but sudo requires a password to run commands."
          }

       Incorrect sudo password:

       .. code-block:: javascript

          {
            "error":"wrong sudo password",
            "message":"Failed to run commands on {destination}: Sudo password is incorrect."
          }

       Cannot use sudo:

       .. code-block:: javascript

          {
            "error":"no sudo",
            "message":"Failed to run commands on {destination}: "+
            "Cannot use sudo, please deactivate it or configure sudo for this user."
          }

GET (compliance)
-----------------------------------------------------
Use to show the compliance state for the named node.

**Request**

.. code-block:: xml

   GET /api/owners/USER/envs/ENV/nodes/NODE_ID/compliance

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/envs/Production/nodes/9b764f79-b96c-4dfa-5a02-9fa3b1abf35b/compliance" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   [
     {
       "failures": 1,
       "impact": 1,
       "log": "Linux kernel parameter \"net.ipv4.tcp_syncookies\" value should eq 1",
       "profileID": "linux",
       "profileOwner": "chef",
       "rule": "chef/linux/sysctl-ipv4-9.2",
       "skipped": false
     },
     {
       "failures": 1,
       "impact": 0.5,
       "log": "Path \"/tmp\" should be mounted",
       "profileID": "linux",
       "profileOwner": "chef",
       "rule": "chef/linux/fs-1",
       "skipped": false
     },
     ...
   ]

GET (patch)
-----------------------------------------------------
Use to show the patch state for the named node.

**Request**

.. code-block:: xml

   GET /api/owners/USER/envs/ENV/nodes/NODE_ID/patches

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/envs/Production/nodes/9b764f79-b96c-4dfa-5a02-9fa3b1abf35b/patches" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   [
     {
       "arch": "amd64",
       "criticality": 0,
       "installedVersion": "2.7.3-0ubuntu3.6",
       "name": "python2.7-minimal",
       "repo": "Ubuntu:12.04/precise-updates",
       "type": "deb",
       "version": "2.7.3-0ubuntu3.8"
     },
     ...
   ]

GET (packages)
-----------------------------------------------------
Use to show the list of installed packages for the named node.

**Request**

.. code-block:: xml

   GET /api/owners/USER/envs/ENV/nodes/NODE_ID/packages

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/envs/Production/nodes/9b764f79-b96c-4dfa-5a02-9fa3b1abf35b/packages" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   [
     {
       "arch": "add",
       "name": "adduser",
       "repo": "",
       "type": "deb",
       "version": "3.113ubuntu2"
     },
     {
       "arch": "commandline",
       "name": "apt",
       "repo": "",
       "type": "deb",
       "version": "0.8.16~exp12ubuntu10.24"
     },
     ...
   ]

/orgs
=====================================================
The ``/orgs`` endpoint has the following methods: ``DELETE``, ``GET`` (for both all organizations or for a single, named organization). ``PATCH``, and ``POST`` .

GET (all organizations)
-----------------------------------------------------
Use to get a list of all organizations.

**Request**

.. code-block:: xml

   GET /api/orgs

For example:

.. code-block:: bash

   curl -X GET "$API_URL/orgs" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   [
     {
       "id": "c89d0a0f-11d6-4b04-7b4d-7e835b4c9551",
       "name": "ACME Corporation",
       "login": "acme"
     }
   ]

GET (named organization)
-----------------------------------------------------
Use to return details about a specific organization.

**Request**

.. code-block:: xml

   GET /api/orgs/ORG

where ``ORG`` is the ``login`` field of the organization.

For example:

.. code-block:: bash

   curl -X GET "$API_URL/orgs/acme" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "id": "c89d0a0f-11d6-4b04-7b4d-7e835b4c9551",
     "name": "ACME Corporation",
     "login": "acme"
   }

PATCH
-----------------------------------------------------
Use to edit the name of an organization.

This method has the following parameters:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Parameter
     - Description
   * - ``name``
     - String. The name of the organization.

**Request**

.. code-block:: xml

   PATCH /api/orgs

For example:

.. code-block:: bash

   curl -w "%{http_code}" -X PATCH "$API_URL/orgs/acme" -H "Content-Type: application/json" \
   -H "$AUTH" -d '{"name":"ACME2 Corporation"}'

**Response**

No Content

POST
-----------------------------------------------------
Use to create an organization.

This method has the following parameters:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Parameter
     - Description
   * - ``name``
     - String. Required. The name of the organization.

**Request**

.. code-block:: xml

   POST /api/orgs

For example:

.. code-block:: bash

   curl -X POST "$API_URL/orgs" -H "Content-Type: application/json" \
   -H "$AUTH" -d '{"name":"ACME Corporation","login":"acme"}'

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "id": "c89d0a0f-11d6-4b04-7b4d-7e835b4c9551",
     "name": "ACME Corporation",
     "login": "acme"
   }

DELETE
-----------------------------------------------------
Use to delete the named organization. The user of this endpoint must have administrative rights.

.. warning:: Deleting an organization will delete all assigned teams, nodes, environments, and scan reports.

**Request**

.. code-block:: xml

   DELETE /api/orgs/ORG

where ``ORG`` is the ``login`` field of the organization.

For example:

.. code-block:: bash

   curl -w "%{http_code}" -X DELETE "$API_URL/orgs/acme" -H "$AUTH"

**Response**

No Content

/scans
=====================================================
The ``/scans`` endpoint has a single method: ``GET`` that may be used to get details for all scans or for a single, named scan.

GET (all scan reports)
-----------------------------------------------------
Use to get a list of all scan reports.

.. note:: All scan reports belong to a named user. Scan reports can be configured to scan nodes from various environments.

**Request**

.. code-block:: xml

   GET /api/owners/USER/scans

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/scans" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   [
     {
       "id": "a74566b9-b527-437f-480f-e56c5b8a1791",
       "owner": "7ae9dd7d-5201-4ae3-4949-60eb4b902e77",
       "start": "2015-05-22T01:10:37.133367688Z",
       "end": "2015-05-22T01:10:42.491573741Z",
       "nodeCount": 1,
       "complianceProfiles": 1,
       "patchlevelProfiles": 1,
       "complianceStatus": 0,
       "patchlevelStatus": 0,
       "unknownStatus": 0,
       "failedCount": 0
     }
   ]

GET (named scan report)
-----------------------------------------------------
Use to return details about the named scan report.

**Request**

.. code-block:: xml

   GET /api/owners/USER/scans/SCAN_ID

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/scans/SCAN_ID" -H "$AUTH"

where ``SCAN_ID`` is similar to ``90def607-1688-40f5-5a4c-161c51fd8aac``.

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "id": "a74566b9-b527-437f-480f-e56c5b8a1791",
     "owner": "john",
     "start": "2015-05-22T01:10:37.133367688Z",
     "end": "2015-05-22T01:10:42.491573741Z",
     "nodeCount": 1,
     "complianceProfiles": 1,
     "patchlevelProfiles": 1,
     "complianceStatus": 0,
     "patchlevelStatus": 0,
     "unknownStatus": 0,
     "failedCount": 0,
     "complianceSummary": {
       "success": 0,
       "minor": 0,
       "major": 43,
       "critical": 2,
       "skipped": 0,
       "total": 45
     },
     "patchlevelSummary": {
     "success": 0,
     "minor": 0,
     "major": 0,
     "critical": 0,
     "unknown": 0,
     "total": 0
     }
   }

It contains the following attributes:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Parameter
     - Description
   * - ``critical``
     - Float. The number of failed rules.
   * - ``end``
     - ISO date. The time at which a scan report ended.
   * - ``id``
     - UUID. The scan report identifier.
   * - ``major``
     - Float. The number of rules that contain major errors.
   * - ``minor``
     - Float. The number of rules that contain minor errors.
   * - ``nodeCount``
     - Integer. The number of nodes that were tested.
   * - ``failedCount``
     - Integer. The number of nodes that were failed to be tested.
   * - ``owner``
     - String. The owner of the scan.
   * - ``skipped``
     - Float. The number of nodes with skipped rules.
   * - ``start``
     - ISO date. The time at which a scan report started.
   * - ``success``
     - Float. The number of successful rules.

POST
-----------------------------------------------------
Use to create a new scan.

This method has the following parameters:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Parameter
     - Description
   * - ``compliance``
     - An array of selected profiles.
   * - ``environments``
     - An array of environments and selected nodes.

**Request**

.. code-block:: xml

   POST /api/owners/USER/scans

with a |json| object similar to:

.. code-block:: javascript

   {
     "compliance": [{
       "owner": "chef",
       "profile": "linux"
     },{
       "owner": "chef",
       "profile": "ssh"
     }],
     "environments": [{
       "id": "b771e025-6445-4ead-5cac-b466ea725177",
       "nodes": ["b771e025-6445-4ead-5cac-b466ea725177"]
     }],
     "patchlevel": [{
       "profile" : "default"
       }]
   }

For example:

.. code-block:: bash

   curl -X POST "$API_URL/owners/john/scans" \
   -H "Content-Type: application/json" -H "$AUTH" -d '{ JSON_BLOCK }'

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "id": "57130678-1a1f-405d-70bf-fe570a25621e"
   }

DELETE (single scan)
-----------------------------------------------------
Delete one scan specified in the URL. If this is the most recent scan of a node, the node will be marked as never scanned.

**Request**

.. code-block:: xml

 DELETE /api/owners/USER/scans/SCAN_ID

For example:

.. code-block:: bash

 curl -w "%{http_code}" -X DELETE "$API_URL/owners/john/scans/57130678-1a1f-405d-70bf-fe570a25621e" -H "$AUTH"

**Response**

No Content


DELETE (bulk)
-----------------------------------------------------
Delete one or multiple scans specified in the payload of the request.

**Request**

.. code-block:: xml

  DELETE /api/owners/USER/scans

with a |json| array of scan ids:

.. code-block:: javascript

 [
   "57130678-1a1f-405d-70bf-fe570a25621e",
   "90def607-1688-40f5-5a4c-161c51fd8aac"
 ]

For example:

.. code-block:: bash

  curl -w "%{http_code}" -X DELETE "$API_URL/owners/john/scans" -H "$AUTH" \
  -d '["57130678-1a1f-405d-70bf-fe570a25621e","90def607-1688-40f5-5a4c-161c51fd8aac"]'

**Response**

No Content

/scans/SCAN_ID/rules
=====================================================
The ``/scans/SCAN_ID/rules`` endpoint has the following methods: ``GET``.

GET (named scan)
-----------------------------------------------------
Use to get the executed compliance rules for the named scan.

**Request**

.. code-block:: xml

   GET /api/owners/USER/scans/SCAN_ID/rules

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/scans/SCAN_ID/rules" -H "$AUTH"

where ``SCAN_ID`` is similar to ``90def607-1688-40f5-5a4c-161c51fd8aac``.

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "chef": {
       "linux": {
         "chef/linux/basic-1": {
           "log": "",
           "complianceStatus": 1,
           "unknownStatus": 0
         },
         "chef/linux/fs-1": {
           "log": "",
           "complianceStatus": 0.5,
           "unknownStatus": 0
         },
       ...
     }
   }

It contains the following attributes:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Parameter
     - Description
   * - ``complianceStatus``
     - Integer. The Common Vulnerability Scoring System (CVSS) range, `a measurement of the level of concern for a vulnerability <https://en.wikipedia.org/wiki/CVSS>`__, as compared to other vulnerabilities. Scores range from ``0.0`` to ``10.0``. High scores are in the 7.0-10.0 range, medium scores are in the 4.0-6.9 range, and low scores are from 0.0-3.9 range.
   * - ``log``
     - String. The rule description.

/scans/SCAN_ID/nodes
=====================================================
The ``/scans/SCAN_ID/nodes`` endpoint has a single method: ``GET``.

GET (all nodes)
-----------------------------------------------------
Use to get all scans for all nodes.

**Request**

.. code-block:: xml

   GET /api/owners/USER/scans/SCAN_ID/nodes

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/scans/SCAN_ID/nodes" -H "$AUTH"

where ``SCAN_ID`` is similar to ``90def607-1688-40f5-5a4c-161c51fd8aac``.

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   [
     {
       "environment": "b771e025-6445-4ead-5cac-b466ea725177",
       "node": "192.168.59.107:11024",
       "complianceStatus": 0,
       "patchlevelStatus": -1,
       "unknownStatus": 0,
       "arch": "",
       "family": "",
       "release": "",
       "connectSuccess": false,
       "connectMessage": "Failed to verify connectivity to sshPassword://root@192.168.56.239:0 using login password : exit status 1",
       "complianceSummary": {
         "success": 0,
         "minor": 0,
         "major": 43,
         "critical": 2,
         "skipped": 0,
         "total": 45
       },
       "patchlevelSummary": {
         "success": 0,
         "minor": 0,
         "major": 0,
         "critical": 0,
         "unknown": 0,
         "total": 0
       },
       "patchStatus": null
     }
   ]

/scans/SCAN_ID/envs/ENV
=====================================================
The ``/scans/SCAN_ID/envs/ENV`` endpoint has a single method: ``GET`` that may be used to get compliance, patch, or package details by node.

GET (compliance by node)
-----------------------------------------------------
Use to get the compliance results for the named node and the named environment.

**Request**

.. code-block:: xml

   GET /api/owners/USER/scans/SCAN_ID/envs/ENV/nodes/NODE_ID/compliance

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/scans/SCAN_ID/envs/Production/nodes/NODE_ID/compliance" -H "$AUTH"

where ``SCAN_ID`` is similar to ``90def607-1688-40f5-5a4c-161c51fd8aac``
and ``NODE_ID`` is similar to ``9b764f79-b96c-4dfa-5a02-9fa3b1abf35b``

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   [
     {
       "profileOwner": "chef",
       "profileID": "linux",
       "rule": "chef/linux/basic-1",
       "impact": 1,
       "failures": 1,
       "skipped": false,
       "log": "Path \"/etc/ssh\" should be directory"
     },
     ...
   ]

It contains the following attributes:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Parameter
     - Description
   * - ``failures``
     - Integer. The amount of failures per rule. Use ``-1`` to skip and ``0`` for no failures.
   * - ``impact``
     - Float. The impact of the compliance results. Must be a value between ``0`` and ``1``.
   * - ``log``
     - String. The error log.
   * - ``profileID``
     - String. The compliance rules identifier.
   * - ``profileOwner``
     - String. The owner of the compliance rules.
   * - ``rule``
     - String. The rule identifier.

GET (patches by node)
-----------------------------------------------------
Use to get the available patches for the named node and the named environment.

**Request**

.. code-block:: xml

   GET /api/owners/USER/scans/SCAN_ID/envs/ENV/nodes/NODE_ID/patches

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/scans/SCAN_ID/envs/Production/nodes/NODE_ID/patches" -H "$AUTH"

where ``SCAN_ID`` is similar to ``90def607-1688-40f5-5a4c-161c51fd8aac``
and ``NODE_ID`` is similar to ``9b764f79-b96c-4dfa-5a02-9fa3b1abf35b``

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   [
     {
       "arch": "amd64",
       "criticality": 0,
       "installedVersion": "2.7.3-0ubuntu3.6",
       "name": "python2.7-minimal",
       "repo": "Ubuntu:12.04/precise-updates",
       "type": "deb",
       "version": "2.7.3-0ubuntu3.8"
     }
     ...
   ]

It contains the following attributes:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Parameter
     - Description
   * - ``arch``
     - String. The CPU architecture.
   * - ``criticality``
     - Integer. The Common Vulnerability Scoring System (CVSS) range, `a measurement of the level of concern for a vulnerability <https://en.wikipedia.org/wiki/CVSS>`__, as compared to other vulnerabilities. Scores range from ``0.0`` to ``10.0``. High scores are in the 7.0-10.0 range, medium scores are in the 4.0-6.9 range, and low scores are from 0.0-3.9 range.
   * - ``name``
     - String. The name of the package.
   * - ``repo``
     - String. The package repository.
   * - ``version``
     - String. The package version.

GET (packages by node)
-----------------------------------------------------
Use to get the installed packages for the named node and the named environment.

**Request**

.. code-block:: xml

   GET /api/owners/USER/scans/SCAN_ID/envs/ENV/nodes/NODE_ID/packages

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/scans/SCAN_ID/envs/Production/nodes/NODE_ID/packages" -H "$AUTH"

where ``SCAN_ID`` is similar to ``90def607-1688-40f5-5a4c-161c51fd8aac``
and ``NODE_ID`` is similar to ``9b764f79-b96c-4dfa-5a02-9fa3b1abf35b``

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   [
     {
       "arch": "add",
       "name": "adduser",
       "repo": "",
       "type": "deb",
       "version": "3.113ubuntu2"
     },
     {
       "arch": "commandline",
       "name": "apt",
       "repo": "",
       "type": "deb",
       "version": "0.8.16~exp12ubuntu10.24"
     },
     ...
   ]

/server/config
=====================================================
The ``/server/config`` endpoint has the following methods: ``GET`` and ``PATCH``.

.. note:: Some parameters of the |chef compliance| server are exposed and are configurable from the |api compliance|.

GET
-----------------------------------------------------
Use to return the global configuration for the |chef compliance| server. The configuration may be edited via the |api compliance| or by using the COMPLIANCE_CONFIG_FILE. Only parameters that may be safely tuned are exposed. All timeout configuration settings are defined in seconds, i.e. ``1800`` is ``30 minutes``.

**Request**

.. code-block:: xml

   GET /api/server/config

For example:

.. code-block:: bash

   curl -X GET "$API_URL/server/config" \
   -H "Content-Type: application/json" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
    "port": null,
    "host": null,
    "colors": null,
    "detectTimeout": 30,
    "scanTimeout": 1800,
    "updateTimeout": 1800,
    "home": null,
    "licensedNodeCount": 25
   }

PATCH
-----------------------------------------------------
Use to edit the global configuration for the |chef compliance| server.

**Request**

.. code-block:: xml

   PATCH /api/server/config

For example:

.. code-block:: bash

   curl -w "%{http_code}" -X PATCH "$API_URL/server/config" \
   -H "Content-Type: application/json" -H "$AUTH" -d '{ JSON_BLOCK }'

**Response**

No Content

/summary
=====================================================
The ``/summary`` endpoint has the following method: ``GET``.

GET
-----------------------------------------------------
Get a quick summary(number of nodes and environments) of the account.

**Request**

.. code-block:: xml

   GET /api/owners/OWNER/summary

For example:

.. code-block:: bash

   curl -X GET "$API_URL/owners/john/summary" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "nodeCount": 28,
     "envCount": 6
   }

/teams
=====================================================
The ``/teams`` endpoint has the following methods: ``DELETE``, ``GET`` (for both all teams or for a single, named team). ``PATCH``, and ``POST``.

GET (all teams)
-----------------------------------------------------
Use to get a list of all teams. Each organization has a ``owners`` team, by default.

**Request**

.. code-block:: xml

   GET /api/orgs/ORG/teams

For example:

.. code-block:: bash

   curl -X GET "$API_URL/orgs/acme/teams" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   [
     {
       "id": "owners",
       "org": "843cd9cd-86d8-40d2-5d8a-a48dc7690a69",
       "name": "Owners"
     },
     {
       "id": "20aff993-3288-426d-6851-d1d47bb40d80",
       "org": "843cd9cd-86d8-40d2-5d8a-a48dc7690a69",
       "name": "audit"
     },
   ]

GET (named team)
-----------------------------------------------------
Use to return details about a specific team.

**Request**

.. code-block:: xml

   GET /api/orgs/ORG/teams/TEAM_ID

For example:

.. code-block:: bash

   curl -X GET "$API_URL/orgs/acme/teams/owners" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "id": "owners",
     "org": "843cd9cd-86d8-40d2-5d8a-a48dc7690a69",
     "name": "Owners",
     "members": [
       "fd500af8-4e30-4e67-7bbd-1287f23af209"
     ],
     "permissions": {
       "harden": "true",
       "manage": "true",
       "patch": "true",
       "scan": "true"
     }
   }

PATCH
-----------------------------------------------------
Use to edit the details for a team that belongs to the named organization.

This method has the following parameters:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Parameter
     - Description
   * - ``name``
     - Required. The name of the user.
   * - ``permissions``
     - Object. The permissions to assign to the team: ``harden``, ``manage``, ``patch``, or ``scan``.

**Request**

.. code-block:: xml

   PATCH /api/orgs/ORG/teams/TEAM_ID

For example:

.. code-block:: bash

   curl -w "%{http_code}" -X PATCH "$API_URL/orgs/acme/teams/TEAM_ID" \
   -H "Content-Type: application/json" -H "$AUTH" -d '{ JSON_BLOCK }'

where ``TEAM_ID`` is similar to ``20aff993-3288-426d-6851-d1d47bb40d80``

**Response**

No Content

POST
-----------------------------------------------------
Use to create a new team within the named organization.

This method has the following parameters:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Parameter
     - Description
   * - ``name``
     - Required. The name of the user.
   * - ``permissions``
     - Object. The permissions to assign to the team: ``harden``, ``manage``, ``patch``, or ``scan``.

**Request**

.. code-block:: xml

   POST /api/orgs/ORG/teams

For example:

.. code-block:: bash

   curl -X POST "$API_URL/orgs/acme/teams" \
   -H "Content-Type: application/json" -H "$AUTH" \
   -d '{"name":"manageteam","permissions":{"manage":"true"}}'

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "org":"843cd9cd-86d8-40d2-5d8a-a48dc7690a69",
     "name":"manageteam",
     "permissions":{"manage":"true"},
     "id":"55ace94c-f873-45d1-48da-e278bbe595b0"
   }

DELETE
-----------------------------------------------------
Use to delete a team from the named organization.

.. warning:: The ``owners`` team cannot be deleted.

**Request**

.. code-block:: xml

   DELETE /api/orgs/ORG/teams/TEAM_ID

For example:

.. code-block:: bash

   curl -w "%{http_code}" -X DELETE "$API_URL/orgs/acme/teams/TEAM_ID" -H "$AUTH"

where ``TEAM_ID`` is similar to ``20aff993-3288-426d-6851-d1d47bb40d80``

**Response**

No Content

/teams/TEAM_ID/members
=====================================================
The ``/teams/TEAM_ID/members`` endpoint has the following methods: ``DELETE``, ``GET``, ``PATCH``, and ``POST``.

DELETE
-----------------------------------------------------
Use to delete a team member.

**Request**

.. code-block:: xml

   DELETE /api/orgs/ORG/teams/TEAM_ID/members/MEMBER_ID

For example:

.. code-block:: bash

   curl -w "%{http_code}" -X DELETE "$API_URL/orgs/acme/teams/TEAM_ID/members/MEMBER_ID" -H "$AUTH"

where ``TEAM_ID`` is similar to ``20aff993-3288-426d-6851-d1d47bb40d80``
and ``MEMBER_ID`` is similar to ``7ae9dd7d-5201-4ae3-4949-60eb4b902e77``

**Response**

No Content

GET
-----------------------------------------------------
Use to get a list of team memberships.

**Request**

.. code-block:: xml

   GET /api/orgs/ORG/teams/TEAM_ID/members

For example:

.. code-block:: bash

   curl -X GET "$API_URL/orgs/acme/teams/TEAM_ID/members" -H "$AUTH"

where ``TEAM_ID`` is similar to ``20aff993-3288-426d-6851-d1d47bb40d80``

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "id": "20aff993-3288-426d-6851-d1d47bb40d80",
     "org": "843cd9cd-86d8-40d2-5d8a-a48dc7690a69",
     "name": "audit",
     "members": [
       "fd500af8-4e30-4e67-7bbd-1287f23af209"
     ],
     "permissions": {
       "harden": "true",
       "manage": "true",
       "patch": "true",
       "scan": "true"
     }
   }

PATCH
-----------------------------------------------------
Use to edit team membership details for the named team member.

**Request**

.. code-block:: xml

   PATCH /api/orgs/ORG/teams/TEAM_ID/members/MEMBER_ID

For example:

.. code-block:: bash

   curl -w "%{http_code}" -X PATCH "$API_URL/orgs/acme/teams/TEAM_ID" \
   -H "Content-Type: application/json" -H "$AUTH" -d '{ JSON_BLOCK }'

where ``TEAM_ID`` is similar to ``20aff993-3288-426d-6851-d1d47bb40d80``

**Response**

No Content

POST
-----------------------------------------------------
Use to add one (or more) a members to the named team.

This method has the following parameters:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Parameter
     - Description
   * - ``users``
     - Required. An array of user identifiers. Full |json| example: '{["bob","mary"]}'

**Request**

.. code-block:: xml

   POST /api/orgs/ORG/teams/TEAM_ID/members

For example:

.. code-block:: bash

   curl -X POST "$API_URL/orgs/acme/teams/TEAM_ID/members" \
   -H "Content-Type: application/json" -H "$AUTH" -d '{["bob"]}'

where ``TEAM_ID`` is similar to ``20aff993-3288-426d-6851-d1d47bb40d80``

**Response**

No Content

/users
=====================================================
The ``/users`` endpoint has a single method: ``GET`` that may be used to get details for all users or for a single, named user.

GET (all users)
-----------------------------------------------------
Use to get a list of all users with their IDs

**Request**

.. code-block:: xml

   GET /api/users

For example:

.. code-block:: bash

   curl -X GET "$API_URL/users" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   [
     {
       "name":"John Doe",
       "login":"john",
       "id":"2538ac60-4238-4622-69cf-64cc0eea2ae5"
     },
     {
       "name":"Jane Doe",
       "login":"jane-doe",
       "id":"f3a5c286-d4d4-4860-63b0-5dbfb58e5e69"
     }
   ]


GET (named user)
-----------------------------------------------------
Use to return details about the named user.

**Request**

.. code-block:: xml

   GET /api/users/USER

For example:

.. code-block:: bash

   curl -X GET "$API_URL/users/john" -H "$AUTH"

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "name":"John Doe",
     "login":"john",
     "id": "2538ac60-4238-4622-69cf-64cc0eea2ae5",
     "preferences": null,
     "permissions": {
       "org_admin":"true",
       "site_admin":"true",
       "user_admin":"true"
     }
   }

POST
-----------------------------------------------------
Use to create a new user.

This method has the following parameters:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Parameter
     - Description
   * - ``name``
     - String. The name of the user.
   * - ``password``
     - String. The unencrypted password for the user.
   * - ``preferences``
     - Hash. Not implemented yet.
   * - ``permissions``
     - Hash. User permissions, for example ``{"org_admin":"true","site_admin":"true","user_admin":"true"}``

**Request**

.. code-block:: xml

   POST /api/users

For example:

.. code-block:: bash

   curl -X POST "$API_URL/users" \
   -H "Content-Type: application/json" -H "$AUTH" -d '{ "name":"Lee Doe", "login":"lee","password":"l8dDnwr-0fgh" }'

**Response**

The response will return a |json| object similar to:

.. code-block:: javascript

   {
     "id":"9296dce4-007f-4f34-42d4-bf8aa5f25d50"
   }

PATCH
-----------------------------------------------------
Use to edit the details for an existing user.

This method has the following parameters:

.. list-table::
   :widths: 200 300
   :header-rows: 1

   * - Parameter
     - Description
   * - ``name``
     - String. The name of the user.
   * - ``password``
     - String. The unencrypted password for the user.

**Request**

.. code-block:: xml

   PATCH /api/users/USER

For example:

.. code-block:: bash

   curl -w "%{http_code}" -X PATCH "$API_URL/users/john" \
   -H "Content-Type: application/json" -H "$AUTH" -d '{ "name":"Sir. Lee Smith" }'


**Response**

No content is returned by this endpoint. That's why the example above uses `-w "%{http_code}"` in order to show the response http code(i.e. 200 for success)

DELETE
-----------------------------------------------------
Use to delete an existing user.

**Request**

.. code-block:: xml

   DELETE /api/users/USER

For example:

.. code-block:: bash

   curl -w "%{http_code}" -X DELETE "$API_URL/users/john" -H "$AUTH"

**Response**

No Content

.. |api compliance| replace:: Compliance API
.. |chef compliance| replace:: Chef Compliance
.. |json| replace:: JSON
.. |tar gz| replace:: tar.gz
.. |zip| replace:: Zip
.. |bash| replace:: Bash
.. |open ssh| replace:: OpenSSH
.. |company_name| replace:: Chef
.. |windows| replace:: Microsoft Windows
.. |mac os x| replace:: Mac OS X
