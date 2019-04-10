# Event Gateway

Event Gateway is the external-facing NATS instance that allows
external clients to submit messages to Automate. Currently it is used by
habitat clients to send applications-related data to the applications service.

Much like Automate Gateway does for HTTP protocol clients, Automate Event
Gateway handles authentication and authorization for NATS protocol clients.

Our NATS architecture looks like this:

```

                ------------       ------------------
hab client -----| NATS (gw) |----- | NATS (internal)|---- domain services
                -------------      ------------------
                      |                    | |
                      |                    | |
                      |              ----------------
                --------------       |NATS STREAMING|
                | A2 AuthN/Z |       |is a client of|
                --------------       |NATS          |
                                     ----------------
```

We have the following requirements for external clients:
* "normal" TLS, i.e., the hostname is the CN, no client certs required, etc.
* uses existing Automate authentication/authorization mechanisms (at this time
  that means tokens which get verified by auth-service)

For internal clients:
* authenticate with each other using mutual TLS; clients must provide
  certificates
* expect the CN of the certificate to match the service name (e.g.,
  "event-service")

Research showed that it would be extremely difficult or impossible to meet all
of these requirements with just one instance of NATS for various reasons,
therefore we have one for each set of clients. The two NATS instances
communicate on a separate NATS gateway protocol that runs on a different port.
Connections to the gateway port are secured with mTLS.

## Authentication

Event Gateway only supports tokens. External NATS clients should
configure an authentication token in the manner supported by the NATS library
used. When using the go client, the token may be encoded in the URL, like
`nats://TOKEN_HERE@host.example:4222`.

## Automate Authorization

The design of NATS requires that Automate-level authorization is checked when
a client attempts to connect. We must also set NATS-level authorization
permissions at this time (see below). This means that we must determine the set
of authorized actions for clients ahead of time, instead of when an action is
attempted.

It is assumed that the only purpose for external clients to connect to this
service is to submit data for ingest, therefore incoming connection requests
are checked for authorization on a hard-coded set of ingest-related
permissions.

## NATS Authorization

As described above, permissions to actions on NATS resources (i.e., publishing
or subcribing to a topic) must be set at connection time. Currently we assume
that clients are only connecting for the purpose of publishing applications
(hab) data for consumption by the applications-service. We therefore restrict
clients to the set of publish/subscribe permissions necessary for publishing to
the relevant topics via the NATS-streaming protocol.

