package nats

import (
	"context"
	"crypto/rand"
	"crypto/subtle"
	"encoding/base64"
	"fmt"
	"io/ioutil"
	"time"

	natsd "github.com/nats-io/gnatsd/server"
	"github.com/pkg/errors"
	log "github.com/sirupsen/logrus"
	"google.golang.org/grpc"
	"google.golang.org/grpc/codes"
	"google.golang.org/grpc/metadata"
	"google.golang.org/grpc/status"

	"github.com/chef/automate/api/interservice/authn"
	authzV1 "github.com/chef/automate/api/interservice/authz"
	authz "github.com/chef/automate/api/interservice/authz/v2"
	"github.com/chef/automate/components/event-gateway/pkg/config"

	"github.com/chef/automate/lib/grpc/secureconn"
)

// Auth Policy
// This auth policy is copied from gateway/datacollector.go
// TODO: (dan) update this with applications resources/actions once we make them exist
const (
	resourceV1 = "ingest:unified_events"
	actionV1   = "create"
	resourceV2 = "infra:ingest:unifiedEvents"
	actionV2   = "infra:ingest:create"
)

const authzClientTimeout = 60 * time.Second

const PRNCount = 20

const HealthCheckTokenPrefix = "HEALTHCHECK-"

func GenerateHealthCheckCredentials(c *config.EventGatewayConfig) error {
	b := make([]byte, PRNCount)
	_, err := rand.Read(b)
	if err != nil {
		return errors.Wrap(err, "failed reading random data to generate healthcheck credentials")
	}
	hcToken := fmt.Sprintf("%s%s", HealthCheckTokenPrefix, base64.URLEncoding.EncodeToString(b))

	err = ioutil.WriteFile(c.Service.HealthCheckCredentialsFile, []byte(hcToken), 0600)
	if err != nil {
		return errors.Wrapf(err, "failed to write healthcheck credentials to %q", c.Service.HealthCheckCredentialsFile)
	}

	return nil
}

func ReadHealthCheckCredentials(c *config.EventGatewayConfig) (string, error) {
	data, err := ioutil.ReadFile(c.Service.HealthCheckCredentialsFile)
	if err != nil {
		return "", errors.Wrapf(err, "failed to read healthcheck credentials from %q", c.Service.HealthCheckCredentialsFile)
	}
	return string(data), nil
}

type automateAuthenticator struct {
	authnClient      authn.AuthenticationClient
	authzClient      authz.AuthorizationClient
	authzV1Client    authzV1.AuthorizationClient
	healthCheckToken string
}

func newAutomateAuthenticator(c *config.EventGatewayConfig) (*automateAuthenticator, error) {
	endpoints := c.Auth

	serviceCerts, err := c.TLSConfig.ReadCerts()
	if err != nil {
		return nil, err
	}

	connFactory := secureconn.NewFactory(*serviceCerts)

	healthCheckToken, err := ReadHealthCheckCredentials(c)
	if err != nil {
		return nil, err
	}

	authnConn, err := dialgRPC(connFactory, "authn-service", endpoints.AuthnEndpoint)
	if err != nil {
		return nil, err
	}
	authzConn, err := dialgRPC(connFactory, "authz-service", endpoints.AuthzEndpoint)
	if err != nil {
		return nil, err
	}

	return &automateAuthenticator{
		authnClient:      authn.NewAuthenticationClient(authnConn),
		authzClient:      authz.NewAuthorizationClient(authzConn),
		authzV1Client:    authzV1.NewAuthorizationClient(authzConn),
		healthCheckToken: healthCheckToken,
	}, nil
}

// Check is a callback function that NATS will call to authenticate connection
// requests. We use this behavior to integrate Automate's token authentication
// with NATS. We do the following:
// * check for a special health check token that the event gateway creates.
//   This token is (re-)generated on service start and is used by the hab
//   health check. If the token given is the health check token, give the
//   connection just the permissions needed to run the health check.
// * check the token with the authn service and then the authz service. The
//   token has to be valid and have the correct ingest permissions to pass.
// * If the token is valid, assign it a NATS user object. This sets NATS-level
//   authorization on the connection to limit the topics the client can
//   publish/subscribe to.
func (a *automateAuthenticator) Check(client natsd.ClientAuthentication) bool {
	log.WithFields(log.Fields{"client": client.RemoteAddress()}).Debug("authenticating NATS connection request")
	token := client.GetOpts().Authorization

	// use ConstantTimeCompare so as not to leak the healthCheckToken via timing attacks.
	if subtle.ConstantTimeCompare([]byte(token), []byte(a.healthCheckToken)) == 1 {
		client.RegisterUser(healthCheckNATSUser())
		log.WithFields(log.Fields{"client": client.RemoteAddress()}).Debug("successfully authenticated NATS connection for healthcheck")
		return true
	}

	err := a.checkToken(token)
	if err == nil { // success
		client.RegisterUser(habNatsUser())
		log.WithFields(log.Fields{"client": client.RemoteAddress()}).Debug("successfully authenticated NATS connection request")
		return true
	}
	log.WithError(err).WithFields(log.Fields{
		"client_address": client.RemoteAddress(),
	}).Error("authentication or authorization failure connecting to NATS gateway")
	return false
}

// habNatsUser makes a *natsd.User struct that we can pass to the
// natsd.ClientAuthentication.RegisterUser() method. We use this RegisterUser
// callback to set NATS-level authorization permissions on clients that have
// presented a valid Automate token. This is intended to restrict the
// privileges of clients and reduce the damage a malicious actor could inflect
// if they came to possess a valid ingestion API token.
//
// Note that the API that NATS provides for us to do this requires us to pass a
// natsd.User struct which contains fields for username and password, and we
// cannot set these fields to nil because they're not pointer fields. For
// paranoia reasons, we set the password to a randomly generated string, but in
// the current NATS implementation this user and password can never be used to
// log in, as password auth is disabled due to the presence of the custom
// authenticator.
func habNatsUser() *natsd.User {
	habClient := &natsd.User{
		// Username and Password should be completely ignored by NATS, as we are
		// only using this data structure for the authorization part, but we assign
		// a big random password just in case some future change in NATS modifies
		// this behavior.
		Username: "hab_client",
		Password: randomPassword(),
		Permissions: &natsd.Permissions{
			// Here is trace logging from events service for a single client. All of
			// these must be permitted. When something is denied, NATS will log a message like
			//
			// 		Publish Violation - User "N/A", Subject "_STAN.discover.event-service"
			//
			// [SUB _INBOX.Uhj3xS5sDhxFIA3ariaaZ0  1]
			// [SUB _INBOX.Uhj3xS5sDhxFIA3ariaabC  2]
			// [SUB _INBOX.Uhj3xS5sDhxFIA3ariaadO.*  3]
			// [PUB _STAN.discover.event-service _INBOX.Uhj3xS5sDhxFIA3ariaadO.BX2rAaIO 85]
			// [MSG _INBOX.Uhj3xS5sDhxFIA3ariaadO.BX2rAaIO 3 221]
			// [SUB _STAN.acks.Uhj3xS5sDhxFIA3ariaafa  4]
			// [PUB _STAN.pub.UviFyS74SqJCUHZK2RM9Ho.habitat _STAN.acks.Uhj3xS5sDhxFIA3ariaafa 156]
			// [MSG _STAN.acks.Uhj3xS5sDhxFIA3ariaafa 4 24]
			// [PUB _STAN.close.UviFyS74SqJCUHZK2RM9Ho _INBOX.Uhj3xS5sDhxFIA3ariaadO.5MoxIlIy 24]
			// [MSG _INBOX.Uhj3xS5sDhxFIA3ariaadO.5MoxIlIy 3 0]
			Publish: &natsd.SubjectPermission{
				Allow: []string{
					"_INBOX.>",
					"_HB.>",
					"_STAN.discover.event-service",
					"_STAN.discover.event-service.*",
					"_STAN.pub.*.habitat",
					"_STAN.close.*",
				},
			},
			Subscribe: &natsd.SubjectPermission{
				Allow: []string{
					"_INBOX.>",
					"_STAN.acks.>",
					"_HB.>",
					"_STAN.discover.event-service.*",
					">",
				},
			},
		},
	}

	return habClient
}

func healthCheckNATSUser() *natsd.User {
	habClient := &natsd.User{
		Username: "health_check_client",
		Password: randomPassword(),
		Permissions: &natsd.Permissions{
			Publish: &natsd.SubjectPermission{
				Allow: []string{
					"_INBOX.>",
					"healthcheck.>",
				},
			},
			Subscribe: &natsd.SubjectPermission{
				Allow: []string{
					"_INBOX.>",
					"healthcheck.>",
				},
			},
		},
	}

	return habClient
}

// according to the nats server code, all other authentication methods are
// disabled when custom auth is enabled.
// also, this user is attached to the connection after auth and shouldn't be
// propagated to where nats would look at it during auth.
// despite all that, generate a random password anyway.
func randomPassword() string {
	pwbytes := [64]byte{}
	_, err := rand.Read(pwbytes[:])
	if err != nil {
		panic(err)
	}
	return string(pwbytes[:64])
}

// checkToken does authentication authorization checks for the given token. It
// returns an error if the token is invalid (authn failure) or does not have
// permissision to create ingest events (authorization failure) or if an
// internal system fails during either of those checks.
func (a *automateAuthenticator) checkToken(token string) error {

	// In the gateway, AuthN works by copying relevant headers from the upstream
	// request to a new request to the authn service. The headers we need to care
	// about are the auth-related ones:
	// "X-Data-Collector-Token", "Api-Token", "X-Client-Cert"
	// "Api-Token" is what we're using in code samples and such, we'll use that
	// one when we craft our request:
	md := metadata.Pairs("Api-Token", token)
	// 60s timeout is chosen to match automate load balancer/gateway
	timeout, cancel := context.WithTimeout(context.Background(), authzClientTimeout)
	defer cancel()

	ctx := metadata.NewOutgoingContext(timeout, md)

	authnResp, err := a.authnClient.Authenticate(ctx, &authn.AuthenticateRequest{})
	if err != nil {
		return errors.Wrap(err, "authn-service error attempting to authenticate token for NATS connection")
	}

	var subjects []string
	subjects = append(authnResp.Teams, authnResp.Subject)

	if len(subjects) < 1 {
		return errors.New("token did not resolve to an authorization subject after authentication request")
	}
	err, tryV1 := a.tryAuthzV2(ctx, subjects)
	if err == nil {
		return nil
	}
	if !tryV1 {
		return err
	}

	v1err, _ := a.tryAuthzV1(ctx, subjects)
	return v1err
}

func (a *automateAuthenticator) tryAuthzV2(ctx context.Context, subjects []string) (err error, shouldTryV1 bool) {
	resp, err := a.authzClient.IsAuthorized(ctx, &authz.IsAuthorizedReq{
		Subjects: subjects,
		Resource: resourceV2,
		Action:   actionV2,
	})
	if err != nil {
		// FailedPrecondition will happen for an IAM v2 request when IAM v2 is not enabled.
		if status.Convert(err).Code() == codes.FailedPrecondition {
			return err, true
		}
		return errors.Wrapf(err, "error authorizing action %q on resource %q for subjects %q", actionV2, resourceV2, subjects), false
	}
	if !resp.GetAuthorized() {
		return errors.Errorf("unauthorized action %q on resource %q for subjects %q", actionV2, resourceV2, subjects), false
	}
	return nil, false
}

func (a *automateAuthenticator) tryAuthzV1(ctx context.Context, subjects []string) (err error, shouldTryV2 bool) {
	resp, err := a.authzV1Client.IsAuthorized(ctx, &authzV1.IsAuthorizedReq{
		Subjects: subjects,
		Resource: resourceV1,
		Action:   actionV1,
	})
	if err != nil {
		// FailedPrecondition will happen for an IAM v1 request when IAM v2 is enabled.
		if status.Convert(err).Code() == codes.FailedPrecondition {
			return errors.Wrap(err, "IAMv1 not available, try IAMv2"), true
		}
		log.WithError(err).Error("error authorizing request")
		return errors.Wrapf(err, "error authorizing action %q on resource %q for subjects %q", actionV1, resourceV1, subjects), false
	}
	if resp.GetAuthorized() {
		return nil, false
	}
	return errors.Errorf("unauthorized action %q on resource %q for subjects %q", actionV1, resourceV1, subjects), false
}

func dialgRPC(connFactory *secureconn.Factory, service string, endpointTarget string) (*grpc.ClientConn, error) {
	log.WithFields(log.Fields{
		"service":  service,
		"endpoint": endpointTarget,
	}).Info("Dialing")

	opts := []grpc.DialOption{}
	conn, err := connFactory.Dial(service, endpointTarget, opts...)

	if err != nil {
		// This case should never happen (unless you tell Dial to block), but
		// handle it just in case
		log.WithFields(log.Fields{
			"service": service,
			"error":   err,
		}).Fatal("Could not create connection")
	}
	return conn, nil
}
