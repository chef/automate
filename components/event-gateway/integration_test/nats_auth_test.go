// +build integration

package integration_test

import (
	"context"
	"crypto/tls"
	"fmt"
	"strconv"
	"testing"
	"time"

	"github.com/gofrs/uuid"
	natsc "github.com/nats-io/nats.go"
	stan "github.com/nats-io/stan.go"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"

	"github.com/chef/automate/api/interservice/authn"
	policies "github.com/chef/automate/api/interservice/authz"
	"github.com/chef/automate/lib/grpc/auth_context"
)

////////////////////////////////////////////////////////////////////////////////
// Authorization-Specific suite initialization
////////////////////////////////////////////////////////////////////////////////

// initNATSAuthTest deletes a built-in authorization policy that allows all
// tokens permission to the ingest endpoints/actions. This is needed to test
// that authorization (as opposed to authentication) can actually fail
// (otherwise the default policy would always give permissions so auth would
// succeed). We stash the policy on the Suite and restore it in the teardown
func (s *Suite) initNATSAuthTest() error {
	err := s.initAuthzClient()
	if err != nil {
		return err
	}

	return nil
}

// teardownNATSAuthTest re-creates any auth policies that were deleted in the
// initNATSAuthTest step.
func (s *Suite) teardownNATSAuthTest() {
	defer s.authzConn.Close()
}

func (s *Suite) initAuthzClient() error {
	ctx := context.Background()

	authzConn, err := suite.connFactory.DialContext(
		ctx,
		"authz-service",
		authZAddress,
		grpc.WithBlock(),
	)
	if err != nil {
		return err
	}

	authzClient := policies.NewPoliciesServiceClient(authzConn)

	s.authzConn = authzConn
	s.AuthzClient = authzClient

	return nil
}

////////////////////////////////////////////////////////////////////////////////
// End of Authorization-Specific suite initialization
////////////////////////////////////////////////////////////////////////////////

func TestEventGatewayAuth(t *testing.T) {
	clusterID := "event-service"
	natsStreamingSubject := "habitat"
	invalidSubject := "invalid-subject"
	message := []byte("test message")

	t.Run("when the token is valid and has required permissions, can publish only to habitat topic", func(t *testing.T) {
		// give permissions for all actions on all resources
		adminToken := getNewToken(t, "*")

		natsURLForAdmin := fmt.Sprintf(natsURLfmt, adminToken)

		tlsConf := &tls.Config{
			MinVersion:         tls.VersionTLS12,
			InsecureSkipVerify: true,
		}

		rawNATSconn, err := natsc.Connect(natsURLForAdmin, natsc.Secure(tlsConf))
		require.NoError(t, err)
		defer rawNATSconn.Close()

		conn, err := stan.Connect(clusterID,
			newClientID(),
			stan.NatsConn(rawNATSconn),
		)
		require.NoError(t, err)
		defer conn.Close()

		// Positive test: can publish to the hab topic
		err = conn.Publish(natsStreamingSubject, message)
		require.NoError(t, err)

		// Negative test: can't publish to another topic
		err = conn.Publish(invalidSubject, message)
		require.Error(t, err)

		// Negative test: can't subscribe to the hab topic
		// This is because we restrict all external connections to NATS to just
		// what they need to publish NATS streaming messages to the "habitat" topic
		// to make the code simpler.
		_, err = conn.Subscribe("habitat", func(m *stan.Msg) {
		})
		require.Error(t, err)

		// Habitat client authz tests
		habClientId := "hab_client"
		habRawNATSconn, err := natsc.Connect(natsURLForAdmin, natsc.Secure(tlsConf))
		require.NoError(t, err)
		defer habRawNATSconn.Close()

		habConn, err := stan.Connect(clusterID,
			habClientId,
			stan.NatsConn(habRawNATSconn),
		)
		require.NoError(t, err)
		defer conn.Close()

		// hab_client can publish to habitat subject
		err = habConn.Publish(natsStreamingSubject, message)
		require.NoError(t, err)

		// hab_client can't publish to another subject
		err = habConn.Publish(invalidSubject, message)
		require.Error(t, err)
	})

	t.Run("when the token is invalid, cannot connect to NATS", func(t *testing.T) {
		natsURLWithInvalidToken := fmt.Sprintf(natsURLfmt, "INVALIDTOKEN")
		_, err := natsc.Connect(natsURLWithInvalidToken)
		require.Error(t, err)
	})

	t.Run("when the authZ subject doesn't have permissions for ingest, cannot connect to NATS", func(t *testing.T) {
		// only allowing read should make this token invalid for ingest
		notAuthorizedToken := getNewToken(t, "*:read")

		natsURLNotAuthorized := fmt.Sprintf(natsURLfmt, notAuthorizedToken)
		_, err := natsc.Connect(natsURLNotAuthorized)
		require.Error(t, err)
	})
}

func newClientID() string {
	return "applications-publisher-" + strconv.FormatInt(time.Now().UnixNano(), 10)
}

// getNewToken creates an auth token and adds it to a policy that is allowed to
// perform the action(s) described by `authorizedAction` on all resources.
func getNewToken(t *testing.T, authorizedAction string) string {
	ctx := context.Background()

	authnConnection, err := suite.connFactory.DialContext(
		ctx,
		"authn-service",
		authNAddress,
		grpc.WithBlock(),
	)
	require.NoError(t, err)
	defer authnConnection.Close() // nolint: errcheck

	authnClient := authn.NewTokensMgmtServiceClient(authnConnection)

	ctx = auth_context.NewOutgoingContext(auth_context.NewContext(ctx,
		[]string{"tls:service:deployment-service:internal"}, []string{}, "res", "act"))
	response, err := authnClient.CreateToken(ctx, &authn.CreateTokenReq{
		Id:       uuid.Must(uuid.NewV4()).String(),
		Name:     "token for event-service integration test",
		Active:   true,
		Projects: []string{},
	})
	require.NoError(t, err)

	polID := "nats-test-policy-" + uuid.Must(uuid.NewV4()).String()
	_, err = suite.AuthzClient.CreatePolicy(ctx, &policies.CreatePolicyReq{
		Id:      polID,
		Name:    polID,
		Members: []string{fmt.Sprintf("token:%s", response.Id)},
		Statements: []*policies.Statement{
			&policies.Statement{
				Effect:   policies.Statement_ALLOW,
				Actions:  []string{authorizedAction},
				Projects: []string{"*"},
			},
		},
		Projects: []string{},
	})
	require.NoError(t, err)

	return response.Value
}
