// +build integration

package integration_test

import (
	"context"
	"crypto/tls"
	"fmt"
	"strconv"
	"testing"
	"time"

	natsc "github.com/nats-io/nats.go"
	stan "github.com/nats-io/stan.go"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc"

	"github.com/chef/automate/api/interservice/authn"
	"github.com/chef/automate/api/interservice/authz"
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
	ctx := context.Background()
	err := s.initAuthzClient()
	if err != nil {
		return err
	}

	policiesResp, err := s.AuthzClient.ListPolicies(ctx, &authz.ListPoliciesReq{})
	if err != nil {
		return err
	}

	for _, p := range policiesResp.Policies {
		if p.Resource == "ingest:*" {
			s.AuthPoliciesToRestore = append(s.AuthPoliciesToRestore, p)
		}
	}

	for _, p := range s.AuthPoliciesToRestore {
		_, err := s.AuthzClient.DeletePolicy(ctx, &authz.DeletePolicyReq{Id: p.Id})
		if err != nil {
			return err
		}
	}

	return nil
}

// teardownNATSAuthTest re-creates any auth policies that were deleted in the
// initNATSAuthTest step.
func (s *Suite) teardownNATSAuthTest() {
	ctx := context.Background()
	defer s.authzConn.Close()

	for _, p := range s.AuthPoliciesToRestore {
		_, err := s.AuthzClient.CreatePolicy(ctx, &authz.CreatePolicyReq{
			Action:   p.Action,
			Subjects: p.Subjects,
			Resource: p.Resource,
		})
		if err != nil {
			fmt.Printf("Error in NATS Auth Test teardown: %s\n", err)
		}
	}

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

	authzClient := authz.NewAuthorizationClient(authzConn)

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
		notAuthorizedToken := getNewToken(t, "read")

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

	authnClient := authn.NewTokensMgmtClient(authnConnection)

	ctx = auth_context.NewOutgoingContext(auth_context.NewContext(ctx,
		[]string{"tls:service:deployment-service:internal"}, []string{}, "res", "act", "v2.1"))
	response, err := authnClient.CreateToken(ctx, &authn.CreateTokenReq{
		Description: "token for event-service integration test",
		Active:      true,
		Projects:    []string{},
	})
	require.NoError(t, err)

	_, err = suite.AuthzClient.CreatePolicy(ctx, &authz.CreatePolicyReq{
		Action:   authorizedAction,
		Subjects: []string{fmt.Sprintf("token:%s", response.Id)},
		Resource: "*",
	})
	require.NoError(t, err)

	return response.Value
}
