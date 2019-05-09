package middleware

import (
	"context"
	"crypto/tls"
	"crypto/x509"
	"encoding/pem"
	"fmt"
	"net/url"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
	"google.golang.org/grpc/credentials"
	"google.golang.org/grpc/metadata"
	"google.golang.org/grpc/peer"

	"github.com/chef/automate/lib/tls/test/helpers"
)

func TestHeaderAuthValidForClientAndPeer(t *testing.T) {
	albCert, _ := devCertToEncodedAndPeer(t, "automate-load-balancer")
	_, agPeer := devCertToEncodedAndPeer(t, "automate-gateway")
	otherServiceCert, otherServicePeer := devCertToEncodedAndPeer(t, "deployment-service")
	hash := "f42fec42094a67caee0c485ee28ec03587170fabfa8a2d7d06188a5acbcee6f0"

	cases := map[string]struct {
		ctx           context.Context
		expectFailure bool
	}{
		"directly from metadata": {
			ctx: metadata.NewIncomingContext(
				context.Background(),
				metadata.Pairs("x-client-cert", otherServiceCert))},

		"from gateway-provided metadata if coming in through ALB": {
			ctx: metadata.NewIncomingContext(
				peer.NewContext(context.Background(), agPeer),
				metadata.Pairs("x-client-cert", albCert, "grpcgateway-x-client-cert", otherServiceCert))},

		"direct request NOT from automate-gateway": {
			ctx: peer.NewContext(context.Background(), otherServicePeer),
		},

		"from gateway-provided metadata and NOT coming in through ALB": {
			ctx: metadata.NewIncomingContext(
				context.Background(),
				metadata.Pairs("grpcgateway-x-client-cert", otherServiceCert)),
			expectFailure: true},

		"garbage data from metadata": {
			ctx: metadata.NewIncomingContext(
				context.Background(),
				metadata.Pairs("x-client-cert", "ThisIsNotACert")),
			expectFailure: true},

		"no metadata": {
			ctx:           context.Background(),
			expectFailure: true},

		"no information in  metadata": {
			ctx:           metadata.NewIncomingContext(context.Background(), metadata.Pairs()),
			expectFailure: true},
	}
	for desc, tc := range cases {
		t.Run(desc, func(t *testing.T) {
			name, ok := headerAuthValidForClientAndPeer(tc.ctx)

			require.Equal(t, !tc.expectFailure, ok, "expected operation result=>%v but actual=>%v", !tc.expectFailure, ok)
			if !tc.expectFailure {
				assert.Equal(t, fmt.Sprintf("tls:service:deployment-service:%s", hash), name)
			}
		})
	}
}

func TestGetProjectsFromMetadata(t *testing.T) {
	cases := map[string]struct {
		input    []string
		expected []string
	}{
		"no projects": {
			input:    []string{""},
			expected: []string{""},
		},
		"a single project": {
			input:    []string{"pikachu"},
			expected: []string{"pikachu"},
		},
		"multiple projects": {
			input:    []string{"pichchu, abra"},
			expected: []string{"pichchu", "abra"},
		},
		"multiple project in separate headers": {
			input:    []string{"pichchu, abra", "kadabra"},
			expected: []string{"pichchu", "abra", "kadabra"},
		},
		"multiple project in separate headers and whitespace": {
			input:    []string{"pichchu, abra", "   kadabra  , alakazam  "},
			expected: []string{"pichchu", "abra", "kadabra", "alakazam"},
		},
		"multiple project in separate headers, whitespace, and repeats": {
			input:    []string{"abra, pichchu, abra", "   kadabra, kadabra  , alakazam  "},
			expected: []string{"abra", "pichchu", "kadabra", "alakazam"},
		},
	}
	for desc, tc := range cases {
		t.Run(desc, func(t *testing.T) {
			require.Equal(t, tc.expected, getProjectsFromMetadata(tc.input))
		})
	}
}

func devCertToEncodedAndPeer(t *testing.T, service string) (string, *peer.Peer) {
	t.Helper()

	serviceCerts := helpers.LoadDevCerts(t, service)
	cert := serviceCerts.ServiceKeyPair.Certificate[0]
	x509Cert, err := x509.ParseCertificate(cert)
	require.NoError(t, err)
	block := pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: x509Cert.Raw})
	require.NotNil(t, block)
	p := peer.Peer{
		AuthInfo: credentials.TLSInfo{
			State: tls.ConnectionState{
				VerifiedChains: [][]*x509.Certificate{
					[]*x509.Certificate{x509Cert},
				},
			},
		},
	}
	return url.QueryEscape(string(block)), &p
}
