package service_authn

import (
	"context"
	"crypto/sha256"
	"crypto/x509"
	"encoding/hex"
	"fmt"

	"google.golang.org/grpc/credentials"
	"google.golang.org/grpc/peer"
)

// ServiceSubjectFromContext returns the client's service subject, as
// determined by the connecting client's TLS certificate's subject's "Common
// Name".
func ServiceSubjectFromContext(ctx context.Context) (string, bool) {
	// what follows is what we get from DIRECT GRPC connections
	peer, ok := peer.FromContext(ctx)
	if ok {
		if tlsInfo, ok := peer.AuthInfo.(credentials.TLSInfo); ok {
			if len(tlsInfo.State.VerifiedChains) > 0 && len(tlsInfo.State.VerifiedChains[0]) > 0 {
				return ServiceSubjectFromCert(tlsInfo.State.VerifiedChains[0][0])
			}
		}
	}
	return "", false
}

func ServiceSubjectFromCert(cert *x509.Certificate) (string, bool) {
	cn := cert.Subject.CommonName
	h := sha256.New()
	if _, err := h.Write(cert.Raw); err != nil {
		return "", false
	}
	hash := hex.EncodeToString(h.Sum(nil))
	return fmt.Sprintf("tls:service:%s:%s", cn, hash), cn != ""
}
