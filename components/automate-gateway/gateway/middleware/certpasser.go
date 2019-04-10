package middleware

import (
	"context"
	"encoding/pem"
	"net/http"
	"net/url"

	"google.golang.org/grpc/metadata"
)

// CertificatePasser takes the certificates from the requesting client
// and stores them in the x-client-cert header for use for cert auth.
func CertificatePasser(_ context.Context, req *http.Request) metadata.MD {
	if tls := req.TLS; tls != nil {
		if tls.PeerCertificates != nil && len(tls.PeerCertificates) > 0 {
			encoded := pem.EncodeToMemory(&pem.Block{
				Type:  "CERTIFICATE",
				Bytes: tls.PeerCertificates[0].Raw,
			})
			return metadata.Pairs("x-client-cert", url.QueryEscape(string(encoded)))
		}
	}
	return metadata.Pairs()
}
