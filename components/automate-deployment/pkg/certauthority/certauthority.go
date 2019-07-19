// Copyright © 2017 Chef Software

// Package certauthority generates and manages x509 certificates for
// use in cross-service authentication.
package certauthority

import (
	"crypto/x509"
	"encoding/pem"
	"fmt"
	"net"
	"time"

	"github.com/pkg/errors"
)

// A CertBackend is used by CertAuthority to generate the CA and
// per-service certificates. By default, the deployment service will
// use the CertstrapBackend implementation of this interface found in
// certstrap.go
type CertBackend interface {
	// Generates the new CA, returning the root CA cert. This
	// should be safe-to-recall across reboots of the service.
	Init() (string, error)
	// Returns true if Init() has been called in the past and
	// CertForService is expected to succeed. This should work
	// across reboots of the process.
	IsInitialized() (bool, error)
	// Generate a cert for a given service, signed by the CA
	CertForService(CertRequest) (*ServiceCertData, error)

	// ReInit re-initializes the certificate authority
	ReInit() (string, error)
}

type CertRequest struct {
	// The name of the service that the cert is being requested
	// for. This name should be used for the Common Name in the
	// subject and also added as a Subject Alternative Name.
	name string
	// IP addresses that should be added as Subject Alternative Names
	ips []net.IP
	// Hostnames that should be added as Subject Alternative Names
	dnsNames []string
}

// A CertAuthority represents a CA certificate and key that can be
// used to generate per-service certificates. The certBackend is
// responsible for certificate generation and maintaining the CA
// across restarts.  This type is responsible for maintaining a copy of
// our root cert for easy access and enforcing our naming conventions
// for services' common names.
type CertAuthority struct {
	// authorityID is appended to the Common Name's of generated
	// certificates to make it easy to tell if two certs come from
	// the same authority or not.
	authorityID string
	// authorityName is the common name of our authority.  It is
	// generated from the authorityID.
	authorityName string
	// PEM-encoded root certificate. We hand pointers to this out
	// via ServiceCertData so configs can consume it.
	rootCertPEM string
	// Parsed root certificate. Stored here to avoid parsing it
	// multiple times.
	rootCert *x509.Certificate
	// A CertBackend where the real work happens.
	certBackend CertBackend
}

// A ServiceCertData is returned to callers of CertForService and
// contains all of the key data required for the service to configure
// mutual TLS
type ServiceCertData struct {
	// The PEM-formated RSA key
	Key string
	// The PEM-formated x509 certificate
	Cert string
	// The root cert for our CA. Services should trust this.
	RootCert *string
}

const caNameFmt = "Chef Automate %s"

var (
	// expiryMargin is how far before the NotAfter time on an x509
	// certificate we will mark the certificate as needing to be
	// regenerated.
	expiryMargin = 28 * (time.Hour * 24)

	// notBeforeMargin is how far before a NotBefore time on an
	// x509 certificate we will allow without marking the
	// certificate as needing to be regenerated. The thinking
	// behind this margin is that if the NotBefore time is near,
	// then it likely is worth it it to wait rather than
	// regenerating a certificate that would then need to be
	// redeployed to the relevant applications.
	notBeforeMargin = 5 * time.Minute
)

// NotSignedByCA is returned by validation functions when the
// certificate being validated is not signed by the CA doing
// the validation.
var NotSignedByCA = errors.New("Certificate is not signed by Certificate Authority")

// CommonNameMismatch is an error returned by validation functions
// when the common name of the certificate subject does not match the
// name of the given CertRequest.
type CommonNameMismatch struct{ want, have string }

func NewCommonNameMismatch(want, have string) *CommonNameMismatch {
	return &CommonNameMismatch{want: want, have: have}
}
func (c *CommonNameMismatch) Error() string {
	return fmt.Sprintf("Certificate's common name (%q) does not match the requested name (%q)", c.have, c.want)
}

// CertExpired is an error returned by validation functions
// when the certificate is expired or within our expiry window.
type CertExpired struct {
	notAfter time.Time
	window   time.Duration
}

func NewCertExpired(notAfter time.Time, window time.Duration) *CertExpired {
	return &CertExpired{
		notAfter: notAfter,
		window:   window,
	}
}
func (c *CertExpired) Error() string {
	timeLeft := time.Until(c.notAfter)
	if timeLeft < 0 {
		return fmt.Sprintf("Certificate has expired (not valid after %s)", c.notAfter)
	}

	return fmt.Sprintf("Certificate expires in %s which is less than the %s expiry window", timeLeft, c.window)
}

type CertNotYetValid struct{ notBefore time.Time }

func NewCertNotYetValid(notBefore time.Time) *CertNotYetValid {
	return &CertNotYetValid{notBefore: notBefore}
}
func (c *CertNotYetValid) Error() string {
	return fmt.Sprintf("Certificate is not valid until %s (%s from now)", c.notBefore, time.Until(c.notBefore))
}

// SANIPAddrMismatch is an error returned by validation functions when
// the Subject Alternative Name extension in the certificate does not
// have IP values for every IP in the given CertRequest.
type SANIPAddrMismatch struct{ want, have []net.IP }

func NewSANIPAddrMismatch(want, have []net.IP) *SANIPAddrMismatch {
	return &SANIPAddrMismatch{want: want, have: have}
}
func (c *SANIPAddrMismatch) Error() string {
	return fmt.Sprintf("Certificate's Subject Alternative Name IP addresses (%v) do not match the requested IP addresses (%v)", c.have, c.want)
}

// SANHostnameMismatch is an error returned by validation functions
// when the Subject Alternative Name extension in the certificate does
// not have DNSName values for every hostname in the given
// CertRequest.
type SANHostnameMismatch struct{ want, have []string }

func NewSANHostnameMismatch(want, have []string) *SANHostnameMismatch {
	return &SANHostnameMismatch{want: want, have: have}
}
func (c *SANHostnameMismatch) Error() string {
	return fmt.Sprintf("Certificate's Subject Alternative Name DNSNames (%v) do not match the requested DNSNames (%v)", c.have, c.want)
}

// NewCertAuthority creates a new CertAuthority with a name generated
// from id, storing certificate data in `directory`.
//
// `id` should be unique enough to facilitate identifying certificates
// from the same deployment.
func NewCertAuthority(backend CertBackend, id string) *CertAuthority {
	return &CertAuthority{
		certBackend:   backend,
		authorityID:   id,
		authorityName: fmt.Sprintf(caNameFmt, id)}
}

func NewCertRequest(name string, ips []net.IP, dnsNames []string) CertRequest {
	return CertRequest{
		name: name,
		ips:  ips,
		// Always add the server-name as a DNS name.
		//
		// RFC 6125 Section 6.4.4 says:
		//
		// > As noted, a client MUST NOT seek a match for a reference identifier
		// > of CN-ID if the presented identifiers include a DNS-ID, SRV-ID,
		// > URI-ID, or any application-specific identifier types supported by the
		// > client.
		// (https://tools.ietf.org/html/rfc6125#section-6.4.4)
		//
		// And RFC 2818 Section 3.1 says:
		//
		// > If a subjectAltName extension of type dNSName is present, that MUST
		// > be used as the identity. Otherwise, the (most specific) Common Name
		// > field in the Subject field of the certificate MUST be used. Although
		// > the use of the Common Name is existing practice, it is deprecated and
		// > Certification Authorities are encouraged to use the dNSName instead.
		//
		// (https://tools.ietf.org/html/rfc2818#section-3.1)
		//
		// One reading of these might one lead to believe we
		// only need to do this if we have /other/ dnsName
		// entries and not just IP entries. But curl and the
		// major browsers ignore CN if *any* SAN extensions
		// exist:
		//
		// https://github.com/curl/curl/issues/1065
		//
		// And, perhaps more importantly, so does the Golang
		// TLS library:
		//
		// https://github.com/golang/go/blob/ee76992200a282f0ed4eb52e686ec254d8313cdc/src/crypto/x509/verify.go#L907-L917
		//
		// Further, since the use of common name for
		// validation has been deprecated, we follow their
		// lead and always add it here.
		//
		dnsNames: appendIfMissing(dnsNames, name),
	}
}

// NewCertstrapCertAuthority is a convenience constructor for the common,
// non-test use case.
func NewCertstrapCertAuthority(dataDir string, id string) *CertAuthority {
	backend := NewCertstrapBackend(dataDir, fmt.Sprintf(caNameFmt, id))
	return NewCertAuthority(backend, id)
}

// InitAuthority generates the CA certificate for the given
// CertAuthority.
func (a *CertAuthority) InitAuthority() error {
	certData, err := a.certBackend.Init()
	if err != nil {
		return err
	}

	rootCert, err := PEMToCert(certData)
	if err != nil {
		return errors.Wrap(err, "could not parse root certificate from backend")
	}

	a.rootCertPEM = certData
	a.rootCert = rootCert
	return nil
}

// IsInitialized returns true if InitAuthority() has been called in
// the past. Delegates to backend.
func (a *CertAuthority) IsInitialized() (bool, error) {
	return a.certBackend.IsInitialized()
}

// CertDataForService returns certificate data for the named
// service. The certificate is signed by our certificate authority and
// is appropriate for authenticating gRPC traffic between Chef
// Automate services.
func (a *CertAuthority) CertDataForService(request CertRequest) (*ServiceCertData, error) {
	init, err := a.certBackend.IsInitialized()
	if err != nil {
		return nil, errors.Wrap(err, "failed to determine CA init state")
	}

	if !init {
		return nil, errors.New("cannot get certificate from uninitialized certificate authority")
	}

	certData, err := a.certBackend.CertForService(request)
	if err != nil {
		return nil, err
	}

	certData.RootCert = &a.rootCertPEM
	return certData, nil
}

// RootCert returns the root certificate for use
func (a *CertAuthority) RootCert() string {
	return a.rootCertPEM
}

// RegenerateRoot regenerates the root certificate for this
// certificate authority. As a result, all certificates issues by the
// previous root certificate will be invalid.
func (a *CertAuthority) RegenerateRoot() error {
	certData, err := a.certBackend.ReInit()
	if err != nil {
		return err
	}

	rootCert, err := PEMToCert(certData)
	if err != nil {
		return errors.Wrap(err, "could not parse root certificate from backend")
	}

	a.rootCertPEM = certData
	a.rootCert = rootCert
	return nil
}

// ValidateCA checks the CA certificate for the certificate authority
// against local validity rules.
func (a *CertAuthority) ValidateCA() error {
	if time.Until(a.rootCert.NotAfter) < expiryMargin {
		return NewCertExpired(a.rootCert.NotAfter, expiryMargin)
	}

	if time.Until(a.rootCert.NotBefore) > notBeforeMargin {
		return NewCertNotYetValid(a.rootCert.NotBefore)
	}

	return nil
}

// IsSignedBy returns true if the given x509 certificate was signed by
// the given CertAuthorities root certificate. For convenience we take
// a string since we tend to pass pem-encoded string versions of the
// certs around.
func (a *CertAuthority) IsSignedBy(cert *x509.Certificate) bool {
	rootCertPool := x509.NewCertPool()
	rootCertPool.AddCert(a.rootCert)
	_, err := cert.Verify(x509.VerifyOptions{
		Roots: rootCertPool,
	})

	return err == nil
}

// ValidateCertificateForRequest checks the existing certificate for
// common problems and whether or not it matches the passed
// CertRequest. If a non-nil error is returned if the certificate is
// not valid.
func (a *CertAuthority) ValidateCertificateForRequest(cert *x509.Certificate, certRequest CertRequest) error {
	if !a.IsSignedBy(cert) {
		return NotSignedByCA
	}

	if time.Until(cert.NotAfter) < expiryMargin {
		return NewCertExpired(cert.NotAfter, expiryMargin)
	}

	if time.Until(cert.NotBefore) > notBeforeMargin {
		return NewCertNotYetValid(cert.NotBefore)
	}

	if cert.Subject.CommonName != certRequest.name {
		return NewCommonNameMismatch(certRequest.name, cert.Subject.CommonName)
	}

	if !allIPsMatch(cert.IPAddresses, certRequest.ips) {
		return NewSANIPAddrMismatch(certRequest.ips, cert.IPAddresses)
	}

	if !allHostnamesMatch(cert.DNSNames, certRequest.dnsNames) {
		return NewSANHostnameMismatch(certRequest.dnsNames, cert.DNSNames)
	}
	return nil
}

func (r CertRequest) String() string {
	return fmt.Sprintf("CertRequest{name: %s, ips: %v, dnsNames: %v}", r.name, r.ips, r.dnsNames)
}

func CertForLog(cert *x509.Certificate) string {
	return fmt.Sprintf("Certificate{Subject: %s Issuer:, %s NotAfter: %s, NotBefore: %s, IPAddresses: %v, DNSNames: %v}",
		cert.Subject,
		cert.Issuer,
		cert.NotAfter,
		cert.NotBefore,
		cert.IPAddresses,
		cert.DNSNames,
	)
}

// BytesToCert takes an array of bytes and tries to parse it as an
// x509 Certificate.
func BytesToCert(bytes []byte) (*x509.Certificate, error) {
	cert, err := x509.ParseCertificate(bytes)
	if err != nil {
		return nil, err
	}
	return cert, nil
}

// PEMToCert converts a PEM-encoded string into an x509.Certificate
func PEMToCert(cert string) (*x509.Certificate, error) {
	pemBlock, _ := pem.Decode([]byte(cert))
	if pemBlock == nil {
		return nil, errors.New("could not PEM-decode string")
	}
	return BytesToCert(pemBlock.Bytes)
}

func appendIfMissing(slice []string, item string) []string {
	for _, s := range slice {
		if item == s {
			return slice
		}
	}
	return append(slice, item)
}

func allIPsMatch(a, b []net.IP) bool {
	if len(a) != len(b) {
		return false
	}

	for _, IPa := range a {
		found := false
		for _, IPb := range b {
			if IPa.Equal(IPb) {
				found = true
				break
			}
		}

		if !found {
			return false
		}
	}

	return true
}

func allHostnamesMatch(a, b []string) bool {
	if len(a) != len(b) {
		return false
	}

	for _, aStr := range a {
		found := false
		for _, bStr := range b {
			if aStr == bStr {
				found = true
				break
			}
		}

		if !found {
			return false
		}
	}

	return true
}
