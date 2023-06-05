package certgenerateutils

import (
	"crypto/rand"
	"crypto/rsa"
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/pem"
	"fmt"
	"math/big"
	"os"
	"time"

	"github.com/pkg/errors"
)

func generateSerial() (*big.Int, error) {
	// According to
	// https://cabforum.org/wp-content/uploads/CA-Browser-Forum-BR-1.6.4.pdf:
	//
	// Effective September 30, 2016, CAs SHALL generate
	// non-sequential Certificate serial numbers greater than zero
	// (0) containing at least 64 bits of output from a CSPRNG.
	//
	// Here, we set the limit to double this requirement.
	limit := new(big.Int).Lsh(big.NewInt(1), 128)
	ret, err := rand.Int(rand.Reader, limit)
	if err != nil {
		return nil, errors.Wrap(err, "failed to generate serial number")
	}
	return ret, nil
}

func GenerateCert(fqdn string) error {
	// Generate a private key
	privateKey, err := rsa.GenerateKey(rand.Reader, 2048)
	if err != nil {
		fmt.Println("Failed to generate private key:", err)
		return err
	}

	serial, err := generateSerial()
	if err != nil {
		fmt.Println("Failed to generate serial:", err)
		return err
	}

	// Create a self-signed certificate
	template := &x509.Certificate{
		SerialNumber: serial,
		Subject: pkix.Name{
			Country:            []string{"US"},
			Organization:       []string{"Chef Software"},
			OrganizationalUnit: []string{"Chef Automate"},
			CommonName:         fqdn,
		},
		NotBefore:             time.Now(),
		NotAfter:              time.Now().AddDate(10, 0, 0),
		KeyUsage:              x509.KeyUsageCertSign | x509.KeyUsageCRLSign,
		BasicConstraintsValid: true,
		IsCA:                  true,
		DNSNames:              []string{fqdn},
	}

	derBytes, err := x509.CreateCertificate(rand.Reader, template, template, &privateKey.PublicKey, privateKey)
	if err != nil {
		fmt.Println("Failed to create certificate:", err)
		return err
	}

	// Save private key to file
	privateKeyFile, err := os.Create("private_key.pem")
	if err != nil {
		fmt.Println("Failed to create private key file:", err)
		return err
	}
	defer privateKeyFile.Close()

	privateKeyBlock := &pem.Block{
		Type:  "PRIVATE KEY",
		Bytes: x509.MarshalPKCS1PrivateKey(privateKey),
	}

	err = pem.Encode(privateKeyFile, privateKeyBlock)
	if err != nil {
		fmt.Println("Failed to encode private key:", err)
		return err
	}

	// Save certificate to file
	certFile, err := os.Create("certificate.pem")
	if err != nil {
		fmt.Println("Failed to create certificate file:", err)
		return err
	}
	defer certFile.Close()

	certBlock := &pem.Block{
		Type:  "CERTIFICATE",
		Bytes: derBytes,
	}

	err = pem.Encode(certFile, certBlock)
	if err != nil {
		fmt.Println("Failed to encode certificate:", err)
		return err
	}
	return nil
}
