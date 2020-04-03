package main

import (
	"context"
	"crypto/tls"
	"fmt"
	"io/ioutil"
	"math/rand"
	"net/http"
	"path"
	"time"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/credentials"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"gocloud.dev/blob"
	"gocloud.dev/blob/s3blob"

	"github.com/chef/automate/lib/httputils"
)

type Generator interface {
	Generate() string
}

type Distribution interface {
	Value() int
}

type ConstantDistribution struct {
	V int
}

func NewConstantDistribution(v int) *ConstantDistribution {
	return &ConstantDistribution{
		V: v,
	}
}

func (c *ConstantDistribution) Value() int {
	return c.V
}

type RandomDistribution struct {
	Min int
	Max int
}

func (r *RandomDistribution) Value() int {
	return rand.Intn(r.Max-r.Min) + r.Min
}

func NewRandomDistribution(min int, max int) *RandomDistribution {
	return &RandomDistribution{
		Min: min,
		Max: max,
	}
}

type StringGenerator struct {
	SizeDistribution Distribution
}

const letters = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

func (g *StringGenerator) Generate() string {
	stringLen := int(g.SizeDistribution.Value())
	b := make([]byte, stringLen)
	for i := range b {
		b[i] = letters[rand.Intn(len(letters))]
	}
	return string(b)
}

type TimestampGenerator struct {
	DateDistribution Distribution
}

func (g *TimestampGenerator) Generate() string {
	timestamp := time.Unix(int64(g.DateDistribution.Value()), 0)

	return timestamp.Format("20060102150405")
}

func initS3Bucket(bucketName string) (*blob.Bucket, error) {
	accessKey, err := ioutil.ReadFile(
		"/hab/svc/deployment-service/data/shared/minio/access_key")
	if err != nil {
		return nil, err
	}

	secretKey, err := ioutil.ReadFile(
		"/hab/svc/deployment-service/data/shared/minio/secret_key")
	if err != nil {
		return nil, err
	}

	tr := httputils.NewDefaultTransport()
	tr.TLSClientConfig = &tls.Config{
		InsecureSkipVerify: true,
	}
	c := &aws.Config{
		Region:   aws.String("us-east-1"),
		Endpoint: aws.String("https://127.0.0.1:10106"),
		Credentials: credentials.NewStaticCredentials(
			string(accessKey),
			string(secretKey), ""),
		S3ForcePathStyle: aws.Bool(true),
		HTTPClient:       &http.Client{Transport: tr},
	}

	s, err := session.NewSession(c)
	if err != nil {
		return nil, err
	}

	bucket, err := s3blob.OpenBucket(context.Background(), s, bucketName, nil)
	if err != nil {
		return nil, err
	}

	return bucket, nil
}

func runGenerate(c *cobra.Command, args []string) error {
	bucketName := "depot-test"
	if len(args) >= 1 {
		bucketName = args[0]
	}

	bucket, err := initS3Bucket(bucketName)
	if err != nil {
		return errors.Wrap(err, "init s3 bucket")
	}

	originNameDistribution := NewRandomDistribution(10, 20)
	numOriginsDistribution := NewConstantDistribution(10)

	packageNameDistribution := NewRandomDistribution(10, 20)
	numPackageNamesDistribution := NewConstantDistribution(10)

	numReleasesDistribution := NewConstantDistribution(10)
	releaseDistribution := NewRandomDistribution(
		int(time.Now().AddDate(-1, 0, 0).Unix()),
		int(time.Now().Unix()))

	numOrigins := numOriginsDistribution.Value()
	originNameGenerator := StringGenerator{
		SizeDistribution: originNameDistribution,
	}
	for iOrigin := 0; iOrigin < numOrigins; iOrigin++ {
		originName := originNameGenerator.Generate()
		packageNameGenerator := StringGenerator{
			SizeDistribution: packageNameDistribution,
		}
		numPackageNames := numPackageNamesDistribution.Value()
		for iPackageName := 0; iPackageName < numPackageNames; iPackageName++ {
			packageName := packageNameGenerator.Generate()
			numReleases := numReleasesDistribution.Value()
			releaseGenerator := TimestampGenerator{
				DateDistribution: releaseDistribution,
			}
			for iReleases := 0; iReleases < numReleases; iReleases++ {
				release := releaseGenerator.Generate()
				fullName := fmt.Sprintf("%s/%s/0.1.0/%s", originName, packageName, release)
				s3Path := path.Join(fullName, "x86_64/linux",
					fmt.Sprintf("%s-%s-%s-%s-x86_64-linux.hart",
						originName,
						packageName,
						"0.1.0",
						release))
				fmt.Printf("Creating %s\n", s3Path)
				if err := writeFile(bucket, s3Path); err != nil {
					return err
				}
			}
		}
	}
	return nil
}

func writeFile(bucket *blob.Bucket, s3Path string) error {
	w, err := bucket.NewWriter(context.Background(), s3Path, nil)
	if err != nil {
		return err
	}
	defer w.Close()
	_, err = w.Write([]byte(s3Path))
	if err != nil {
		return err
	}
	return w.Close()
}
