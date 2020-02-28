package main

import (
	"bufio"
	"context"
	"crypto/tls"
	"fmt"
	"io/ioutil"
	"math/rand"
	"net/http"
	"os"
	"path"
	"path/filepath"
	"regexp"
	"strings"
	"time"

	"github.com/aws/aws-sdk-go/aws"
	"github.com/aws/aws-sdk-go/aws/credentials"
	"github.com/aws/aws-sdk-go/aws/session"
	"github.com/pkg/errors"
	"github.com/spf13/cobra"
	"gocloud.dev/blob"
	"gocloud.dev/blob/s3blob"

	"github.com/chef/automate/components/automate-deployment/pkg/cli"
	"github.com/chef/automate/lib/httputils"
	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/platform/command"
)

var generateCmd = &cobra.Command{
	Use:  "generate",
	RunE: runGenerate,
}

var generateRealCmd = &cobra.Command{
	Use:  "real",
	RunE: runGenerateReal,
	Args: cobra.ExactArgs(1), // path
}

var realCmdOpts = struct {
	PlanPath string
	NumPkgs  int
	Username string
	Password string
	SeedList string
	Seed     bool
	URL      string
	Debug    bool
}{}

func init() {
	generateRealCmd.PersistentFlags().IntVarP(
		&realCmdOpts.NumPkgs, "package-count", "c", 3,
		"Number of packages to generate",
	)

	generateRealCmd.PersistentFlags().StringVarP(
		&realCmdOpts.Username, "user", "u", "admin",
		"Depot username",
	)

	generateRealCmd.PersistentFlags().StringVarP(
		&realCmdOpts.Password, "password", "p", "chefautomate",
		"Depot password",
	)

	generateRealCmd.PersistentFlags().StringVarP(
		&realCmdOpts.URL, "url", "", "https://localhost/bldr/v1",
		"Depot password",
	)

	generateRealCmd.PersistentFlags().StringVarP(
		&realCmdOpts.SeedList, "seed-list", "l", "core_deps_x86_64-linux_stable",
		"Core deps seed list",
	)

	generateRealCmd.PersistentFlags().BoolVarP(
		&realCmdOpts.Seed, "seed", "s", false,
		"Seed the core deps",
	)

	generateRealCmd.PersistentFlags().BoolVarP(
		&realCmdOpts.Debug, "debug", "d", false,
		"Enable debug mode",
	)

	generateCmd.AddCommand(generateRealCmd)
}

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

type RealPlanGenerator struct {
}

func (r *RealPlanGenerator) Generate() error {
	writer := cli.NewWriter(os.Stdout, os.Stderr, os.Stdin)
	writer.StartSpinner()
	fail := func(err error) error {
		writer.StopSpinner()
		writer.Fail(command.StderrFromError(err))
		writer.FailError(err)
		return err
	}

	harts := []string{}
	identReg := regexp.MustCompile(`pkg_artifact=(.*)`)
	cwd, err := os.Getwd()
	if err != nil {
		return fail(err)
	}

	for i := 0; i < realCmdOpts.NumPkgs; i++ {
		habOpts := []command.Opt{command.Args("pkg", "build", realCmdOpts.PlanPath)}

		if realCmdOpts.Debug {
			habOpts = append(habOpts, command.Stderr(os.Stderr))
			habOpts = append(habOpts, command.Stdout(os.Stdout))
		}

		err := command.Run("hab", habOpts...)
		if err != nil {
			return fail(err)
		}

		lastEnvP := filepath.Join(cwd, "results", "last_build.env")
		lastEnv, err := os.Open(lastEnvP)
		defer lastEnv.Close() // nolint errcheck
		if err != nil {
			return fail(err)
		}

		scanner := bufio.NewScanner(lastEnv)
		for scanner.Scan() {
			l := scanner.Text()
			matches := identReg.FindAllString(l, 1)
			if len(matches) > 0 {
				parts := strings.Split(matches[0], "=")
				hart := filepath.Join(cwd, "results", parts[1])
				defer os.Remove(hart) // nolint errcheck
				harts = append(harts, hart)
				break
			}
		}
	}

	tmpDir, err := ioutil.TempDir(filepath.Join(cwd, "results"), "gen")
	if err != nil {
		return fail(err)
	}
	defer os.RemoveAll(tmpDir) // nolint errcheck

	// bulkupload looks for the "artifacts" and "keys" dir in the root directory
	// that is passed
	artifactsDir := filepath.Join(tmpDir, "artifacts")
	err = os.Mkdir(artifactsDir, 0644)
	if err != nil {
		return fail(err)
	}
	keysDir := filepath.Join(tmpDir, "keys")

	for _, hart := range harts {
		if err = fileutils.CopyFile(hart, filepath.Join(artifactsDir, filepath.Base(hart))); err != nil {
			return fail(err)
		}
	}

	err = fileutils.CopyDir("/hab/cache/keys", keysDir, fileutils.Overwrite())
	if err != nil {
		return fail(err)
	}

	token, err := doLogin(realCmdOpts.Username, realCmdOpts.Password)
	if err != nil {
		return err
	}

	if realCmdOpts.Seed {
		// TODO: Download seed list and upload it to the builder
	}

	habOpts := []command.Opt{
		command.Envvar("HAB_AUTH_TOKEN", token),
		command.Envvar("HAB_SSL_CERT_VERIFY_NONE", "1"),
		command.Args("pkg", "bulkupload",
			"--url", realCmdOpts.URL,
			"--channel", "stable", tmpDir,
			"--auto-create-origins",
		),
	}

	if realCmdOpts.Debug {
		habOpts = append(habOpts, command.Stderr(os.Stderr))
		habOpts = append(habOpts, command.Stdout(os.Stdout))
	}

	err = command.Run("hab", habOpts...)
	if err != nil {
		return fail(err)
	}

	writer.StopSpinner()
	return nil
}

func NewRealPlanGenerator() *RealPlanGenerator {
	return &RealPlanGenerator{}
}

func runGenerateReal(c *cobra.Command, args []string) error {
	generator := NewRealPlanGenerator()

	if len(args) > 0 {
		if path := args[0]; path != "" {
			realCmdOpts.PlanPath = path
		}
	}

	return generator.Generate()
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
		return nil, errors.Wrapf(err, "opening bucket %s", bucketName)
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
				fmt.Printf("Creating %s/%s\n", bucketName, s3Path)
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
