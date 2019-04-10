package backup

import (
	"context"
	"io/ioutil"
	"os"
	"testing"

	"github.com/stretchr/testify/suite"
)

type BucketTestSuite struct {
	suite.Suite
	locationSpec LocationSpecification
	beforeTest   func(*BucketTestSuite)
	afterTest    func(*BucketTestSuite)
}

func (suite *BucketTestSuite) TestRoundtrip() {
	ctx := context.Background()
	roots := []string{"", "root1", "root2/root3"}

	for _, root := range roots {
		bucketRoot := suite.locationSpec.ToBucket(root)

		// Write hello to the key foo/bar/baz
		w, err := bucketRoot.NewWriter(ctx, "foo/bar/baz")
		suite.Require().NoError(err)

		_, err = w.Write([]byte("hello"))
		suite.Require().NoError(err)

		err = w.Close()
		suite.Require().NoError(err)

		// Read back hello from the foo/bar/baz key
		reader, err := bucketRoot.NewReader(ctx, "foo/bar/baz", &NoOpObjectVerifier{})
		suite.Require().NoError(err)

		data, err := ioutil.ReadAll(reader)
		suite.Require().NoError(err)

		suite.Assert().Equal("hello", string(data))

		err = reader.Close()
		suite.Require().NoError(err)

		// Read back "hello" from the foo/bar/baz key and verify the SHA256 (success case)
		shas := make(map[string]string)
		shas["foo/bar/baz"] = "2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824"
		verifier := SHA256Verifier{blobSHA256s: shas}

		reader, err = bucketRoot.NewReader(ctx, "foo/bar/baz", &verifier)
		suite.Require().NoError(err)

		data, err = ioutil.ReadAll(reader)
		suite.Require().NoError(err)

		suite.Assert().Equal("hello", string(data))

		err = reader.Close()
		suite.Require().NoError(err)

		// Read back "hello" from the foo/bar/baz key and verify the SHA256 (checksum mismatch case)
		shas = make(map[string]string)
		shas["foo/bar/baz"] = "not-matching-sha256-sum"
		verifier = SHA256Verifier{blobSHA256s: shas}

		reader, err = bucketRoot.NewReader(ctx, "foo/bar/baz", &verifier)
		suite.Require().Error(err)
		expectedMsg := "Object \"foo/bar/baz\" in the snapshot has been modified or is corrupted. Expected checksum: \"not-matching-sha256-sum\"; actual: \"2cf24dba5fb0a30e26e83b2ac5b9e29e1b161e5c1fa7425e73043362938b9824\""
		suite.Assert().Equal(expectedMsg, err.Error())

		// Make sure IsNotExist returns true when we read a key that
		// doesn't exist
		reader, err = bucketRoot.NewReader(ctx, "bar", &NoOpObjectVerifier{})
		suite.Require().Error(err)
		suite.Assert().True(IsNotExist(err))
		IsNotExist(err)

		// List all keys
		objs, prefixes, err := bucketRoot.List(ctx, "", false)
		suite.Require().NoError(err)

		suite.Assert().Equal([]BucketObject{{"foo/bar/baz"}}, objs)
		suite.Assert().Equal([]SharedPrefix{}, prefixes)

		// List subset
		objs, prefixes, err = bucketRoot.List(ctx, "foo/", false)
		suite.Require().NoError(err)

		suite.Assert().Equal([]BucketObject{{"foo/bar/baz"}}, objs)
		suite.Assert().Equal([]SharedPrefix{}, prefixes)

		// List non existent keys
		objs, prefixes, err = bucketRoot.List(ctx, "bar/", false)
		suite.Require().NoError(err)

		suite.Assert().Equal([]BucketObject{}, objs)
		suite.Assert().Equal([]SharedPrefix{}, prefixes)

		// List "directories" at root
		objs, prefixes, err = bucketRoot.List(ctx, "", true)
		suite.Require().NoError(err)

		suite.Assert().Equal([]BucketObject{}, objs)
		suite.Assert().Equal([]SharedPrefix{"foo/"}, prefixes)

		// List "directories" at foo/
		objs, prefixes, err = bucketRoot.List(ctx, "foo/", true)
		suite.Require().NoError(err)

		suite.Assert().Equal([]BucketObject{}, objs)
		suite.Assert().Equal([]SharedPrefix{"foo/bar/"}, prefixes)

		// List "directories" at foo/bar
		objs, prefixes, err = bucketRoot.List(ctx, "foo/bar/", true)
		suite.Require().NoError(err)

		suite.Assert().Equal([]BucketObject{{"foo/bar/baz"}}, objs)
		suite.Assert().Equal([]SharedPrefix{}, prefixes)
	}

	// List all keys again (there should be 3)
	bucketRoot := suite.locationSpec.ToBucket("")
	objs, prefixes, err := bucketRoot.List(ctx, "", false)
	suite.Require().NoError(err)

	suite.Assert().Equal([]BucketObject{
		{"foo/bar/baz"},
		{"root1/foo/bar/baz"},
		{"root2/root3/foo/bar/baz"},
	}, objs)
	suite.Assert().Equal([]SharedPrefix{}, prefixes)
}

func (suite *BucketTestSuite) TestDeleteSingle() {
	ctx := context.Background()
	bucketRoot := suite.locationSpec.ToBucket("")
	w, err := bucketRoot.NewWriter(ctx, "delete_test_single/obj1")
	suite.Require().NoError(err)

	_, err = w.Write([]byte("hello"))
	suite.Require().NoError(err)

	err = w.Close()
	suite.Require().NoError(err)

	err = bucketRoot.Delete(ctx, []string{"delete_test_single/obj1"})
	suite.Require().NoError(err)

	objs, sharedPrefixes, err := bucketRoot.List(ctx, "", true)
	suite.Require().NoError(err)
	suite.Assert().Empty(objs, "there should not object keys because the bucket is empty")
	suite.Assert().Empty(sharedPrefixes, "there should be no shared keys because the bucket is empty")

	_, err = bucketRoot.NewReader(ctx, "delete_test_single/obj1", &NoOpObjectVerifier{})
	suite.Require().Error(err)
	suite.Assert().True(IsNotExist(err))
}

func (suite *BucketTestSuite) TestDeleteMultiple() {
	ctx := context.Background()
	bucketRoot := suite.locationSpec.ToBucket("")
	w, err := bucketRoot.NewWriter(ctx, "delete_test_multiple/obj1")
	suite.Require().NoError(err)

	_, err = w.Write([]byte("hello"))
	suite.Require().NoError(err)

	err = w.Close()
	suite.Require().NoError(err)

	w, err = bucketRoot.NewWriter(ctx, "delete_test_multiple/obj2")
	suite.Require().NoError(err)

	_, err = w.Write([]byte("hello"))
	suite.Require().NoError(err)

	err = w.Close()
	suite.Require().NoError(err)

	err = bucketRoot.Delete(ctx, []string{"delete_test_multiple/obj1", "delete_test_multiple/obj2"})
	suite.Require().NoError(err)

	objs, sharedPrefixes, err := bucketRoot.List(ctx, "", true)
	suite.Require().NoError(err)
	suite.Assert().Empty(objs, "there should not object keys because the bucket is empty")
	suite.Assert().Empty(sharedPrefixes, "there should be no shared keys because the bucket is empty")

	_, err = bucketRoot.NewReader(ctx, "delete_test_multiple/obj1", &NoOpObjectVerifier{})
	suite.Require().Error(err)
	suite.Assert().True(IsNotExist(err))

	_, err = bucketRoot.NewReader(ctx, "delete_test_multiple/obj2", &NoOpObjectVerifier{})
	suite.Require().Error(err)
	suite.Assert().True(IsNotExist(err))
}

func (suite *BucketTestSuite) BeforeTest(suiteName, testName string) {
	if suite.beforeTest != nil {
		suite.beforeTest(suite)
	}
}

func (suite *BucketTestSuite) AfterTest(suiteName, testName string) {
	if suite.afterTest != nil {
		suite.afterTest(suite)
	}
}

func TestFilesystemBucket(t *testing.T) {
	suite.Run(t, &BucketTestSuite{
		beforeTest: func(suite *BucketTestSuite) {
			tmpDir, err := ioutil.TempDir("", "fs-bucket-test")
			suite.Require().NoError(err)
			suite.locationSpec = FilesystemLocationSpecification{Path: tmpDir}
			suite.afterTest = func(*BucketTestSuite) {
				os.RemoveAll(tmpDir)
			}
		},
	})
}

/*
I'm checking in this commented code because I think it'll be useful if someone ever wants to
make changes to the S3 implementation. Just spin up minio and set the endpoint to that. The
reason its not running by default is because I can't find a golang implementation of S3 that
can be embedded. Mocking enough out to make it work would just be a lie.
*/

/*
func TestS3Bucket(t *testing.T) {
	i := 0
	suite.Run(t, &BucketTestSuite{
		beforeTest: func(suite *BucketTestSuite) {
			i++
			bucketName := "testing-" + strconv.Itoa(i)
			fmt.Println(bucketName)
			suite.locationSpec = S3LocationSpecification{
				BucketName: bucketName,
				Endpoint:   "http://localhost:10143",
				AccessKey:  "minio",
				SecretKey:  "miniosecret",
			}
		},
	})
}
*/
