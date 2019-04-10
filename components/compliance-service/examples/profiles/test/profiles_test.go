package profilestest

import (
	"os"
	"reflect"
	"testing"

	"github.com/chef/automate/components/compliance-service/api/profiles"
)

func TestUpload(t *testing.T) {
	t.Log("Testing the upload, expecting success!")
	dir, err := os.Getwd()
	if err != nil {
		t.Error("Could not read directory. Cannot access profile")
		t.Fail()
	}
	dir = dir + "/linux.tar.gz"
	t.Log("file", dir)
	res, err := Upload(dir, "fake", "application/x-gtar")
	if err != nil {
		t.Errorf("Expected a check result message, got error %v", err)
		t.Fail()
	}
	checkResult := profiles.CheckResult{}
	if reflect.TypeOf(res) != reflect.TypeOf(&checkResult) {
		t.Errorf("Expected a check result message, got %v", res)
		t.Fail()
	}
}

func TestRead(t *testing.T) {
	t.Log("Testing the read, expecting success!")
	res, err := Read("fake", "linux", "1.1.0")
	if err != nil {
		t.Errorf("Expected a profile, got error %v", err)
		t.Fail()
	}
	profile := profiles.Profile{}
	if reflect.TypeOf(res) != reflect.TypeOf(&profile) {
		t.Errorf("Expected a profile, got %v", res)
		t.Fail()
	}
	t.Logf("Profile: %v", res)
}

func TestReadTar(t *testing.T) {
	t.Log("Testing the read_tar, expecting success!")
	res, err := ReadTar("fake", "linux", "1.1.0")
	if err != nil {
		t.Errorf("Expected a profile tar, got error %v", err)
		t.Fail()
	}
	profile := profiles.ProfileData{}
	if reflect.TypeOf(res) != reflect.TypeOf(&profile) {
		t.Errorf("Expected profile data, got %v", res)
		t.Fail()
	}
	t.Log("Profile Data Retrieved")
}
