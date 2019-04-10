package main

import (
	"flag"
	"fmt"
	"log"

	profilestest "github.com/chef/automate/components/compliance-service/examples/profiles/test"
)

const (
	address = "localhost:10121"
)

func main() {
	filePtr := flag.String("file", "", "File to upload")
	ownerPtr := flag.String("owner", "fake", "Owner to upload file to")
	fileTypePtr := flag.String("file-type", "application/x-gtar", "File type being uploaded")
	namePtr := flag.String("name", "linux", "Name of profile to retrieve")
	versionPtr := flag.String("version", "1.1.0", "Version of profile to retrieve")
	flag.Parse()
	fmt.Printf("Uploading file: %s", *filePtr)

	res, err := profilestest.Upload(*filePtr, *ownerPtr, *fileTypePtr)
	if err != nil {
		log.Printf("error, %v", err)
	}
	log.Printf("CheckResult, %v", res)

	res1, err1 := profilestest.Read(*ownerPtr, *namePtr, *versionPtr)
	if err1 != nil {
		log.Printf("error, %v", err1)
	}
	log.Printf("Profile, %v", res1)

	res2, err2 := profilestest.ReadTar(*ownerPtr, *namePtr, *versionPtr)
	if err2 != nil {
		log.Printf("error, %v", err2)
	}
	log.Printf("Profile Tar, %v", res2)
}
