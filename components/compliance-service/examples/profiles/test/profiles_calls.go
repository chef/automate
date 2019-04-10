package profilestest

import (
	"context"
	"io"
	"io/ioutil"
	"log"

	ps "github.com/chef/automate/components/compliance-service/api/profiles"
	"github.com/chef/automate/components/compliance-service/examples/helpers"
)

const (
	address = "localhost:10121"
)

func Upload(file string, owner string, fileType string) (*ps.CheckResult, error) {
	connFactory := helpers.SecureConnFactory()
	conn, err := connFactory.Dial("compliance-service", address)
	if err != nil {
		log.Fatalf("did not connect: %v", err)
	}
	defer conn.Close()

	profiles := ps.NewProfilesServiceClient(conn)

	fileData, err := ioutil.ReadFile(file)
	stream, err := profiles.Create(context.Background())
	if err != nil {
		log.Printf("%v.Create(_) = _, %v", profiles, err)
		return nil, err
	}

	request := ps.ProfilePostRequest{
		Owner: owner,
		Chunk: &ps.Chunk{Data: fileData},
		Meta:  &ps.Metadata{ContentType: fileType},
	}

	err = stream.Send(&request)
	reply, err := stream.CloseAndRecv()
	if err != nil {
		log.Printf("%v.CloseAndRecv() got error %v, want %v", stream, err, nil)
		return nil, err
	}
	return reply, nil
}

func Read(owner string, name string, version string) (*ps.Profile, error) {
	connFactory := helpers.SecureConnFactory()
	conn, err := connFactory.Dial("compliance-service", address)
	if err != nil {
		log.Printf("did not connect: %v", err)
		return nil, err
	}
	defer conn.Close()

	profiles := ps.NewProfilesServiceClient(conn)

	profile, err := profiles.Read(context.Background(), &ps.ProfileDetails{Name: name, Owner: owner, Version: version})
	if err != nil {
		log.Printf("could not read profile: %v", err)
		return nil, err
	}
	return profile, nil
}

func ReadTar(owner string, name string, version string) (*ps.ProfileData, error) {
	connFactory := helpers.SecureConnFactory()
	conn, err := connFactory.Dial("compliance-service", address)
	if err != nil {
		log.Fatalf("did not connect: %v", err)
	}
	defer conn.Close()

	profiles := ps.NewProfilesServiceClient(conn)

	var tdata *ps.ProfileData
	stream1, err1 := profiles.ReadTar(context.Background(), &ps.ProfileDetails{Name: name, Version: version, Owner: owner})
	if err1 != nil {
		log.Fatalf("could not read tar: %v", err1)
	}
	for {
		data, err := stream1.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			log.Fatalf("%v.ReadTar(_) = _, %v", profiles, err)
		}
		tdata = data
	}
	return tdata, nil
}
