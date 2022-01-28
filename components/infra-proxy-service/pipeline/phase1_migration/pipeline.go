package phase1_migration

import (
	"archive/zip"
	"context"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	"github.com/pkg/errors"
)

// Meta --> About zip file, unzipped location
//          Overall Result
// Orgs --> [ {
//   orgname:
//   fullname:
// }]
// Users --> [ {
//   serverusername:
//	   ldapusername:
//     automateusername: // if ldapusername != "" then ldapusername or serverusername
//		   isconflicting: // if ldapusername != "" then false or check with auth service
//       hashKey: // store for password only if ldapusername != ""
//}
//   ]
// OrgsUsersAssoiat --> orgs [
//   {
//	   orgname: <orgname>
//		   users: [
//	   {
//		   username: <username>,
//			   isadmin: true/false
//	   }
//	   ],
//}
//]

type Result struct {
	meta         *Meta
	parsedResult *ParsedReseult
}

type Meta struct {
	stageResults []StageResult
}

type StageResult struct {
	stageName string
	isSuccess bool
	failure   error
}

type ParsedResult struct {
	orgs      Organisations
	users     Users
	orgsUsers OrgsUsersAssociations
}

type Org struct {
	Name     string    `json:"name"`
	FullName string    `json:"full_name"`
	Action   ActionOps // This to identify if the org needs to be added, removed, updated, skipped
}

type ActionOps int

const (
	Insert ActionOps = 1 + iota
	Skip
	Delete
	Update
)

type ActionOps struct {
}

// Unzip returns all the files of the zipped file

// --> Unzip --> ParseOrgs --> ParseUsers --> ........ --> CompletePhase1 --> Finish Pipeline ( Convert struct to JSON, store JSON to stage table, Update the status)
//      |---------|---------------|------------------------------------------> Error Handle

// --> StageTableReadJSON --> PopulateOrgs --> PopulateUsers --> ........ --> CompletePhase2 --> Finish Pipeline ( Update the status)
//      |---------|---------------|------------------------------------------> Error Handle

// func unZip(ctx context.Context, src <-chan string, errc <-chan error) <-chan *Result
func Unzip(ctx context.Context, src string) (<-chan string, <-chan error) {
	filePath := make(chan string)
	errc := make(chan error, 1)
	go func() {
		r, err := zip.OpenReader(src)
		if err != nil {
			fmt.Println("cannot open reader")
			ctx.Done()
		}
		defer r.Close()
		defer close(filePath)
		defer close(errc)

		for _, f := range r.File {
			fpath := filepath.Join("", f.Name)

			// Checking for any invalid file paths
			if !strings.HasPrefix(fpath, filepath.Clean("backup")+string(os.PathSeparator)) {
				errc <- errors.Errorf("invalid path")
				ctx.Done()
			}

			// filenames = append(filenames, fpath)

			if f.FileInfo().IsDir() {
				os.MkdirAll(fpath, os.ModePerm)
				continue
			}

			// Creating the files in the target directory
			if err = os.MkdirAll(filepath.Dir(fpath), os.ModePerm); err != nil {
				errc <- errors.Errorf("cannot create dir")
				ctx.Done()
			}

			// The created file will be stored in
			// outFile with permissions to write &/or truncate
			outFile, err := os.OpenFile(fpath,
				os.O_WRONLY|os.O_CREATE|os.O_TRUNC,
				f.Mode())
			if err != nil {
				errc <- errors.Errorf("cannot create a file")
				ctx.Done()
			}

			rc, err := f.Open()
			if err != nil {
				errc <- errors.Errorf("cannot open file")
				ctx.Done()
			}

			_, err = io.Copy(outFile, rc)

			outFile.Close()
			rc.Close()

			if err != nil {
				errc <- errors.Errorf("cannot copy a file")
				ctx.Done()
			}

			select {
			case filePath <- fpath:
			case <-ctx.Done():
				return
			}
		}

	}()
	return filePath, errc
}

// ParseOrg returns all the organizations from the unzipped file
func ParseOrg(ctx context.Context, fileDest ...<-chan interface{}) (<-chan Org, <-chan error) {
	orgChan := make(chan Org)
	errc := make(chan error, 1)

	go func() {
		defer close(orgChan)
		defer close(errc)
		// org OrhStruct := YashviOrgsfunction(folderPath string)
		// in.org = &org
		orgChan <- Org{Name: "Org Alpha"}
		orgChan <- Org{Name: "Org Beta"}
	}()
	return orgChan, errc
}

// ParseKeyDump returns Key Dump
func ParseKeyDump(ctx context.Context, fileDest <-chan string) (<-chan KeyDump, <-chan error) {
	keyDump := make(chan KeyDump)
	errc := make(chan error, 1)

	go func() {
		defer close(keyDump)
		defer close(errc)

		keyDump <- KeyDump{
			Admin: true,
		}
	}()
	return keyDump, errc
}

// ParseUser returns all the users from the unzipped file
func ParseUser(ctx context.Context, fileDest <-chan string) (<-chan User, <-chan error) {
	userChan := make(chan User)
	errc := make(chan error, 1)

	go func() {
		defer close(userChan)
		defer close(errc)

		userChan <- User{Username: "Alpha"}
		userChan <- User{Username: "Beta"}
	}()
	return userChan, errc
}

// ConflictingUsers returns users who are already stored into the database
func ConflictingUsers(ctx context.Context, user <-chan User) (<-chan User, <-chan error) {
	confUser := make(chan User)
	errc := make(chan error, 1)

	go func() {
		defer close(confUser)
		defer close(errc)

		// Check if the user is in the DB
		confUser <- User{Username: "Gamma"}
	}()
	return confUser, errc
}

// OrgMembers returns users and organizations associated to them.
func OrgMembers(ctx context.Context, user <-chan User) (<-chan map[User][]Org, <-chan error) {
	userOrg := make(chan map[User][]Org)
	errc := make(chan error, 1)

	go func() {
		defer close(userOrg)
		defer close(errc)

		userOrgMap := map[User][]Org{}
		userOrg <- userOrgMap
	}()
	return userOrg, errc
}

// AdminUsers Return all the users who has admin privilege
func AdminUsers(ctx context.Context, user <-chan User) (<-chan User, <-chan error) {
	adminUser := make(chan User)
	errc := make(chan error, 1)

	go func() {
		defer close(adminUser)
		defer close(errc)

		// Check if the user is in the DB
		adminUser <- User{Username: "Delta"}
		errc <- errors.Errorf("asd path")
	}()
	return adminUser, errc
}

func RunMigrationPipeline(filePath string) {
	fmt.Println("Pipeline started. Waiting for pipeline to complete.")

	var errc <-chan error
	ctx, cancelFunc := context.WithCancel(context.Background())
	defer cancelFunc()

	// Unzipping
	ch, errc := Unzip(ctx, filePath)
	errcList = append(errcList, errc)

	orgs, errc := ParseOrg(ctx, ch)
	errcList = append(errcList, errc)

	keyDump, errc := ParseKeyDump(ctx, ch)
	errcList = append(errcList, errc)

	users, errc := ParseUser(ctx, ch)
	errcList = append(errcList, errc)

	existingUsers, errc := ConflictingUsers(ctx, users)
	errcList = append(errcList, errc)

	orgsUser, errc := OrgMembers(ctx, users)
	errcList = append(errcList, errc)

	adminUsers, errc := AdminUsers(ctx, users)
	errcList = append(errcList, errc)

	// orgs
	for org := range orgs {
		fmt.Println("Organization: ", org)
	}
	fmt.Println()

	// Keydump
	for kd := range keyDump {
		fmt.Println("Keydump: ", kd)
	}
	fmt.Println()

	// Existing user
	for eu := range existingUsers {
		fmt.Println("Existing user: ", eu)
	}
	fmt.Println()

	// User's Org
	for ou := range orgsUser {
		fmt.Println("User's Org: ", ou)
	}

	fmt.Println()
	// Admin User
	for au := range adminUsers {
		fmt.Println("Admin User: ", au)
	}

	// Log all the errors stored in errcList variable

}
