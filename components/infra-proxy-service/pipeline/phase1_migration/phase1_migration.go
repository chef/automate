package phase1_migration

import (
	"context"
	"fmt"
)

type Result struct {
	Meta         *Meta
	ParsedResult *ParsedResult
	Context      context.Context
}

type Meta struct {
	StageResults []StageResult
}

type StageResult struct {
	StageName string
	IsSuccess bool
	Failure   error
}

type ParsedResult struct {
	Orgs      []Org
	Users     []User
	OrgsUsers []OrgsUsersAssociations
	KeyDump   KeyDump
}

// type OrgsUsersAssociations map[User][]Org
type OrgsUsersAssociations struct {
	OrgName Org
	Users   []User
}

type ActionOps int

const (
	Insert ActionOps = 1 + iota
	Skip
	Delete
	Update
)

type MigrationPipe func(<-chan Result) <-chan Result

// Unzip returns all the files of the zipped file

// --> Unzip --> ParseOrgs --> ParseUsers --> ........ --> CompletePhase1 --> Finish Pipeline ( Convert struct to JSON, store JSON to stage table, Update the status)
//      |---------|---------------|------------------------------------------> Error Handle

// --> StageTableReadJSON --> PopulateOrgs --> PopulateUsers --> ........ --> CompletePhase2 --> Finish Pipeline ( Update the status)
//      |---------|---------------|------------------------------------------> Error Handle

func UnzipSrc(src string) MigrationPipe {
	return func(in <-chan Result) <-chan Result {
		return unzip(context.Background(), src)
	}
}

// func unZip(ctx context.Context, src <-chan string, errc <-chan error) <-chan *Result
func unzip(ctx context.Context, src string) <-chan Result {
	result := make(chan Result)
	go func() {
		defer close(result)
		fmt.Println("Zip function is called!")
	}()
	return result
}

// ParseOrg returns MigrationPipe
func ParseOrg(res <-chan Result) MigrationPipe {
	return func(in <-chan Result) <-chan Result {
		return parseOrg(context.Background(), res)
	}
}

func parseOrg(ctx context.Context, result <-chan Result) <-chan Result {
	go func() {
		fmt.Println("ParseOrg routine is called!")
	}()
	return result
}

// ParseUser returns MigrationPipe
func ParseUser(res <-chan Result) MigrationPipe {
	return func(in <-chan Result) <-chan Result {
		return parseUser(context.Background(), res)
	}
}

func parseUser(ctx context.Context, result <-chan Result) <-chan Result {
	go func() {
		fmt.Println("ParseUser routine is called!")
	}()
	return result
}

// ConflictingUsers returns MigrationPipe
func ConflictingUsers(res <-chan Result) MigrationPipe {
	return func(in <-chan Result) <-chan Result {
		return conflictingUsers(context.Background(), res)
	}
}

func conflictingUsers(ctx context.Context, result <-chan Result) <-chan Result {
	go func() {
		fmt.Println("Conflicting routine is called!")
	}()
	return result
}

// OrgMembers returns MigrationPipe
func OrgMembers(res <-chan Result) MigrationPipe {
	return func(in <-chan Result) <-chan Result {
		return orgMembers(context.Background(), res)
	}
}

func orgMembers(ctx context.Context, result <-chan Result) <-chan Result {
	go func() {
		fmt.Println("orgMembers routine is called!")
	}()
	return result
}

// AdminUsers Return MigrationPipe
func AdminUsers(res <-chan Result) MigrationPipe {
	return func(in <-chan Result) <-chan Result {
		return adminUsers(context.Background(), res)
	}
}

func adminUsers(ctx context.Context, result <-chan Result) <-chan Result {
	go func() {
		fmt.Println("adminUsers routine is called!")
	}()
	return result
}

// AdminUsers Return MigrationPipe
func ParseKeyDump(res <-chan Result) MigrationPipe {
	return func(in <-chan Result) <-chan Result {
		return parseKeyDump(context.Background(), res)
	}
}

// ParseKeyDump returns Key Dump
func parseKeyDump(ctx context.Context, result <-chan Result) <-chan Result {
	go func() {
		fmt.Println("parseKeyDump routine is called!")
	}()
	return result
}

func migrationPipeline(source <-chan Result, pipes ...MigrationPipe) {
	fmt.Println("Pipeline started. Waiting for pipeline to complete.")
	msg := make(chan string)
	go func() {
		for _, pipe := range pipes {
			source = pipe(source)
		}

		for s := range source {
			fmt.Println(s)
		}
		msg <- "Done"
	}()

	fmt.Println("Pipeline Status: ", <-msg)
}

func RunmigrationPipeline() {
	c := make(chan Result)

	migrationPipeline(c,
		UnzipSrc(""),
		ParseOrg(c),
		ParseUser(c),
		ConflictingUsers(c),
		OrgMembers(c),
		AdminUsers(c),
		ParseKeyDump(c),
	)
}
