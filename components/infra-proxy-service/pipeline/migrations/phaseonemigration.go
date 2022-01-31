package migrations

import (
	"context"
	"fmt"

	"github.com/chef/automate/components/infra-proxy-service/pipeline"
)

type MigrationPipe func(<-chan pipeline.Result) <-chan pipeline.Result

// ParseOrg returns MigrationPipe
func UnzipSrc(res <-chan pipeline.Result) MigrationPipe {
	return func(in <-chan pipeline.Result) <-chan pipeline.Result {
		return unzip(context.Background(), res)
	}
}

func unzip(ctx context.Context, result <-chan pipeline.Result) <-chan pipeline.Result {
	go func() {
		fmt.Println("Zip function is called!")
	}()
	return result
}

// ParseOrg returns MigrationPipe
func ParseOrg(res <-chan pipeline.Result) MigrationPipe {
	return func(in <-chan pipeline.Result) <-chan pipeline.Result {
		return parseOrg(context.Background(), res)
	}
}

func parseOrg(ctx context.Context, result <-chan pipeline.Result) <-chan pipeline.Result {
	go func() {
		fmt.Println("ParseOrg routine is called!")
	}()
	return result
}

// ParseUser returns MigrationPipe
func ParseUser(res <-chan pipeline.Result) MigrationPipe {
	return func(in <-chan pipeline.Result) <-chan pipeline.Result {
		return parseUser(context.Background(), res)
	}
}

func parseUser(ctx context.Context, result <-chan pipeline.Result) <-chan pipeline.Result {
	go func() {
		fmt.Println("ParseUser routine is called!")
	}()
	return result
}

// ConflictingUsers returns MigrationPipe
func ConflictingUsers(res <-chan pipeline.Result) MigrationPipe {
	return func(in <-chan pipeline.Result) <-chan pipeline.Result {
		return conflictingUsers(context.Background(), res)
	}
}

func conflictingUsers(ctx context.Context, result <-chan pipeline.Result) <-chan pipeline.Result {
	go func() {
		fmt.Println("Conflicting routine is called!")
	}()
	return result
}

// OrgMembers returns MigrationPipe
func OrgMembers(res <-chan pipeline.Result) MigrationPipe {
	return func(in <-chan pipeline.Result) <-chan pipeline.Result {
		return orgMembers(context.Background(), res)
	}
}

func orgMembers(ctx context.Context, result <-chan pipeline.Result) <-chan pipeline.Result {
	go func() {
		fmt.Println("orgMembers routine is called!")
	}()
	return result
}

// AdminUsers Return MigrationPipe
func AdminUsers(res <-chan pipeline.Result) MigrationPipe {
	return func(in <-chan pipeline.Result) <-chan pipeline.Result {
		return adminUsers(context.Background(), res)
	}
}

func adminUsers(ctx context.Context, result <-chan pipeline.Result) <-chan pipeline.Result {
	go func() {
		fmt.Println("adminUsers routine is called!")
	}()
	return result
}

// AdminUsers Return MigrationPipe
func ParseKeyDump(res <-chan pipeline.Result) MigrationPipe {
	return func(in <-chan pipeline.Result) <-chan pipeline.Result {
		return parseKeyDump(context.Background(), res)
	}
}

// ParseKeyDump returns Key Dump
func parseKeyDump(ctx context.Context, result <-chan pipeline.Result) <-chan pipeline.Result {
	go func() {
		fmt.Println("parseKeyDump routine is called!")
	}()
	return result
}

func migrationPipeline(source <-chan pipeline.Result, pipes ...MigrationPipe) {
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

func RunPhaseOnePipeline(src string) {
	c := make(chan pipeline.Result, 1)

	c <- pipeline.Result{}
	defer close(c)

	migrationPipeline(c,
		UnzipSrc(c),
		ParseOrg(c),
		ParseUser(c),
		ConflictingUsers(c),
		OrgMembers(c),
		AdminUsers(c),
	)
}
