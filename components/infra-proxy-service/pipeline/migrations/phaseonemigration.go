package main

import (
	"context"
	"fmt"
	"github.com/chef/automate/components/infra-proxy-service/pipeline"
	"time"
)

type PipelineData struct {
	Result pipeline.Result
	Done   chan<- error
}
type MigrationPipe func(context.Context, <-chan PipelineData) <-chan PipelineData

// ParseOrg returns MigrationPipe
func UnzipSrc(ctx context.Context) MigrationPipe {
	return func(c context.Context, result <-chan PipelineData) <-chan PipelineData {
		return unzip(ctx, result)
	}
}

func unzip(ctx context.Context, result <-chan PipelineData) <-chan PipelineData {
	fmt.Println("Starting unzip pipeline")
	out := make(chan PipelineData, 100)
	go func() {

		for res := range result {
			res.Result.Meta.UnzipFolder = "backup"
			select {
			case out <- res:
			case <-ctx.Done():
				break
			}
		}
		fmt.Println("Closing unzip")
		close(out)
	}()
	return out
}

// ParseOrg returns MigrationPipe
func ParseOrg(ctx context.Context) MigrationPipe {
	return func(c context.Context, result <-chan PipelineData) <-chan PipelineData {
		return parseOrg(ctx, result)
	}
}

func parseOrg(ctx context.Context, result <-chan PipelineData) <-chan PipelineData {
	fmt.Println("Starting parse orgs pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		fmt.Println("Processing to parse orgs...")
		for res := range result {
			// res.Done <- fmt.Errorf("Error")
			// continue
			select {
			case out <- res:
			case <-ctx.Done():
				break
			}
			fmt.Println("after write")
		}
		fmt.Println("CLosing orgs pipeline")
		close(out)
	}()
	return out
}

// ParseUser returns MigrationPipe
func ParseUser() MigrationPipe {
	return func(ctx context.Context, result <-chan PipelineData) <-chan PipelineData {
		return parseUser(ctx, result)
	}
}

func parseUser(ctx context.Context, result <-chan PipelineData) <-chan PipelineData {
	fmt.Println("Starting parse user pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		fmt.Println("Processing to parsing users...")
		for res := range result {
			select {
			case out <- res:
			case <-ctx.Done():
				break
			}
		}
		fmt.Println("Closing parseUser")
		close(out)
	}()

	return out
}

// ConflictingUsers returns MigrationPipe
func ConflictingUsers(ctx context.Context) MigrationPipe {
	return func(ctx context.Context, result <-chan PipelineData) <-chan PipelineData {
		return conflictingUsers(ctx, result)
	}
}

func conflictingUsers(ctx context.Context, result <-chan PipelineData) <-chan PipelineData {
	fmt.Println("Starting conflicting user check pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		fmt.Println("Processing to check conflicting users...")

		for res := range result {
			select {
			case out <- res:
			case <-ctx.Done():
				break
			}
		}
		fmt.Println("Closing conflictUser")
		close(out)

	}()

	return out
}

// OrgMembers returns MigrationPipe
func OrgMembers() MigrationPipe {
	return func(ctx context.Context, result <-chan PipelineData) <-chan PipelineData {
		return orgMembers(ctx, result)
	}
}

func orgMembers(ctx context.Context, result <-chan PipelineData) <-chan PipelineData {
	fmt.Println("Starting org user check pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		fmt.Println("Processing to check org users association...")
		for res := range result {
			select {
			case out <- res:
			case <-ctx.Done():
				break
			}
		}
		fmt.Println("Closing orgmember")
		close(out)

	}()

	return out
}

// AdminUsers Return MigrationPipe
func AdminUsers() MigrationPipe {
	return func(ctx context.Context, result <-chan PipelineData) <-chan PipelineData {
		return adminUsers(ctx, result)
	}
}

func adminUsers(ctx context.Context, result <-chan PipelineData) <-chan PipelineData {
	fmt.Println("Starting org admin user check pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		fmt.Println("Processing to to check admin users...")
		for res := range result {
			select {
			case out <- res:
			case <-ctx.Done():
				break
			}
		}
		fmt.Println("Closing adminUsers")
		close(out)

	}()

	return out
}

func migrationPipeline(ctx context.Context, source <-chan PipelineData, pipes ...MigrationPipe) {
	fmt.Println("Pipeline started...")

	go func() {
		for _, pipe := range pipes {
			source = pipe(ctx, source)
		}

		for s := range source {
			s.Done <- nil
		}
	}()
}

func RunPhaseOnePipeline(ctx context.Context) chan<- PipelineData {
	c := make(chan PipelineData, 100)
	migrationPipeline(ctx, c,
		UnzipSrc(ctx),
		ParseOrg(ctx),
		ParseUser(),
		ConflictingUsers(ctx),
		OrgMembers(),
		AdminUsers(),
	)
	return c
}

func Run(msg chan string) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	c := RunPhaseOnePipeline(ctx)
	done := make(chan error)
	time.Sleep(2 * time.Second)
	select {
	case c <- PipelineData{Result: pipeline.Result{}, Done: done}:
	}
	err := <-done
	fmt.Println("received done")
	if err != nil {
		fmt.Println("received error")
	}
	close(c)
	time.Sleep(2 * time.Second)
}

func main() {
	fmt.Println("Pipeline running")
	msg := make(chan string)
	Run(msg)
}
