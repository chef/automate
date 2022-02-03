package pipeline

import (
	"context"
	"fmt"
)

type PipelineData struct {
	Result Result
	Done   chan<- error
	Ctx    context.Context
}

type PhaseOnePipleine struct {
	in chan<- PipelineData
}

type PhaseOnePipelineProcessor func(<-chan PipelineData) <-chan PipelineData

// ParseOrg returns PhaseOnePipelineProcessor
func UnzipSrc() PhaseOnePipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return unzip(result)
	}
}

func unzip(result <-chan PipelineData) <-chan PipelineData {
	fmt.Println("Starting unzip pipeline")
	out := make(chan PipelineData, 100)
	go func() {

		for res := range result {
			res.Result.Meta.UnzipFolder = "backup"
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		fmt.Println("Closing unzip")
		close(out)
	}()
	return out
}

// ParseOrg returns PhaseOnePipelineProcessor
func ParseOrg() PhaseOnePipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return parseOrg(result)
	}
}

func parseOrg(result <-chan PipelineData) <-chan PipelineData {
	fmt.Println("Starting parse orgs pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		fmt.Println("Processing to parse orgs...")
		for res := range result {
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
			fmt.Println("after write")
		}
		fmt.Println("CLosing orgs pipeline")
		close(out)
	}()
	return out
}

// ParseUser returns PhaseOnePipelineProcessor
func ParseUser() PhaseOnePipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return parseUser(result)
	}
}

func parseUser(result <-chan PipelineData) <-chan PipelineData {
	fmt.Println("Starting parse user pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		fmt.Println("Processing to parsing users...")
		for res := range result {
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		fmt.Println("Closing parseUser")
		close(out)
	}()

	return out
}

// ConflictingUsers returns PhaseOnePipelineProcessor
func ConflictingUsers() PhaseOnePipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return conflictingUsers(result)
	}
}

func conflictingUsers(result <-chan PipelineData) <-chan PipelineData {
	fmt.Println("Starting conflicting user check pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		fmt.Println("Processing to check conflicting users...")

		for res := range result {
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		fmt.Println("Closing conflictUser")
		close(out)

	}()

	return out
}

// OrgMembers returns PhaseOnePipelineProcessor
func OrgMembers() PhaseOnePipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return orgMembers(result)
	}
}

func orgMembers(result <-chan PipelineData) <-chan PipelineData {
	fmt.Println("Starting org user check pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		fmt.Println("Processing to check org users association...")
		for res := range result {
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		fmt.Println("Closing orgmember")
		close(out)

	}()

	return out
}

// AdminUsers Return PhaseOnePipelineProcessor
func AdminUsers() PhaseOnePipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return adminUsers(result)
	}
}

func adminUsers(result <-chan PipelineData) <-chan PipelineData {
	fmt.Println("Starting org admin user check pipeline")

	out := make(chan PipelineData, 100)

	go func() {
		fmt.Println("Processing to to check admin users...")
		for res := range result {
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		fmt.Println("Closing adminUsers")
		close(out)

	}()

	return out
}

func migrationPipeline(source <-chan PipelineData, pipes ...PhaseOnePipelineProcessor) {
	fmt.Println("Pipeline started...")

	go func() {
		for _, pipe := range pipes {
			source = pipe(source)
		}

		for s := range source {
			s.Done <- nil
		}
	}()
}

func SetupPhaseOnePipeline() PhaseOnePipleine {
	c := make(chan PipelineData, 100)
	migrationPipeline(c,
		UnzipSrc(),
		ParseOrg(),
		ParseUser(),
		ConflictingUsers(),
		OrgMembers(),
		AdminUsers(),
	)
	return PhaseOnePipleine{in: c}
}

func (p *PhaseOnePipleine) Run(result Result) {
	go func() {
		ctx, cancel := context.WithCancel(context.Background())
		defer cancel()
		done := make(chan error)
		select {
		case p.in <- PipelineData{Result: result, Done: done, Ctx: ctx}:
		}
		err := <-done
		if err != nil {
			fmt.Println("received error")
		}
		fmt.Println("received done")
	}()
}
