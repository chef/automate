package pipeline

import (
	"context"
	"fmt"
)

type PhaseTwoPipleine struct {
	in chan<- PipelineData
}

type PhaseTwoPipelineProcessor func(<-chan PipelineData) <-chan PipelineData

// PopulateOrgs returns PhaseTwoPipelineProcessor
func PopulateOrgs() PhaseTwoPipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return populateOrgs(result)
	}
}

func populateOrgs(result <-chan PipelineData) <-chan PipelineData {
	fmt.Println("Starting populateOrgs routine")
	out := make(chan PipelineData, 100)

	go func() {
		for res := range result {
			fmt.Println("Processing to populateOrgs...")
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		fmt.Println("Closing populateOrgs routine")
		close(out)
	}()
	return out
}

// PopulateOrgs returns PhaseTwoPipelineProcessor
func CreateProject() PhaseTwoPipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return createProject(result)
	}
}

func createProject(result <-chan PipelineData) <-chan PipelineData {
	fmt.Println("Starting CreateProject routine")
	out := make(chan PipelineData, 100)

	go func() {
		for res := range result {
			fmt.Println("Processing to createProject...")
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		fmt.Println("Closing CreateProject routine")
		close(out)
	}()
	return out
}

// PopulateUsers returns PhaseTwoPipelineProcessor
func PopulateUsers() PhaseTwoPipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return populateUsers(result)
	}
}

func populateUsers(result <-chan PipelineData) <-chan PipelineData {
	fmt.Println("Starting PopulateUsers routine")
	out := make(chan PipelineData, 100)

	go func() {
		for res := range result {
			fmt.Println("Processing to populateUsers...")
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		fmt.Println("Closing PopulateUsers routine")
		close(out)
	}()
	return out
}

// PopulateORGUser returns PhaseTwoPipelineProcessor
func PopulateORGUser() PhaseTwoPipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return populateORGUser(result)
	}
}

func populateORGUser(result <-chan PipelineData) <-chan PipelineData {
	fmt.Println("Starting PopulateORGUser routine")
	out := make(chan PipelineData, 100)

	go func() {
		for res := range result {
			fmt.Println("Processing to populateORGUser...")
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		fmt.Println("Closing PopulateORGUser routine")
		close(out)
	}()
	return out
}

// PopulateMembersPolicy returns PhaseTwoPipelineProcessor
func PopulateMembersPolicy() PhaseTwoPipelineProcessor {
	return func(result <-chan PipelineData) <-chan PipelineData {
		return populateMembersPolicy(result)
	}
}

func populateMembersPolicy(result <-chan PipelineData) <-chan PipelineData {
	fmt.Println("Starting PopulateMembersPolicy routine")
	out := make(chan PipelineData, 100)

	go func() {
		for res := range result {
			fmt.Println("Processing to populateMembersPolicy...")
			select {
			case out <- res:
			case <-res.Ctx.Done():
				res.Done <- nil
			}
		}
		fmt.Println("Closing PopulateMembersPolicy routine")
		close(out)
	}()
	return out
}

func migrationTwoPipeline(source <-chan PipelineData, pipes ...PhaseTwoPipelineProcessor) {
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

func SetupPhaseTwoPipeline() PhaseTwoPipleine {
	c := make(chan PipelineData, 100)
	migrationTwoPipeline(c,
		PopulateOrgs(),
		CreateProject(),
		PopulateUsers(),
		PopulateORGUser(),
		PopulateMembersPolicy(),
	)
	return PhaseTwoPipleine{in: c}
}

func (p *PhaseTwoPipleine) Run(result Result) {
	status := make(chan string)
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
		status <- "Done"
	}()
	<-status
}
