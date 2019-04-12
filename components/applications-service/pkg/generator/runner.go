package generator

import (
	"fmt"
	"math/rand"
	"strings"
	"time"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/components/applications-service/pkg/nats"
	uuid "github.com/chef/automate/lib/uuid4"
)

type RunnerConfig struct {
	AuthToken string
	Host      string
	Tick      int
}

type LoadGenRunner struct {
	SupervisorGroups []*SupervisorGroup
}

func (r *LoadGenRunner) Run(cfg *RunnerConfig) {
	for _, g := range r.SupervisorGroups {
		g.Run(cfg)
	}
}

type SupervisorGroup struct {
	Name              string
	Count             int32
	MessagePrototypes []*MessagePrototype
}

func (s *SupervisorGroup) PrettyStr() string {
	var b strings.Builder

	headerStr := fmt.Sprintf("Supervisor group: %s", s.Name)
	fmt.Fprintln(&b, headerStr)
	fmt.Fprintln(&b, strings.Repeat("=", len(headerStr)))

	fmt.Fprintf(&b, "- Simulated supervisors: %d\n", s.Count)
	fmt.Fprintf(&b, "- Services per supervisor: %d\n", len(s.MessagePrototypes))

	fmt.Fprintln(&b, "")

	for _, m := range s.MessagePrototypes {
		fmt.Fprintln(&b, m.PrettyStr())
	}

	return b.String()
}

func (s *SupervisorGroup) Run(cfg *RunnerConfig) {
	for n := int32(0); n < s.Count; n++ {
		fmt.Printf("supervisor group %q (%d)\n", s.Name, n)
		go s.SpawnSup(n, cfg)
	}
}

func (s *SupervisorGroup) SpawnSup(n int32, cfg *RunnerConfig) error {
	sup, err := NewSupSim(s.Name, n, cfg, s.MessagePrototypes)
	if err != nil {
		fmt.Printf("Supervisor setup failed: %s\n", err)
		return err
	}

	err = sup.Run()
	if err != nil {
		fmt.Printf("Supervisor failed to run: %s\n", err)
		return err
	}

	return nil
}

type SupSim struct {
	Name              string
	Idx               int32
	Cfg               *RunnerConfig
	UUID              uuid.UUID
	MessagePrototypes []*MessagePrototype
	nc                *nats.NatsClient
}

func NewSupSim(name string, idx int32, cfg *RunnerConfig, messages []*MessagePrototype) (*SupSim, error) {
	uuid, err := uuid.NewV4()
	if err != nil {
		return nil, err
	}
	return &SupSim{
		Name:              name,
		UUID:              uuid,
		Cfg:               cfg,
		MessagePrototypes: messages,
	}, nil
}

func (s *SupSim) Run() error {
	url := fmt.Sprintf("nats://%s@%s:4222", s.Cfg.AuthToken, s.Cfg.Host)
	cluster := "event-service"
	client := fmt.Sprintf("load-generator-%s", s.UUID)
	durable := client
	subject := "habitat"

	s.nc = nats.NewExternalClient(url, cluster, client, durable, subject)
	s.nc.InsecureSkipVerify = true
	err := s.nc.Connect()
	if err != nil {
		return err
	}
	defer s.nc.Close()
	// run loadgen loop

	// TODO: use logger instead
	fmt.Printf("starting sup %s (%d) (%s)\n", s.Name, s.Idx, s.UUID)

	// the ticker always waits its full time before publishing. Also we wish to
	// splay out the simulated sups for a more even distribution of messages. So
	// we get a random splay, publish once, then loop.
	splay := rand.Int31n(int32(s.Cfg.Tick))
	time.Sleep(time.Duration(splay) * time.Second)
	s.PublishAll()

	ticker := time.NewTicker(time.Duration(s.Cfg.Tick) * time.Second)
	for _ = range ticker.C {
		s.PublishAll()
	}
	return nil
}

func (s *SupSim) PublishAll() error {
	for _, m := range s.MessagePrototypes {
		fmt.Printf("publish messages from sup %s (%d) (%s)\n", s.Name, s.Idx, s.UUID)
		msg := &applications.HabService{
			SupervisorId: s.UUID.String(),
			Group:        "default",
			PkgIdent: &applications.PackageIdent{
				Origin:  m.Origin,
				Name:    m.PkgName,
				Version: "0.1.0",
				Release: m.Release,
			},
		}
		err := s.nc.PublishHabService(msg)
		if err != nil {
			return err
		}
	}
	return nil
}

// Data elements of a message that currently exist:
// sup-id         AUTO
// group          hardcode to "default"
// application    assign per-service in sup template
// environment    <name>   The environment name of the current deployment
// health         hardcode to 0 (OK) for now.
// status         hardcode to 0 (RUNNING) for now
//
// Package Indentifier
// --origin      parse from service name
// --name        parse from service name
// --version     hardcode to 1.0.0 for now
// --release     generate from the startup time of the generator
type MessagePrototype struct {
	Application string
	Environment string
	Channel     string
	Site        string
	Origin      string
	PkgName     string
	Release     string
}

func (m *MessagePrototype) SetRelease() {
	m.Release = now.Format("20060102150405")
}

func (m *MessagePrototype) ApplySvcTemplate(t ServiceTemplate) {
	// TODO: make this safer
	parts := strings.Split(t.Package, "/")
	m.Origin = parts[0]
	m.PkgName = parts[1]
	m.Application = t.Application
}

func (m *MessagePrototype) ApplySupCfg(s SupervisorCfg) {
	m.Environment = s.Environment
	m.Channel = s.Channel
	m.Site = s.Site
}

func (m *MessagePrototype) PrettyStr() string {
	var b strings.Builder
	b.WriteString(fmt.Sprintf("  - Package: %s/%s/%s\n", m.Origin, m.PkgName, m.Release))
	b.WriteString(fmt.Sprintf("  - Application: %s\n", m.Application))
	b.WriteString(fmt.Sprintf("  - Environment: %s\n", m.Environment))
	b.WriteString(fmt.Sprintf("  - Channel: %s\n", m.Channel))
	b.WriteString(fmt.Sprintf("  - Site: %s\n", m.Site))
	return b.String()
}
