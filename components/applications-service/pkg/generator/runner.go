package generator

import (
	"fmt"
	"math/rand"
	"strings"
	"time"

	"github.com/chef/automate/api/external/applications"
	"github.com/chef/automate/components/applications-service/pkg/nats"
	uuid "github.com/chef/automate/lib/uuid4"
	"github.com/pkg/errors"

	log "github.com/sirupsen/logrus"
)

// Everything is hardcoded to 0.1.0 for now
const versionNumber = "0.1.0"

type RunnerConfig struct {
	AuthToken string
	Host      string
	Tick      int
	Verbosity int
}

type LoadGenRunner struct {
	SupervisorGroups []*SupervisorGroup
	stats            *RunnerStatsKeeper
}

func (r *LoadGenRunner) Run(cfg *RunnerConfig) {

	logLevel := log.ErrorLevel
	switch cfg.Verbosity {
	case 0:
	case 1:
		logLevel = log.InfoLevel
	default:
		logLevel = log.DebugLevel
	}

	log.SetLevel(logLevel)

	for _, supGroup := range r.SupervisorGroups {
		fmt.Print(supGroup.PrettyStr())
	}

	r.stats = NewStatsKeeper()
	go r.stats.RunCollectAndPrintLoop()

	for _, g := range r.SupervisorGroups {
		g.Run(cfg, r.stats)
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

func (s *SupervisorGroup) Run(cfg *RunnerConfig, stats *RunnerStatsKeeper) {
	for n := int32(0); n < s.Count; n++ {
		log.WithFields(log.Fields{"name": s.Name, "index": n}).Info("spawning supervisor")
		go s.SpawnSup(n, cfg, stats)
	}
}

func (s *SupervisorGroup) SpawnSup(n int32, cfg *RunnerConfig, stats *RunnerStatsKeeper) error {
	uuid, err := uuid.NewV4()
	if err != nil {
		log.WithError(err).Error("Failed to make a V4 UUID")
		return err
	}

	sup := &SupSim{
		Name:              s.Name,
		UUID:              uuid,
		Cfg:               cfg,
		MessagePrototypes: s.MessagePrototypes,
		Stats:             stats,
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
	Stats             *RunnerStatsKeeper
	nc                *nats.NatsClient
}

func (s *SupSim) Run() error {
	s.Stats.SupStarted()
	defer s.Stats.SupDied()

	var (
		url     = fmt.Sprintf("nats://%s@%s:4222", s.Cfg.AuthToken, s.Cfg.Host)
		cluster = "event-service"
		client  = fmt.Sprintf("load-generator-%s", s.UUID)
		durable = client
		subject = "habitat"
	)

	s.nc = nats.NewExternalClient(url, cluster, client, durable, subject)
	s.nc.InsecureSkipVerify = true
	err := s.nc.Connect()
	if err != nil {
		return err
	}
	defer s.nc.Close()
	// run loadgen loop

	log.WithFields(log.Fields{"name": s.Name, "index": s.Idx, "uuid": s.UUID.String()}).Info("starting supervisor")

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
	log.WithFields(log.Fields{
		"name":          s.Name,
		"index":         s.Idx,
		"uuid":          s.UUID.String(),
		"service_count": len(s.MessagePrototypes),
	}).Info("publishing messages")

	for _, m := range s.MessagePrototypes {

		msg := m.CreateMessage(s.UUID.String())

		log.WithFields(log.Fields{
			"name":          s.Name,
			"index":         s.Idx,
			"uuid":          s.UUID.String(),
			"service_count": len(s.MessagePrototypes),
			"pkg_fqid":      m.PkgFQID(),
		}).Debug("publishing messages")

		err := s.nc.PublishHabService(msg)
		if err != nil {
			s.Stats.FailedPublish()
			return err
		}
		s.Stats.SuccessfulPublish()
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

func (m *MessagePrototype) ApplySvcTemplate(t ServiceTemplate) error {
	parts := strings.Split(t.Package, "/")
	if len(parts) != 2 {
		return errors.Errorf("failed to parse package name %q into origin/package components", t.Package)
	}
	m.Origin = parts[0]
	m.PkgName = parts[1]
	m.Application = t.Application
	return nil
}

func (m *MessagePrototype) ApplySupCfg(s SupervisorCfg) {
	m.Environment = s.Environment
	m.Channel = s.Channel
	m.Site = s.Site
}

func (m *MessagePrototype) CreateMessage(uuid string) *applications.HabService {
	return &applications.HabService{
		SupervisorId: uuid,
		Group:        "default",
		HealthCheck:  applications.HealthStatus_OK,
		Status:       applications.ServiceStatus_RUNNING,
		Application:  m.Application,
		Environment:  m.Environment,
		Fqdn:         fmt.Sprintf("%s.example", uuid),
		PkgIdent: &applications.PackageIdent{
			Origin:  m.Origin,
			Name:    m.PkgName,
			Version: "0.1.0",
			Release: m.Release,
		},
	}
}

func (m *MessagePrototype) PkgFQID() string {
	return fmt.Sprintf("%s/%s/%s/%s", m.Origin, m.PkgName, versionNumber, m.Release)
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
