package main

import (
	"os/exec"
	"strings"
	"time"

	"github.com/pkg/errors"
	"github.com/sirupsen/logrus"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-deployment/pkg/habpkg"
	"github.com/chef/automate/components/automate-deployment/pkg/target"
	"github.com/chef/automate/lib/platform/command"
)

var opts = struct {
	Debug       bool
	Timeout     string
	WorkerCount int
}{}

var warnThreshold time.Duration

func main() {
	cmd := &cobra.Command{
		Use:           "hab-bug-6252-repro PKG_IDENT",
		Short:         "Attempt to reproduce habitat/habitat#6252",
		SilenceUsage:  true,
		SilenceErrors: true,
		Args:          cobra.ExactArgs(1),
		PersistentPreRun: func(cmd *cobra.Command, args []string) {
			if opts.Debug {
				logrus.SetLevel(logrus.DebugLevel)
			}
		},
		RunE: run,
	}

	cmd.PersistentFlags().BoolVarP(
		&opts.Debug,
		"debug",
		"d",
		false,
		"Enabled debug output")

	cmd.PersistentFlags().StringVar(
		&opts.Timeout,
		"threshold",
		"10s",
		"How much time to wait before reporting a possible repro")

	cmd.PersistentFlags().IntVar(
		&opts.WorkerCount,
		"workers",
		4,
		"How much time to wait before reporting a possible repro")

	err := cmd.Execute()
	if err != nil {
		logrus.Fatal(err)
	}
}

func run(_ *cobra.Command, args []string) error {
	habPkg, err := habpkg.FromString(args[0])
	if err != nil {
		return errors.Wrap(err, "could not parse argument")
	}

	warnThreshold, err = time.ParseDuration(opts.Timeout)
	if err != nil {
		return errors.Wrap(err, "could not parse threshold")
	}

	logrus.SetFormatter(&logrus.TextFormatter{
		FullTimestamp: true,
	})

	habPath, err := exec.LookPath("hab")
	if err != nil {
		errors.Wrap(err, "could not find hab in path")
	}

	habVer, err := command.Output("hab", command.Args("--version"))
	if err != nil {
		errors.Wrap(err, "could not get hab version")
	}

	logrus.Info("Starting reproduction attempt for habitat/habitat#6252")
	logrus.Infof("hab found in path: %s", habPath)
	logrus.Infof("hab --version: %s", strings.TrimSpace(habVer))
	tgt := target.NewLocalTarget(true)
	resultChan := make(chan struct{}, 10)
	doneChan := make(chan struct{})
	startTime := time.Now()
	for i := 0; i < opts.WorkerCount; i++ {
		go workerThread(i, doneChan, resultChan, tgt, &habPkg)
	}

	count := 0
	for {
		select {
		case <-resultChan:
			count++
			if (count % 1000) == 0 {
				logrus.WithFields(logrus.Fields{
					"total_duration": time.Since(startTime),
					"count":          count,
				}).Info("still running")
			}
		}
	}

}

func workerThread(threadNum int, doneChan chan struct{}, resultChan chan struct{}, t target.Target, habPkg *habpkg.HabPkg) {
	tlog := logrus.WithFields(logrus.Fields{
		"worker": threadNum,
	})
	tlog.Info("Starting")
	try := 1
	for {
		select {
		case <-doneChan:
			tlog.Info("stopping")
			return
		default:
		}

		log := tlog.WithFields(logrus.Fields{
			"try": try,
		})
		tryCommand(log, t, habPkg, doneChan)
		resultChan <- struct{}{}
		try++
	}
}

func tryCommand(log *logrus.Entry, t target.Target, habPkg *habpkg.HabPkg, doneChan chan struct{}) {
	commandDoneChan := runCommand(log, t, habPkg)
	timer := time.NewTimer(warnThreshold)
	select {
	case <-commandDoneChan:
		if !timer.Stop() {
			<-timer.C
		}
	case <-timer.C:
		log.Warnf("POSSIBLE REPRODUCTION! hab command running for more than %s, stopping other threads and waiting", warnThreshold)
		close(doneChan)
		<-commandDoneChan
		log.Warn("FALSE ALARM :(")
		timer.Stop()
	}
}

func runCommand(log *logrus.Entry, t target.Target, habPkg *habpkg.HabPkg) chan struct{} {
	doneChan := make(chan struct{})
	go func() {
		_, err := t.IsInstalled(habPkg)
		if err != nil {
			log.WithError(err).Error("IsInstalled failed!")
		}
		close(doneChan)
	}()
	return doneChan
}
