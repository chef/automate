// Since we aren't using the errors from this, we get lots of unparam violations as well
package events

import (
	"fmt"
	"sync/atomic"
	"time"

	"github.com/sirupsen/logrus"

	api "github.com/chef/automate/api/interservice/deployment"
)

// traceLogging enables additional debug logging that can be useful
// when developing the event_sender but is unlikely to be useful when
// debugging production issues.
var traceLogging = false

// SendFun is the prototype for the function that will get called during
// StreamTo
type SendFun = func(*api.DeployEvent) error

// EventSender is the interface we use to instrument the
// deployment-service to be able to capture status/progress events of
// its actions. Right now, we use it to send progress updates of an
// initial deployment out to the client.
type EventSender interface {
	Deploy(status api.DeployEvent_Status)
	Phase(status api.DeployEvent_Status, phaseID api.DeployEvent_PhaseID)
	PhaseStep(status api.DeployEvent_Status, phaseID api.DeployEvent_PhaseID,
		stepName string, errStr string)
	// streamTo is used by the server to provide the streaming
	// status of events out to the client.
	StreamTo(SendFun) error
	// TaskComplete signals that the server isn't doing anything else and the
	// stream should end.
	TaskComplete()
	Backup(backup api.DeployEvent_Backup)
	Stop()
}

// stream encapsulates a function to send events to and a channel
// to report error/finished on
type stream struct {
	sendFun  SendFun
	doneChan chan error
}

type memoryEventSender struct {
	deploymentID string

	// seq is a unique monotonically increasing sequence number
	// for each event. It is incremented as part of send.
	seq uint64

	// eventLog is the in memory store for events
	eventLog []*api.DeployEvent

	// eventChan is where the deploy goroutine sends events.
	// The eventSender run loop reads events from this channel to
	// distribute to its registered listeners.
	eventChan chan *api.DeployEvent

	// ctlChan allows for registration/deregistration of listeners
	// with the eventSender run loop.
	ctlChan chan controlMessage

	// registeredChans is a set of registered listening
	// channels. The eventSender run loop sends all received
	// messages to each registered channel.
	streams map[*stream]uint64
}

// controlMessage is sent to the memoryEventSender to allow register,
// deregister, and deploymentCompleted.
type controlMessage struct {
	op     controlOp
	stream *stream
}

type controlOp int

const (
	ctlRegister controlOp = 0
	ctlStop     controlOp = 2
)

func (cm *controlMessage) String() string {
	s := ""
	switch cm.op {
	case ctlRegister:
		s = "ctlRegister"
	case ctlStop:
		s = "ctlStop"
	}
	return s
}

// NewMemoryEventSender creates a new memory backed eventSender given
// a specified deploymentID.
func NewMemoryEventSender(deploymentID string) EventSender {
	sender := &memoryEventSender{
		deploymentID: deploymentID,
		seq:          0,
		ctlChan:      make(chan controlMessage),
		eventLog:     make([]*api.DeployEvent, 0, 100),
		eventChan:    make(chan *api.DeployEvent),
		streams:      make(map[*stream]uint64, 10),
	}
	sender.run()
	return sender
}

func (es *memoryEventSender) run() {
	logEvent := func(e *api.DeployEvent, msg string) {
		if traceLogging {
			logrus.WithFields(logrus.Fields{
				"mod":    "server.memoryEventSender.run",
				"action": "event_received",
				"seq":    fmt.Sprintf("%d", e.Sequence),
			}).Debug(msg)
		}
	}
	go runLoop(es, logEvent)
}

func runLoop(es *memoryEventSender, logEvent func(*api.DeployEvent, string)) {
	for {
		select {
		case event := <-es.eventChan:
			if event == nil {
				logrus.WithFields(logrus.Fields{
					"mod":  "server.memoryEventSender.runLoop",
					"chan": "eventChan",
				}).Error("received nil event")
				break
			}
			es.eventLog = append(es.eventLog, event)
			logEvent(event, "start_distribution")
			for s, lastSeq := range es.streams {
				if lastSeq < event.Sequence {
					logEvent(event, fmt.Sprintf("distribute: %v", s))
					if err := es.sendToStream(s, event); err != nil {
						logrus.WithError(err).WithFields(logrus.Fields{
							"mod":  "server.memoryEventSender.runLoop",
							"chan": "eventChan",
						}).Warn("failed to send event")
					}
				}
			}
			logEvent(event, "end_distribution")
		case ctl := <-es.ctlChan:
			if ctl.op == ctlStop {
				return
			}
			runLoopHandleCtl(es, ctl)
		}
	}
}

func runLoopHandleCtl(es *memoryEventSender, ctl controlMessage) {
	logrus.WithFields(logrus.Fields{
		"mod":     "server.memoryEventSender.runLoopHandleCtl",
		"ctl_msg": ctl.String(),
	}).Debug()

	switch ctl.op {
	case ctlRegister:
		es.streams[ctl.stream] = 0
		es.sendPastEvents(ctl.stream)
	}
}

func (es *memoryEventSender) sendPastEvents(s *stream) {
	for _, pastEvent := range es.eventLog {
		if err := es.sendToStream(s, pastEvent); err != nil {
			return
		}
	}
}

func (es *memoryEventSender) sendToStream(s *stream, ev *api.DeployEvent) error {
	if err := s.sendFun(ev); err != nil {
		es.deregister(s, err)
		return err
	}
	if done := ev.GetTaskComplete(); done != nil {
		es.deregister(s, nil)
		return nil
	}
	es.streams[s] = ev.Sequence
	return nil
}

func (es *memoryEventSender) deregister(s *stream, err error) {
	if err != nil {
		s.doneChan <- err
	}
	close(s.doneChan)
	delete(es.streams, s)
}

func (es *memoryEventSender) register(sendFun SendFun) chan error {
	s := stream{
		doneChan: make(chan error, 1),
		sendFun:  sendFun,
	}
	ctl := controlMessage{
		op:     ctlRegister,
		stream: &s,
	}
	logrus.Debugf("reg: %v", ctl)
	es.ctlChan <- ctl
	logrus.Debugf("reg accepted: %v", ctl)

	return s.doneChan
}

func (es *memoryEventSender) Stop() {
	ctl := controlMessage{
		op: ctlStop,
	}
	logrus.Debugf("sending ctlStop")
	es.ctlChan <- ctl
	logrus.Debugf("ctlStop accepted")
}

func (es *memoryEventSender) StreamTo(sendFun SendFun) error {
	doneChan := es.register(sendFun)
	err := <-doneChan
	return err
}

func (es *memoryEventSender) Deploy(status api.DeployEvent_Status) {
	pe := NewDeployEvent(status)
	es.sendDeploy(&pe)
}

func (es *memoryEventSender) Phase(status api.DeployEvent_Status, phaseID api.DeployEvent_PhaseID) {
	pe := NewPhaseEvent(phaseID, status)
	es.sendPhase(&pe)
}

func (es *memoryEventSender) PhaseStep(status api.DeployEvent_Status,
	phaseID api.DeployEvent_PhaseID,
	stepName string,
	errStr string) {
	pe := NewPhaseStepEvent(phaseID, stepName, status, errStr)
	es.sendPhaseStep(&pe)
}

func (es *memoryEventSender) TaskComplete() {
	e := NewTaskCompleteEvent()
	es.sendTaskComplete(&e)
}

func (es *memoryEventSender) Backup(backup api.DeployEvent_Backup) {
	es.sendBackup(&backup)
}

func (es *memoryEventSender) sendDeploy(event *api.DeployEvent_Deploy) {
	sendable := EventForDeploy(es.nextSeq(), es.deploymentID, time.Now(), event)
	es.send(&sendable)
}

func (es *memoryEventSender) sendPhase(event *api.DeployEvent_Phase) {
	sendable := EventForPhase(es.nextSeq(), es.deploymentID, time.Now(), event)
	es.send(&sendable)
}

func (es *memoryEventSender) sendPhaseStep(event *api.DeployEvent_PhaseStep) {
	sendable := EventForPhaseStep(es.nextSeq(), es.deploymentID, time.Now(), event)
	es.send(&sendable)
}

func (es *memoryEventSender) sendBackup(event *api.DeployEvent_Backup) {
	sendable := EventForBackup(es.nextSeq(), es.deploymentID, time.Now(), event)
	es.send(&sendable)
}

func (es *memoryEventSender) sendTaskComplete(event *api.DeployEvent_TaskComplete) {
	sendable := EventForTaskComplete(es.nextSeq(), es.deploymentID, time.Now(), event)
	es.send(&sendable)
}

func (es *memoryEventSender) send(event *api.DeployEvent) {
	es.eventChan <- event
	if traceLogging {
		logrus.WithFields(logrus.Fields{
			"mod":       "server.memoryEventSender.send",
			"eventChan": fmt.Sprintf("%v", es.eventChan),
			"seq":       fmt.Sprintf("%d", event.Sequence),
		}).Debug("sent_event")
	}
}

func (es *memoryEventSender) nextSeq() uint64 {
	return atomic.AddUint64(&es.seq, 1)
}
