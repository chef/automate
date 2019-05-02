package backup

import (
	"sync"

	"github.com/sirupsen/logrus"

	api "github.com/chef/automate/api/interservice/deployment"
)

// Executor executes synchronous and asynchronous backup operations. It
// listens for the operation events, annotates them, and publishes them to the
// status and publishing the backup status to event sender.
type Executor struct {
	// BackupTask event operations channel to publish backup events to
	opEventChan chan api.DeployEvent_Backup_Operation

	// BackupTask error channel
	opErrChan chan error

	// Backup specification to execute
	spec Spec

	// Backup Task cancel callback. This should be called in the event of
	// an operation failure.
	cancel func()

	// Operation progress calculator. These are used to aggregate
	// the overall progress of all operation that are to be
	// executed.
	syncProgress *ProgressCalculator

	// Operation progress channel. This is a channel for each operation to
	// publish progress events.
	syncProgressChan chan OperationProgress
	progressExitChan chan struct{}

	// What backup operation type were executing
	execType api.DeployEvent_Backup_Operation_Type

	// Backup execution lock. This should be removed when the es-sidecar can
	// handle async requests.
	locky *sync.Mutex

	// objectManifest tracks the objects that get written to the blob storage.
	// This lets us collect checksums of all the objects and inject them into the
	// backup metadata.
	objectManifest ObjectManifest
}

// ExecutorOpt represents an optional configuration function for an Executor
type ExecutorOpt func(*Executor)

// NewExecutor returns a new instance of a backup job executor
func NewExecutor(opts ...ExecutorOpt) *Executor {
	executor := &Executor{
		// Operation progress calculator
		syncProgress: NewProgressCalculator(),

		// Operation progress channel
		syncProgressChan: make(chan OperationProgress),
		progressExitChan: make(chan struct{}),

		// TODO(jaym) 05-07-2018: Remove the lock
		// The purpose of the lock is to make sure only 1 backup executor runs at a time.
		// The reason we only want 1 backup executor at a time is because we can
		// only have 1 elasticsearch snapshot going at a time. Once es-sidecar-service
		// can handle this situation, we can remove this lock
		locky:          &sync.Mutex{},
		objectManifest: NewObjectManifest(),
	}

	for _, opt := range opts {
		opt(executor)
	}

	return executor
}

// WithEventChan configures the event sender channel that operation events
// and published to.
func WithEventChan(c chan api.DeployEvent_Backup_Operation) ExecutorOpt {
	return func(executor *Executor) {
		executor.opEventChan = c
	}
}

// WithErrorChan configures the error channel that the executor will publish
// if an operation fails.
func WithErrorChan(c chan error) ExecutorOpt {
	return func(executor *Executor) {
		executor.opErrChan = c
	}
}

// WithSpec configures the backup operations from a given Spec
func WithSpec(s Spec) ExecutorOpt {
	return func(executor *Executor) {
		executor.spec = s
	}
}

// WithCancel configures the backup execution context cancellation callback
func WithCancel(canx func()) ExecutorOpt {
	return func(executor *Executor) {
		executor.cancel = canx
	}
}

// WithLock sets the lock an executor must hold before it may do any actual
// backup task
func WithLock(lock *sync.Mutex) ExecutorOpt {
	return func(executor *Executor) {
		executor.locky = lock
	}
}

// Backup runs the operations and waits for them to complete. When
// completed or failed it publishes notifications to the event channel
// and notifies that runner that the operations have completed.
func (b *Executor) Backup(backupCtx Context) error {
	defer b.stopProgressCalculator()
	b.execType = api.DeployEvent_Backup_Operation_BACKUP
	b.resetProgress()
	b.startProgressCalculator()

	b.locky.Lock()
	defer b.locky.Unlock()

	if err := b.runBackupOperations(backupCtx); err != nil {
		return err
	}

	if err := b.runFinalizingOperations(backupCtx); err != nil {
		return err
	}

	b.opEventChan <- api.DeployEvent_Backup_Operation{
		Status:       api.DeployEvent_COMPLETE_OK,
		Type:         b.execType,
		Name:         b.spec.Name,
		SyncProgress: 100,
	}

	return nil
}

func (b *Executor) DeleteBackup(backupCtx Context) error {
	metadataOps := []Operation{}

	for _, op := range b.spec.SyncOps() {
		if err := op.Delete(backupCtx); err != nil {
			b.cancel()
			return err
		}
	}

	for _, op := range b.spec.FinalizingOps() {
		if _, ok := op.(*MetadataWriterOperation); ok {
			// our metadata operations should be the last that are
			// deleted in case something fails.
			// This should mean that deletes are always retryable
			metadataOps = append(metadataOps, op)
			continue
		}

		if err := op.Delete(backupCtx); err != nil {
			b.cancel()
			return err
		}
	}

	for _, op := range metadataOps {
		if err := op.Delete(backupCtx); err != nil {
			b.cancel()
			return err
		}
	}

	return nil
}

// Restore does a restores of all backup operations in the spec
func (b *Executor) Restore(backupCtx Context, metadata *Metadata) error {
	defer b.stopProgressCalculator()
	b.execType = api.DeployEvent_Backup_Operation_RESTORE
	b.resetProgress()
	b.startProgressCalculator()

	b.opEventChan <- api.DeployEvent_Backup_Operation{
		Status:       api.DeployEvent_RUNNING,
		Type:         b.execType,
		Name:         b.spec.Name,
		SyncProgress: 0,
	}

	if err := b.runRestoreSync(backupCtx, metadata, b.spec.SyncOps()); err != nil {
		return err
	}

	b.syncProgress.Done()

	b.opEventChan <- api.DeployEvent_Backup_Operation{
		Status:       api.DeployEvent_COMPLETE_OK,
		Type:         b.execType,
		Name:         b.spec.Name,
		SyncProgress: 100,
	}

	return nil
}

// runRestoreSync does a synchronous restore of the given operations
func (b *Executor) runRestoreSync(backupCtx Context, metadata *Metadata, ops []Operation) error {
	for _, o := range ops {
		op := o

		// Make sure another op hasn't already failed before we start more.
		select {
		case <-backupCtx.ctx.Done():
			return backupCtx.ctx.Err()
		default:
		}

		verifier := metadata.Verifier()

		if err := op.Restore(backupCtx, b.spec.Name, verifier, b.syncProgressChan); err != nil {
			b.opEventChan <- api.DeployEvent_Backup_Operation{
				Status:       api.DeployEvent_COMPLETE_FAIL,
				Type:         b.execType,
				Name:         b.spec.Name,
				SyncProgress: b.syncProgress.Percent(),
			}

			// Do a non-blocking publish to the error channel
			select {
			case b.opErrChan <- err:
			default:
			}

			// Cancel our context to signal other operations and specification
			// to terminate.
			b.cancel()
			return err
		}
	}

	return nil
}

// RestoreSyncPaths synchronously restores path data of the spec
func (b *Executor) RestoreSyncPaths(backupCtx Context, metadata *Metadata) error {
	defer b.stopProgressCalculator()
	b.execType = api.DeployEvent_Backup_Operation_RESTORE
	b.resetProgress()
	b.startProgressCalculator()

	ops := []Operation{}

	for _, o := range b.spec.SyncPaths {
		op := o
		ops = append(ops, &op)
	}

	return b.runRestoreSync(backupCtx, metadata, ops)
}

// RestoreSyncCmds synchronously restores cmd data of the spec
func (b *Executor) RestoreSyncCmds(backupCtx Context, metadata *Metadata) error {
	defer b.stopProgressCalculator()
	b.execType = api.DeployEvent_Backup_Operation_RESTORE
	b.resetProgress()
	b.startProgressCalculator()

	ops := []Operation{}

	for _, o := range b.spec.SyncCmds {
		op := o
		ops = append(ops, &op)
	}

	return b.runRestoreSync(backupCtx, metadata, ops)
}

// startProgressCalculator starts the progress event publisher
func (b *Executor) startProgressCalculator() {
	go func() {
		for {
			select {
			case u := <-b.syncProgressChan:
				b.syncProgress.Update(u)
				b.opEventChan <- api.DeployEvent_Backup_Operation{
					Status:       api.DeployEvent_RUNNING,
					Type:         b.execType,
					Name:         b.spec.Name,
					SyncProgress: b.syncProgress.Percent(),
				}
			case <-b.progressExitChan:
				return
			}
		}
	}()
}

// stopProgressCalculator stops the progress event publisher
func (b *Executor) stopProgressCalculator() {
	close(b.progressExitChan)
}

// resetProgress resets progress to zero for all operations.
func (b *Executor) resetProgress() {
	b.progressExitChan = make(chan struct{})
	b.syncProgress = NewProgressCalculator()

	for _, o := range b.spec.SyncOps() {
		op := o
		b.syncProgress.Update(OperationProgress{Name: op.String(), Progress: 0})
	}
}

func (b *Executor) runBackupOperations(backupCtx Context) error {
	for _, o := range b.spec.SyncOps() {
		op := o
		// Make sure another op hasn't already failed before we start more.
		select {
		case <-backupCtx.ctx.Done():
			return backupCtx.ctx.Err()
		default:
		}

		logrus.WithFields(logrus.Fields{
			"task_id": backupCtx.backupTask.TaskID(),
			"mode":    "sync",
			"op_name": op.String(),
		}).Debug("Starting backup operation")

		if err := op.Backup(backupCtx, b.objectManifest, b.syncProgressChan); err != nil {
			b.opEventChan <- api.DeployEvent_Backup_Operation{
				Status:       api.DeployEvent_COMPLETE_FAIL,
				Type:         b.execType,
				Name:         b.spec.Name,
				SyncProgress: b.syncProgress.Percent(),
			}

			// Do a non-blocking publish to the error channel
			select {
			case b.opErrChan <- err:
			default:
			}

			// Cancel our context to signal other operations and specification
			// to terminate.
			b.cancel()
			return err
		}
	}

	b.syncProgress.Done()
	b.opEventChan <- api.DeployEvent_Backup_Operation{
		Status:       api.DeployEvent_COMPLETE_OK,
		Type:         b.execType,
		Name:         b.spec.Name,
		SyncProgress: b.syncProgress.Percent(),
	}

	return nil
}

func (b *Executor) runFinalizingOperations(backupCtx Context) error {
	for _, o := range b.spec.FinalizingOps() {
		op := o
		// Make sure another op hasn't already failed before we start more.
		select {
		case <-backupCtx.ctx.Done():
			return backupCtx.ctx.Err()
		default:
		}

		logrus.WithFields(logrus.Fields{
			"task_id": backupCtx.backupTask.TaskID(),
			"mode":    "sync",
			"op_name": op.String(),
		}).Debug("Starting backup operation")

		if err := op.Backup(backupCtx, b.objectManifest, b.syncProgressChan); err != nil {
			b.opEventChan <- api.DeployEvent_Backup_Operation{
				Status:       api.DeployEvent_COMPLETE_FAIL,
				Type:         b.execType,
				Name:         b.spec.Name,
				SyncProgress: b.syncProgress.Percent(),
			}

			// Do a non-blocking publish to the error channel
			select {
			case b.opErrChan <- err:
			default:
			}

			// Cancel our context to signal other operations and specification
			// to terminate.
			b.cancel()
			return err
		}
	}

	return nil
}

// OperationProgress represents an operations current progress
type OperationProgress struct {
	Name     string
	Progress float64
}

// NewProgressCalculator returns a new initialized ProgressCalculator
func NewProgressCalculator() *ProgressCalculator {
	return &ProgressCalculator{
		mutex: sync.Mutex{},
		ops:   map[string]OperationProgress{},
	}
}

// ProgressCalculator tracks the progress of multiple operations
type ProgressCalculator struct {
	mutex sync.Mutex
	ops   map[string]OperationProgress
}

// Update updates an operation in the progress calculator
func (p *ProgressCalculator) Update(op OperationProgress) {
	p.mutex.Lock()
	p.ops[op.Name] = op
	p.mutex.Unlock()
}

// Done sets all operations in the progress calculator to 100%
func (p *ProgressCalculator) Done() {
	p.mutex.Lock()
	for _, op := range p.ops {
		p.ops[op.Name] = OperationProgress{
			Name:     op.Name,
			Progress: float64(100),
		}
	}
	p.mutex.Unlock()
}

// Percent returns the average completion percent between all operations in
// the progress calculator. It's a bit microsoftian in that some operations
// may run much longer than others.
func (p *ProgressCalculator) Percent() float64 {
	p.mutex.Lock()
	defer p.mutex.Unlock()

	if len(p.ops) == 0 {
		return 100
	}

	total := float64(0)
	for _, op := range p.ops {
		total += op.Progress
	}

	return total / float64(len(p.ops))
}
