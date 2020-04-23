package storage

import (
	"errors"
	"fmt"
)

// Error responses common to all storage adapters, be it memstore, postgres, etc.
var (
	// ErrNotFound is returned when a requested policy wasn't found.
	ErrNotFound = errors.New("not found")

	// ErrCannotDelete is thrown by our custom pg error from migration 02
	// if a user tries to delete a policy that is marked as non-deletable.
	ErrCannotDelete = errors.New("policy not deletable")

	// ErrConflict indicates that the object being created already exists.
	ErrConflict = errors.New("conflict")

	// ErrDatabase results from unexpected database errors.
	ErrDatabase = errors.New("database internal")

	// ErrChangeProjectForRule indicates that an update operation attempted to change
	// the project for a rule, which is not allowed.
	ErrChangeProjectForRule = errors.New("cannot change rule project")

	// ErrMarkedForDeletion indicates an update was attempted on a rule that
	// is staged for deletion (cannot be "un-deleted")
	ErrMarkedForDeletion = errors.New("rule marked for deletion")

	// ErrChangeTypeForRule indicates that an update operation attempted to change
	// the type for a rule, which is not allowed.
	ErrChangeTypeForRule = errors.New("cannot change rule type")

	// ErrProjectInGraveyard indicates that an attempt was made to create a project with
	// an ID that is currently in iam_project_graveyard
	ErrProjectInGraveyard = errors.New("cannot create project with ID that is currently being deleted")
)

// TxCommitError occurs when the database attempts to commit a transaction and
// fails.
type TxCommitError struct {
	underlying error
}

func NewTxCommitError(e error) error {
	return &TxCommitError{underlying: e}
}

func (e *TxCommitError) Error() string {
	return "commit db transaction: " + e.underlying.Error()
}

// MissingFieldError occurs when a required field was not passed.
type ForeignKeyError struct {
	Msg string
}

func (e *ForeignKeyError) Error() string {
	return e.Msg
}

type MaxProjectsExceededError struct {
	projectLimit int
}

func NewMaxProjectsExceededError(limit int) error {
	return &MaxProjectsExceededError{projectLimit: limit}
}

// MaxProjectsExceededError indicates that a new project cannot be created
// since the max allowed are already created.
func (e *MaxProjectsExceededError) Error() string {
	return fmt.Sprintf("cannot create project: limit of %v projects already reached",
		e.projectLimit)
}
