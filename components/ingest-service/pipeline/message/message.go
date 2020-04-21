package message

import uuid "github.com/gofrs/uuid"

type Finishable interface {
	FinishProcessing(err error)
	MessageID() uuid.UUID
}
