package server_test

import (
	"testing"
	"time"

	"github.com/stretchr/testify/assert"

	"github.com/chef/automate/components/automate-deployment/pkg/server"
)

func expectResponseOnChan(t *testing.T, testChan chan bool) {
	select {
	case res := <-testChan:
		assert.Equal(t, true, res)
	case <-time.After(time.Second * 5):
		assert.Fail(t, "user-defined function failed to run in 5 seconds")
	}

}

func expectNoResponseOnChan(t *testing.T, testChan chan bool) {
	// Not the best test, but wait a few milliseconds, if we don't
	// get another job, let's call it good.  Don't call this too
	// much or the tests get slow.
	select {
	case <-testChan:
		assert.Fail(t, "user-defined function responded unexpectedly!")
	case <-time.After(time.Millisecond * 5):
		return
	}
}

func TestLoopRunsUserFunc(t *testing.T) {
	testChan := make(chan bool)
	testFunc := func() { testChan <- true }
	testLooper := server.NewLooper(time.Duration(1)*time.Nanosecond, testFunc)
	testLooper.Start()
	defer testLooper.Stop()
	expectResponseOnChan(t, testChan)
}
func TestUserFuncRunsAtStart(t *testing.T) {
	// We're going to use a duration of 100 seconds. Our response is expected
	// in 5 seconds. That means if we don't run at start, we will hit a timeout
	testChan := make(chan bool)
	testFunc := func() { testChan <- true }
	testLooper := server.NewLooper(time.Duration(100)*time.Second, testFunc)
	testLooper.Start()
	defer testLooper.Stop()
	expectResponseOnChan(t, testChan)
}

func TestLoopRunsMoreThanOnce(t *testing.T) {
	testChan := make(chan bool)
	testFunc := func() { testChan <- true }
	testLooper := server.NewLooper(time.Duration(1)*time.Nanosecond, testFunc)
	testLooper.Start()
	defer testLooper.Stop()
	expectResponseOnChan(t, testChan)
	expectResponseOnChan(t, testChan)
}

func TestStopCanStopIfUserFuncIsBlocked(t *testing.T) {
	testChanOut := make(chan bool)
	testChanIn := make(chan bool)
	testFunc := func() {
		testChanOut <- true
		select {
		case <-testChanIn:
			return
		}

	}
	testLooper := server.NewLooper(time.Duration(0)*time.Nanosecond, testFunc)
	testLooper.Start()

	select {
	case <-testChanOut:
		testLooper.Stop()  // testFunc should be blocked now, let's try to stop
		testChanIn <- true // unblock our testFunc

		// Not the best test, but wait a few milliseconds, if
		// we don't get another job, let's call it good.
		expectNoResponseOnChan(t, testChanOut)
	case <-time.After(time.Second * 5):
		assert.Fail(t, "user-defined function failed to run in 5 seconds")
	}
}

func TestStopRecallIsSafe(t *testing.T) {
	testFunc := func() {}
	testLooper := server.NewLooper(time.Duration(5)*time.Second, testFunc)
	testLooper.Start()
	testLooper.Stop()
	testLooper.Stop()
}

func TestStartRecallIsSafe(t *testing.T) {
	testChanOut := make(chan bool)
	testChanIn := make(chan bool)
	testFunc := func() {
		testChanOut <- true
		select {
		case <-testChanIn:
			return
		}

	}

	testLooper := server.NewLooper(time.Duration(0)*time.Second, testFunc)
	testLooper.Start()
	defer testLooper.Stop()

	testLooper.Start()
	expectResponseOnChan(t, testChanOut)
	expectNoResponseOnChan(t, testChanOut)
}

func TestStartAfterStop(t *testing.T) {
	testChan := make(chan bool)
	testFunc := func() { testChan <- true }

	testLooper := server.NewLooper(time.Duration(0)*time.Second, testFunc)
	testLooper.Start()
	expectResponseOnChan(t, testChan)
	testLooper.Stop()
	testLooper.Start()
	expectResponseOnChan(t, testChan)
}
