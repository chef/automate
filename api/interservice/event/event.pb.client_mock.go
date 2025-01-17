// Code generated by MockGen. DO NOT EDIT.
// Source: event/event.pb.go

// Package event is a generated GoMock package.
package event

import (
	context "context"
	reflect "reflect"

	gomock "github.com/golang/mock/gomock"
	grpc "google.golang.org/grpc"
)

// MockEventServiceClient is a mock of EventServiceClient interface.
type MockEventServiceClient struct {
	ctrl     *gomock.Controller
	recorder *MockEventServiceClientMockRecorder
}

// MockEventServiceClientMockRecorder is the mock recorder for MockEventServiceClient.
type MockEventServiceClientMockRecorder struct {
	mock *MockEventServiceClient
}

// NewMockEventServiceClient creates a new mock instance.
func NewMockEventServiceClient(ctrl *gomock.Controller) *MockEventServiceClient {
	mock := &MockEventServiceClient{ctrl: ctrl}
	mock.recorder = &MockEventServiceClientMockRecorder{mock}
	return mock
}

// EXPECT returns an object that allows the caller to indicate expected use.
func (m *MockEventServiceClient) EXPECT() *MockEventServiceClientMockRecorder {
	return m.recorder
}

// Publish mocks base method.
func (m *MockEventServiceClient) Publish(ctx context.Context, in *PublishRequest, opts ...grpc.CallOption) (*PublishResponse, error) {
	m.ctrl.T.Helper()
	varargs := []interface{}{ctx, in}
	for _, a := range opts {
		varargs = append(varargs, a)
	}
	ret := m.ctrl.Call(m, "Publish", varargs...)
	ret0, _ := ret[0].(*PublishResponse)
	ret1, _ := ret[1].(error)
	return ret0, ret1
}

// Publish indicates an expected call of Publish.
func (mr *MockEventServiceClientMockRecorder) Publish(ctx, in interface{}, opts ...interface{}) *gomock.Call {
	mr.mock.ctrl.T.Helper()
	varargs := append([]interface{}{ctx, in}, opts...)
	return mr.mock.ctrl.RecordCallWithMethodType(mr.mock, "Publish", reflect.TypeOf((*MockEventServiceClient)(nil).Publish), varargs...)
}

// Start mocks base method.
func (m *MockEventServiceClient) Start(ctx context.Context, in *StartRequest, opts ...grpc.CallOption) (*StartResponse, error) {
	m.ctrl.T.Helper()
	varargs := []interface{}{ctx, in}
	for _, a := range opts {
		varargs = append(varargs, a)
	}
	ret := m.ctrl.Call(m, "Start", varargs...)
	ret0, _ := ret[0].(*StartResponse)
	ret1, _ := ret[1].(error)
	return ret0, ret1
}

// Start indicates an expected call of Start.
func (mr *MockEventServiceClientMockRecorder) Start(ctx, in interface{}, opts ...interface{}) *gomock.Call {
	mr.mock.ctrl.T.Helper()
	varargs := append([]interface{}{ctx, in}, opts...)
	return mr.mock.ctrl.RecordCallWithMethodType(mr.mock, "Start", reflect.TypeOf((*MockEventServiceClient)(nil).Start), varargs...)
}

// Stop mocks base method.
func (m *MockEventServiceClient) Stop(ctx context.Context, in *StopRequest, opts ...grpc.CallOption) (*StopResponse, error) {
	m.ctrl.T.Helper()
	varargs := []interface{}{ctx, in}
	for _, a := range opts {
		varargs = append(varargs, a)
	}
	ret := m.ctrl.Call(m, "Stop", varargs...)
	ret0, _ := ret[0].(*StopResponse)
	ret1, _ := ret[1].(error)
	return ret0, ret1
}

// Stop indicates an expected call of Stop.
func (mr *MockEventServiceClientMockRecorder) Stop(ctx, in interface{}, opts ...interface{}) *gomock.Call {
	mr.mock.ctrl.T.Helper()
	varargs := append([]interface{}{ctx, in}, opts...)
	return mr.mock.ctrl.RecordCallWithMethodType(mr.mock, "Stop", reflect.TypeOf((*MockEventServiceClient)(nil).Stop), varargs...)
}

// Subscribe mocks base method.
func (m *MockEventServiceClient) Subscribe(ctx context.Context, in *SubscribeRequest, opts ...grpc.CallOption) (*SubscribeResponse, error) {
	m.ctrl.T.Helper()
	varargs := []interface{}{ctx, in}
	for _, a := range opts {
		varargs = append(varargs, a)
	}
	ret := m.ctrl.Call(m, "Subscribe", varargs...)
	ret0, _ := ret[0].(*SubscribeResponse)
	ret1, _ := ret[1].(error)
	return ret0, ret1
}

// Subscribe indicates an expected call of Subscribe.
func (mr *MockEventServiceClientMockRecorder) Subscribe(ctx, in interface{}, opts ...interface{}) *gomock.Call {
	mr.mock.ctrl.T.Helper()
	varargs := append([]interface{}{ctx, in}, opts...)
	return mr.mock.ctrl.RecordCallWithMethodType(mr.mock, "Subscribe", reflect.TypeOf((*MockEventServiceClient)(nil).Subscribe), varargs...)
}

// MockEventServiceServer is a mock of EventServiceServer interface.
type MockEventServiceServer struct {
	ctrl     *gomock.Controller
	recorder *MockEventServiceServerMockRecorder
}

// MockEventServiceServerMockRecorder is the mock recorder for MockEventServiceServer.
type MockEventServiceServerMockRecorder struct {
	mock *MockEventServiceServer
}

// NewMockEventServiceServer creates a new mock instance.
func NewMockEventServiceServer(ctrl *gomock.Controller) *MockEventServiceServer {
	mock := &MockEventServiceServer{ctrl: ctrl}
	mock.recorder = &MockEventServiceServerMockRecorder{mock}
	return mock
}

// EXPECT returns an object that allows the caller to indicate expected use.
func (m *MockEventServiceServer) EXPECT() *MockEventServiceServerMockRecorder {
	return m.recorder
}

// Publish mocks base method.
func (m *MockEventServiceServer) Publish(arg0 context.Context, arg1 *PublishRequest) (*PublishResponse, error) {
	m.ctrl.T.Helper()
	ret := m.ctrl.Call(m, "Publish", arg0, arg1)
	ret0, _ := ret[0].(*PublishResponse)
	ret1, _ := ret[1].(error)
	return ret0, ret1
}

// Publish indicates an expected call of Publish.
func (mr *MockEventServiceServerMockRecorder) Publish(arg0, arg1 interface{}) *gomock.Call {
	mr.mock.ctrl.T.Helper()
	return mr.mock.ctrl.RecordCallWithMethodType(mr.mock, "Publish", reflect.TypeOf((*MockEventServiceServer)(nil).Publish), arg0, arg1)
}

// Start mocks base method.
func (m *MockEventServiceServer) Start(arg0 context.Context, arg1 *StartRequest) (*StartResponse, error) {
	m.ctrl.T.Helper()
	ret := m.ctrl.Call(m, "Start", arg0, arg1)
	ret0, _ := ret[0].(*StartResponse)
	ret1, _ := ret[1].(error)
	return ret0, ret1
}

// Start indicates an expected call of Start.
func (mr *MockEventServiceServerMockRecorder) Start(arg0, arg1 interface{}) *gomock.Call {
	mr.mock.ctrl.T.Helper()
	return mr.mock.ctrl.RecordCallWithMethodType(mr.mock, "Start", reflect.TypeOf((*MockEventServiceServer)(nil).Start), arg0, arg1)
}

// Stop mocks base method.
func (m *MockEventServiceServer) Stop(arg0 context.Context, arg1 *StopRequest) (*StopResponse, error) {
	m.ctrl.T.Helper()
	ret := m.ctrl.Call(m, "Stop", arg0, arg1)
	ret0, _ := ret[0].(*StopResponse)
	ret1, _ := ret[1].(error)
	return ret0, ret1
}

// Stop indicates an expected call of Stop.
func (mr *MockEventServiceServerMockRecorder) Stop(arg0, arg1 interface{}) *gomock.Call {
	mr.mock.ctrl.T.Helper()
	return mr.mock.ctrl.RecordCallWithMethodType(mr.mock, "Stop", reflect.TypeOf((*MockEventServiceServer)(nil).Stop), arg0, arg1)
}

// Subscribe mocks base method.
func (m *MockEventServiceServer) Subscribe(arg0 context.Context, arg1 *SubscribeRequest) (*SubscribeResponse, error) {
	m.ctrl.T.Helper()
	ret := m.ctrl.Call(m, "Subscribe", arg0, arg1)
	ret0, _ := ret[0].(*SubscribeResponse)
	ret1, _ := ret[1].(error)
	return ret0, ret1
}

// Subscribe indicates an expected call of Subscribe.
func (mr *MockEventServiceServerMockRecorder) Subscribe(arg0, arg1 interface{}) *gomock.Call {
	mr.mock.ctrl.T.Helper()
	return mr.mock.ctrl.RecordCallWithMethodType(mr.mock, "Subscribe", reflect.TypeOf((*MockEventServiceServer)(nil).Subscribe), arg0, arg1)
}
