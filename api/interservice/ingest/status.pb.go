// Code generated by protoc-gen-go. DO NOT EDIT.
// versions:
// 	protoc-gen-go v1.36.0
// 	protoc        v3.19.0
// source: interservice/ingest/status.proto

package ingest

import (
	context "context"
	_ "google.golang.org/genproto/googleapis/api/annotations"
	grpc "google.golang.org/grpc"
	codes "google.golang.org/grpc/codes"
	status "google.golang.org/grpc/status"
	protoreflect "google.golang.org/protobuf/reflect/protoreflect"
	protoimpl "google.golang.org/protobuf/runtime/protoimpl"
	reflect "reflect"
	sync "sync"
)

const (
	// Verify that this generated code is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(20 - protoimpl.MinVersion)
	// Verify that runtime/protoimpl is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(protoimpl.MaxVersion - 20)
)

// Health message
//
// The ingest-service health is constructed with:
//   - Status:
//     => ok:             Everything is alright
//     => initialization: The service is in its initialization process
//     => warning:        Something might be wrong?
//     => critical:       Something is wrong!
//
// @afiune: Here we can add more health information to the response
type Health struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	Status        string                 `protobuf:"bytes,1,opt,name=status,proto3" json:"status,omitempty" toml:"status,omitempty" mapstructure:"status,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *Health) Reset() {
	*x = Health{}
	mi := &file_interservice_ingest_status_proto_msgTypes[0]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *Health) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*Health) ProtoMessage() {}

func (x *Health) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_ingest_status_proto_msgTypes[0]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use Health.ProtoReflect.Descriptor instead.
func (*Health) Descriptor() ([]byte, []int) {
	return file_interservice_ingest_status_proto_rawDescGZIP(), []int{0}
}

func (x *Health) GetStatus() string {
	if x != nil {
		return x.Status
	}
	return ""
}

// Migration message
type MigrationStatus struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	Total         int64                  `protobuf:"varint,1,opt,name=total,proto3" json:"total,omitempty" toml:"total,omitempty" mapstructure:"total,omitempty"`
	Completed     int64                  `protobuf:"varint,2,opt,name=completed,proto3" json:"completed,omitempty" toml:"completed,omitempty" mapstructure:"completed,omitempty"`
	Status        string                 `protobuf:"bytes,3,opt,name=status,proto3" json:"status,omitempty" toml:"status,omitempty" mapstructure:"status,omitempty"`
	Finished      bool                   `protobuf:"varint,4,opt,name=finished,proto3" json:"finished,omitempty" toml:"finished,omitempty" mapstructure:"finished,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *MigrationStatus) Reset() {
	*x = MigrationStatus{}
	mi := &file_interservice_ingest_status_proto_msgTypes[1]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *MigrationStatus) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*MigrationStatus) ProtoMessage() {}

func (x *MigrationStatus) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_ingest_status_proto_msgTypes[1]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use MigrationStatus.ProtoReflect.Descriptor instead.
func (*MigrationStatus) Descriptor() ([]byte, []int) {
	return file_interservice_ingest_status_proto_rawDescGZIP(), []int{1}
}

func (x *MigrationStatus) GetTotal() int64 {
	if x != nil {
		return x.Total
	}
	return 0
}

func (x *MigrationStatus) GetCompleted() int64 {
	if x != nil {
		return x.Completed
	}
	return 0
}

func (x *MigrationStatus) GetStatus() string {
	if x != nil {
		return x.Status
	}
	return ""
}

func (x *MigrationStatus) GetFinished() bool {
	if x != nil {
		return x.Finished
	}
	return false
}

// Metrics message
type Metrics struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	Uptime        float64                `protobuf:"fixed64,5,opt,name=uptime,proto3" json:"uptime,omitempty" toml:"uptime,omitempty" mapstructure:"uptime,omitempty"`
	Pipeline      *PipelineMetrics       `protobuf:"bytes,6,opt,name=pipeline,proto3" json:"pipeline,omitempty" toml:"pipeline,omitempty" mapstructure:"pipeline,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *Metrics) Reset() {
	*x = Metrics{}
	mi := &file_interservice_ingest_status_proto_msgTypes[2]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *Metrics) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*Metrics) ProtoMessage() {}

func (x *Metrics) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_ingest_status_proto_msgTypes[2]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use Metrics.ProtoReflect.Descriptor instead.
func (*Metrics) Descriptor() ([]byte, []int) {
	return file_interservice_ingest_status_proto_rawDescGZIP(), []int{2}
}

func (x *Metrics) GetUptime() float64 {
	if x != nil {
		return x.Uptime
	}
	return 0
}

func (x *Metrics) GetPipeline() *PipelineMetrics {
	if x != nil {
		return x.Pipeline
	}
	return nil
}

type PipelineMetrics struct {
	state               protoimpl.MessageState `protogen:"open.v1"`
	TotalRunMessages    int64                  `protobuf:"varint,5,opt,name=total_run_messages,json=totalRunMessages,proto3" json:"total_run_messages,omitempty" toml:"total_run_messages,omitempty" mapstructure:"total_run_messages,omitempty"`
	TotalActionMessages int64                  `protobuf:"varint,6,opt,name=total_action_messages,json=totalActionMessages,proto3" json:"total_action_messages,omitempty" toml:"total_action_messages,omitempty" mapstructure:"total_action_messages,omitempty"`
	unknownFields       protoimpl.UnknownFields
	sizeCache           protoimpl.SizeCache
}

func (x *PipelineMetrics) Reset() {
	*x = PipelineMetrics{}
	mi := &file_interservice_ingest_status_proto_msgTypes[3]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *PipelineMetrics) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*PipelineMetrics) ProtoMessage() {}

func (x *PipelineMetrics) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_ingest_status_proto_msgTypes[3]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use PipelineMetrics.ProtoReflect.Descriptor instead.
func (*PipelineMetrics) Descriptor() ([]byte, []int) {
	return file_interservice_ingest_status_proto_rawDescGZIP(), []int{3}
}

func (x *PipelineMetrics) GetTotalRunMessages() int64 {
	if x != nil {
		return x.TotalRunMessages
	}
	return 0
}

func (x *PipelineMetrics) GetTotalActionMessages() int64 {
	if x != nil {
		return x.TotalActionMessages
	}
	return 0
}

type HealthRequest struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *HealthRequest) Reset() {
	*x = HealthRequest{}
	mi := &file_interservice_ingest_status_proto_msgTypes[4]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *HealthRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*HealthRequest) ProtoMessage() {}

func (x *HealthRequest) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_ingest_status_proto_msgTypes[4]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use HealthRequest.ProtoReflect.Descriptor instead.
func (*HealthRequest) Descriptor() ([]byte, []int) {
	return file_interservice_ingest_status_proto_rawDescGZIP(), []int{4}
}

type MetricsRequest struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *MetricsRequest) Reset() {
	*x = MetricsRequest{}
	mi := &file_interservice_ingest_status_proto_msgTypes[5]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *MetricsRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*MetricsRequest) ProtoMessage() {}

func (x *MetricsRequest) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_ingest_status_proto_msgTypes[5]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use MetricsRequest.ProtoReflect.Descriptor instead.
func (*MetricsRequest) Descriptor() ([]byte, []int) {
	return file_interservice_ingest_status_proto_rawDescGZIP(), []int{5}
}

type MigrationStatusRequest struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *MigrationStatusRequest) Reset() {
	*x = MigrationStatusRequest{}
	mi := &file_interservice_ingest_status_proto_msgTypes[6]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *MigrationStatusRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*MigrationStatusRequest) ProtoMessage() {}

func (x *MigrationStatusRequest) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_ingest_status_proto_msgTypes[6]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use MigrationStatusRequest.ProtoReflect.Descriptor instead.
func (*MigrationStatusRequest) Descriptor() ([]byte, []int) {
	return file_interservice_ingest_status_proto_rawDescGZIP(), []int{6}
}

var File_interservice_ingest_status_proto protoreflect.FileDescriptor

var file_interservice_ingest_status_proto_rawDesc = []byte{
	0x0a, 0x20, 0x69, 0x6e, 0x74, 0x65, 0x72, 0x73, 0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x2f, 0x69,
	0x6e, 0x67, 0x65, 0x73, 0x74, 0x2f, 0x73, 0x74, 0x61, 0x74, 0x75, 0x73, 0x2e, 0x70, 0x72, 0x6f,
	0x74, 0x6f, 0x12, 0x1b, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x61, 0x75, 0x74, 0x6f, 0x6d, 0x61, 0x74,
	0x65, 0x2e, 0x64, 0x6f, 0x6d, 0x61, 0x69, 0x6e, 0x2e, 0x69, 0x6e, 0x67, 0x65, 0x73, 0x74, 0x1a,
	0x1c, 0x67, 0x6f, 0x6f, 0x67, 0x6c, 0x65, 0x2f, 0x61, 0x70, 0x69, 0x2f, 0x61, 0x6e, 0x6e, 0x6f,
	0x74, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x73, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x22, 0x20, 0x0a,
	0x06, 0x48, 0x65, 0x61, 0x6c, 0x74, 0x68, 0x12, 0x16, 0x0a, 0x06, 0x73, 0x74, 0x61, 0x74, 0x75,
	0x73, 0x18, 0x01, 0x20, 0x01, 0x28, 0x09, 0x52, 0x06, 0x73, 0x74, 0x61, 0x74, 0x75, 0x73, 0x22,
	0x79, 0x0a, 0x0f, 0x4d, 0x69, 0x67, 0x72, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x53, 0x74, 0x61, 0x74,
	0x75, 0x73, 0x12, 0x14, 0x0a, 0x05, 0x74, 0x6f, 0x74, 0x61, 0x6c, 0x18, 0x01, 0x20, 0x01, 0x28,
	0x03, 0x52, 0x05, 0x74, 0x6f, 0x74, 0x61, 0x6c, 0x12, 0x1c, 0x0a, 0x09, 0x63, 0x6f, 0x6d, 0x70,
	0x6c, 0x65, 0x74, 0x65, 0x64, 0x18, 0x02, 0x20, 0x01, 0x28, 0x03, 0x52, 0x09, 0x63, 0x6f, 0x6d,
	0x70, 0x6c, 0x65, 0x74, 0x65, 0x64, 0x12, 0x16, 0x0a, 0x06, 0x73, 0x74, 0x61, 0x74, 0x75, 0x73,
	0x18, 0x03, 0x20, 0x01, 0x28, 0x09, 0x52, 0x06, 0x73, 0x74, 0x61, 0x74, 0x75, 0x73, 0x12, 0x1a,
	0x0a, 0x08, 0x66, 0x69, 0x6e, 0x69, 0x73, 0x68, 0x65, 0x64, 0x18, 0x04, 0x20, 0x01, 0x28, 0x08,
	0x52, 0x08, 0x66, 0x69, 0x6e, 0x69, 0x73, 0x68, 0x65, 0x64, 0x22, 0x6b, 0x0a, 0x07, 0x4d, 0x65,
	0x74, 0x72, 0x69, 0x63, 0x73, 0x12, 0x16, 0x0a, 0x06, 0x75, 0x70, 0x74, 0x69, 0x6d, 0x65, 0x18,
	0x05, 0x20, 0x01, 0x28, 0x01, 0x52, 0x06, 0x75, 0x70, 0x74, 0x69, 0x6d, 0x65, 0x12, 0x48, 0x0a,
	0x08, 0x70, 0x69, 0x70, 0x65, 0x6c, 0x69, 0x6e, 0x65, 0x18, 0x06, 0x20, 0x01, 0x28, 0x0b, 0x32,
	0x2c, 0x2e, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x61, 0x75, 0x74, 0x6f, 0x6d, 0x61, 0x74, 0x65, 0x2e,
	0x64, 0x6f, 0x6d, 0x61, 0x69, 0x6e, 0x2e, 0x69, 0x6e, 0x67, 0x65, 0x73, 0x74, 0x2e, 0x50, 0x69,
	0x70, 0x65, 0x6c, 0x69, 0x6e, 0x65, 0x4d, 0x65, 0x74, 0x72, 0x69, 0x63, 0x73, 0x52, 0x08, 0x70,
	0x69, 0x70, 0x65, 0x6c, 0x69, 0x6e, 0x65, 0x22, 0x73, 0x0a, 0x0f, 0x50, 0x69, 0x70, 0x65, 0x6c,
	0x69, 0x6e, 0x65, 0x4d, 0x65, 0x74, 0x72, 0x69, 0x63, 0x73, 0x12, 0x2c, 0x0a, 0x12, 0x74, 0x6f,
	0x74, 0x61, 0x6c, 0x5f, 0x72, 0x75, 0x6e, 0x5f, 0x6d, 0x65, 0x73, 0x73, 0x61, 0x67, 0x65, 0x73,
	0x18, 0x05, 0x20, 0x01, 0x28, 0x03, 0x52, 0x10, 0x74, 0x6f, 0x74, 0x61, 0x6c, 0x52, 0x75, 0x6e,
	0x4d, 0x65, 0x73, 0x73, 0x61, 0x67, 0x65, 0x73, 0x12, 0x32, 0x0a, 0x15, 0x74, 0x6f, 0x74, 0x61,
	0x6c, 0x5f, 0x61, 0x63, 0x74, 0x69, 0x6f, 0x6e, 0x5f, 0x6d, 0x65, 0x73, 0x73, 0x61, 0x67, 0x65,
	0x73, 0x18, 0x06, 0x20, 0x01, 0x28, 0x03, 0x52, 0x13, 0x74, 0x6f, 0x74, 0x61, 0x6c, 0x41, 0x63,
	0x74, 0x69, 0x6f, 0x6e, 0x4d, 0x65, 0x73, 0x73, 0x61, 0x67, 0x65, 0x73, 0x22, 0x0f, 0x0a, 0x0d,
	0x48, 0x65, 0x61, 0x6c, 0x74, 0x68, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x22, 0x10, 0x0a,
	0x0e, 0x4d, 0x65, 0x74, 0x72, 0x69, 0x63, 0x73, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x22,
	0x18, 0x0a, 0x16, 0x4d, 0x69, 0x67, 0x72, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x53, 0x74, 0x61, 0x74,
	0x75, 0x73, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x32, 0x81, 0x03, 0x0a, 0x13, 0x49, 0x6e,
	0x67, 0x65, 0x73, 0x74, 0x53, 0x74, 0x61, 0x74, 0x75, 0x73, 0x53, 0x65, 0x72, 0x76, 0x69, 0x63,
	0x65, 0x12, 0x5f, 0x0a, 0x0a, 0x47, 0x65, 0x74, 0x4d, 0x65, 0x74, 0x72, 0x69, 0x63, 0x73, 0x12,
	0x2b, 0x2e, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x61, 0x75, 0x74, 0x6f, 0x6d, 0x61, 0x74, 0x65, 0x2e,
	0x64, 0x6f, 0x6d, 0x61, 0x69, 0x6e, 0x2e, 0x69, 0x6e, 0x67, 0x65, 0x73, 0x74, 0x2e, 0x4d, 0x65,
	0x74, 0x72, 0x69, 0x63, 0x73, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x1a, 0x24, 0x2e, 0x63,
	0x68, 0x65, 0x66, 0x2e, 0x61, 0x75, 0x74, 0x6f, 0x6d, 0x61, 0x74, 0x65, 0x2e, 0x64, 0x6f, 0x6d,
	0x61, 0x69, 0x6e, 0x2e, 0x69, 0x6e, 0x67, 0x65, 0x73, 0x74, 0x2e, 0x4d, 0x65, 0x74, 0x72, 0x69,
	0x63, 0x73, 0x12, 0x74, 0x0a, 0x09, 0x47, 0x65, 0x74, 0x48, 0x65, 0x61, 0x6c, 0x74, 0x68, 0x12,
	0x2a, 0x2e, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x61, 0x75, 0x74, 0x6f, 0x6d, 0x61, 0x74, 0x65, 0x2e,
	0x64, 0x6f, 0x6d, 0x61, 0x69, 0x6e, 0x2e, 0x69, 0x6e, 0x67, 0x65, 0x73, 0x74, 0x2e, 0x48, 0x65,
	0x61, 0x6c, 0x74, 0x68, 0x52, 0x65, 0x71, 0x75, 0x65, 0x73, 0x74, 0x1a, 0x23, 0x2e, 0x63, 0x68,
	0x65, 0x66, 0x2e, 0x61, 0x75, 0x74, 0x6f, 0x6d, 0x61, 0x74, 0x65, 0x2e, 0x64, 0x6f, 0x6d, 0x61,
	0x69, 0x6e, 0x2e, 0x69, 0x6e, 0x67, 0x65, 0x73, 0x74, 0x2e, 0x48, 0x65, 0x61, 0x6c, 0x74, 0x68,
	0x22, 0x16, 0x82, 0xd3, 0xe4, 0x93, 0x02, 0x10, 0x12, 0x0e, 0x2f, 0x61, 0x70, 0x69, 0x2f, 0x76,
	0x30, 0x2f, 0x68, 0x65, 0x61, 0x6c, 0x74, 0x68, 0x12, 0x92, 0x01, 0x0a, 0x12, 0x47, 0x65, 0x74,
	0x4d, 0x69, 0x67, 0x72, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x53, 0x74, 0x61, 0x74, 0x75, 0x73, 0x12,
	0x33, 0x2e, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x61, 0x75, 0x74, 0x6f, 0x6d, 0x61, 0x74, 0x65, 0x2e,
	0x64, 0x6f, 0x6d, 0x61, 0x69, 0x6e, 0x2e, 0x69, 0x6e, 0x67, 0x65, 0x73, 0x74, 0x2e, 0x4d, 0x69,
	0x67, 0x72, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x53, 0x74, 0x61, 0x74, 0x75, 0x73, 0x52, 0x65, 0x71,
	0x75, 0x65, 0x73, 0x74, 0x1a, 0x2c, 0x2e, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x61, 0x75, 0x74, 0x6f,
	0x6d, 0x61, 0x74, 0x65, 0x2e, 0x64, 0x6f, 0x6d, 0x61, 0x69, 0x6e, 0x2e, 0x69, 0x6e, 0x67, 0x65,
	0x73, 0x74, 0x2e, 0x4d, 0x69, 0x67, 0x72, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x53, 0x74, 0x61, 0x74,
	0x75, 0x73, 0x22, 0x19, 0x82, 0xd3, 0xe4, 0x93, 0x02, 0x13, 0x12, 0x11, 0x2f, 0x61, 0x70, 0x69,
	0x2f, 0x76, 0x30, 0x2f, 0x6d, 0x69, 0x67, 0x72, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x42, 0x32, 0x5a,
	0x30, 0x67, 0x69, 0x74, 0x68, 0x75, 0x62, 0x2e, 0x63, 0x6f, 0x6d, 0x2f, 0x63, 0x68, 0x65, 0x66,
	0x2f, 0x61, 0x75, 0x74, 0x6f, 0x6d, 0x61, 0x74, 0x65, 0x2f, 0x61, 0x70, 0x69, 0x2f, 0x69, 0x6e,
	0x74, 0x65, 0x72, 0x73, 0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x2f, 0x69, 0x6e, 0x67, 0x65, 0x73,
	0x74, 0x62, 0x06, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x33,
}

var (
	file_interservice_ingest_status_proto_rawDescOnce sync.Once
	file_interservice_ingest_status_proto_rawDescData = file_interservice_ingest_status_proto_rawDesc
)

func file_interservice_ingest_status_proto_rawDescGZIP() []byte {
	file_interservice_ingest_status_proto_rawDescOnce.Do(func() {
		file_interservice_ingest_status_proto_rawDescData = protoimpl.X.CompressGZIP(file_interservice_ingest_status_proto_rawDescData)
	})
	return file_interservice_ingest_status_proto_rawDescData
}

var file_interservice_ingest_status_proto_msgTypes = make([]protoimpl.MessageInfo, 7)
var file_interservice_ingest_status_proto_goTypes = []any{
	(*Health)(nil),                 // 0: chef.automate.domain.ingest.Health
	(*MigrationStatus)(nil),        // 1: chef.automate.domain.ingest.MigrationStatus
	(*Metrics)(nil),                // 2: chef.automate.domain.ingest.Metrics
	(*PipelineMetrics)(nil),        // 3: chef.automate.domain.ingest.PipelineMetrics
	(*HealthRequest)(nil),          // 4: chef.automate.domain.ingest.HealthRequest
	(*MetricsRequest)(nil),         // 5: chef.automate.domain.ingest.MetricsRequest
	(*MigrationStatusRequest)(nil), // 6: chef.automate.domain.ingest.MigrationStatusRequest
}
var file_interservice_ingest_status_proto_depIdxs = []int32{
	3, // 0: chef.automate.domain.ingest.Metrics.pipeline:type_name -> chef.automate.domain.ingest.PipelineMetrics
	5, // 1: chef.automate.domain.ingest.IngestStatusService.GetMetrics:input_type -> chef.automate.domain.ingest.MetricsRequest
	4, // 2: chef.automate.domain.ingest.IngestStatusService.GetHealth:input_type -> chef.automate.domain.ingest.HealthRequest
	6, // 3: chef.automate.domain.ingest.IngestStatusService.GetMigrationStatus:input_type -> chef.automate.domain.ingest.MigrationStatusRequest
	2, // 4: chef.automate.domain.ingest.IngestStatusService.GetMetrics:output_type -> chef.automate.domain.ingest.Metrics
	0, // 5: chef.automate.domain.ingest.IngestStatusService.GetHealth:output_type -> chef.automate.domain.ingest.Health
	1, // 6: chef.automate.domain.ingest.IngestStatusService.GetMigrationStatus:output_type -> chef.automate.domain.ingest.MigrationStatus
	4, // [4:7] is the sub-list for method output_type
	1, // [1:4] is the sub-list for method input_type
	1, // [1:1] is the sub-list for extension type_name
	1, // [1:1] is the sub-list for extension extendee
	0, // [0:1] is the sub-list for field type_name
}

func init() { file_interservice_ingest_status_proto_init() }
func file_interservice_ingest_status_proto_init() {
	if File_interservice_ingest_status_proto != nil {
		return
	}
	type x struct{}
	out := protoimpl.TypeBuilder{
		File: protoimpl.DescBuilder{
			GoPackagePath: reflect.TypeOf(x{}).PkgPath(),
			RawDescriptor: file_interservice_ingest_status_proto_rawDesc,
			NumEnums:      0,
			NumMessages:   7,
			NumExtensions: 0,
			NumServices:   1,
		},
		GoTypes:           file_interservice_ingest_status_proto_goTypes,
		DependencyIndexes: file_interservice_ingest_status_proto_depIdxs,
		MessageInfos:      file_interservice_ingest_status_proto_msgTypes,
	}.Build()
	File_interservice_ingest_status_proto = out.File
	file_interservice_ingest_status_proto_rawDesc = nil
	file_interservice_ingest_status_proto_goTypes = nil
	file_interservice_ingest_status_proto_depIdxs = nil
}

// Reference imports to suppress errors if they are not otherwise used.
var _ context.Context
var _ grpc.ClientConnInterface

// This is a compile-time assertion to ensure that this generated file
// is compatible with the grpc package it is being compiled against.
const _ = grpc.SupportPackageIsVersion6

// IngestStatusServiceClient is the client API for IngestStatusService service.
//
// For semantics around ctx use and closing/ending streaming RPCs, please refer to https://godoc.org/google.golang.org/grpc#ClientConn.NewStream.
type IngestStatusServiceClient interface {
	GetMetrics(ctx context.Context, in *MetricsRequest, opts ...grpc.CallOption) (*Metrics, error)
	GetHealth(ctx context.Context, in *HealthRequest, opts ...grpc.CallOption) (*Health, error)
	GetMigrationStatus(ctx context.Context, in *MigrationStatusRequest, opts ...grpc.CallOption) (*MigrationStatus, error)
}

type ingestStatusServiceClient struct {
	cc grpc.ClientConnInterface
}

func NewIngestStatusServiceClient(cc grpc.ClientConnInterface) IngestStatusServiceClient {
	return &ingestStatusServiceClient{cc}
}

func (c *ingestStatusServiceClient) GetMetrics(ctx context.Context, in *MetricsRequest, opts ...grpc.CallOption) (*Metrics, error) {
	out := new(Metrics)
	err := c.cc.Invoke(ctx, "/chef.automate.domain.ingest.IngestStatusService/GetMetrics", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *ingestStatusServiceClient) GetHealth(ctx context.Context, in *HealthRequest, opts ...grpc.CallOption) (*Health, error) {
	out := new(Health)
	err := c.cc.Invoke(ctx, "/chef.automate.domain.ingest.IngestStatusService/GetHealth", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *ingestStatusServiceClient) GetMigrationStatus(ctx context.Context, in *MigrationStatusRequest, opts ...grpc.CallOption) (*MigrationStatus, error) {
	out := new(MigrationStatus)
	err := c.cc.Invoke(ctx, "/chef.automate.domain.ingest.IngestStatusService/GetMigrationStatus", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// IngestStatusServiceServer is the server API for IngestStatusService service.
type IngestStatusServiceServer interface {
	GetMetrics(context.Context, *MetricsRequest) (*Metrics, error)
	GetHealth(context.Context, *HealthRequest) (*Health, error)
	GetMigrationStatus(context.Context, *MigrationStatusRequest) (*MigrationStatus, error)
}

// UnimplementedIngestStatusServiceServer can be embedded to have forward compatible implementations.
type UnimplementedIngestStatusServiceServer struct {
}

func (*UnimplementedIngestStatusServiceServer) GetMetrics(context.Context, *MetricsRequest) (*Metrics, error) {
	return nil, status.Errorf(codes.Unimplemented, "method GetMetrics not implemented")
}
func (*UnimplementedIngestStatusServiceServer) GetHealth(context.Context, *HealthRequest) (*Health, error) {
	return nil, status.Errorf(codes.Unimplemented, "method GetHealth not implemented")
}
func (*UnimplementedIngestStatusServiceServer) GetMigrationStatus(context.Context, *MigrationStatusRequest) (*MigrationStatus, error) {
	return nil, status.Errorf(codes.Unimplemented, "method GetMigrationStatus not implemented")
}

func RegisterIngestStatusServiceServer(s *grpc.Server, srv IngestStatusServiceServer) {
	s.RegisterService(&_IngestStatusService_serviceDesc, srv)
}

func _IngestStatusService_GetMetrics_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(MetricsRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(IngestStatusServiceServer).GetMetrics(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: "/chef.automate.domain.ingest.IngestStatusService/GetMetrics",
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(IngestStatusServiceServer).GetMetrics(ctx, req.(*MetricsRequest))
	}
	return interceptor(ctx, in, info, handler)
}

func _IngestStatusService_GetHealth_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(HealthRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(IngestStatusServiceServer).GetHealth(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: "/chef.automate.domain.ingest.IngestStatusService/GetHealth",
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(IngestStatusServiceServer).GetHealth(ctx, req.(*HealthRequest))
	}
	return interceptor(ctx, in, info, handler)
}

func _IngestStatusService_GetMigrationStatus_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(MigrationStatusRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(IngestStatusServiceServer).GetMigrationStatus(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: "/chef.automate.domain.ingest.IngestStatusService/GetMigrationStatus",
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(IngestStatusServiceServer).GetMigrationStatus(ctx, req.(*MigrationStatusRequest))
	}
	return interceptor(ctx, in, info, handler)
}

var _IngestStatusService_serviceDesc = grpc.ServiceDesc{
	ServiceName: "chef.automate.domain.ingest.IngestStatusService",
	HandlerType: (*IngestStatusServiceServer)(nil),
	Methods: []grpc.MethodDesc{
		{
			MethodName: "GetMetrics",
			Handler:    _IngestStatusService_GetMetrics_Handler,
		},
		{
			MethodName: "GetHealth",
			Handler:    _IngestStatusService_GetHealth_Handler,
		},
		{
			MethodName: "GetMigrationStatus",
			Handler:    _IngestStatusService_GetMigrationStatus_Handler,
		},
	},
	Streams:  []grpc.StreamDesc{},
	Metadata: "interservice/ingest/status.proto",
}
