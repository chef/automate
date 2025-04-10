// Local Copy of github.com/habitat-sh/habitat/master/components/sup/protocols/event.proto
// to update, use update-event-proto.sh
// DO NOT EDIT
// local modification: must specify proto version at the very top

// Code generated by protoc-gen-go. DO NOT EDIT.
// versions:
// 	protoc-gen-go v1.36.0
// 	protoc        v3.19.0
// source: external/habitat/event.proto

package habitat

import (
	protoreflect "google.golang.org/protobuf/reflect/protoreflect"
	protoimpl "google.golang.org/protobuf/runtime/protoimpl"
	durationpb "google.golang.org/protobuf/types/known/durationpb"
	timestamppb "google.golang.org/protobuf/types/known/timestamppb"
	wrapperspb "google.golang.org/protobuf/types/known/wrapperspb"
	reflect "reflect"
	sync "sync"
)

const (
	// Verify that this generated code is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(20 - protoimpl.MinVersion)
	// Verify that runtime/protoimpl is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(protoimpl.MaxVersion - 20)
)

// Note that there is no `None` value, as currently exists in the Rust
// code for the Supervisor. Here we are only going to deal with the
// actual update strategies (because `None` isn't really a strategy!)
type UpdateStrategy int32

const (
	// Service updates are applied immediately as they are available,
	// with no additional coordination.
	UpdateStrategy_AtOnce UpdateStrategy = 0
	// Updates are applied one-by-one throughout a service group to
	// prevent everything from updating all at once.
	UpdateStrategy_Rolling UpdateStrategy = 1
)

// Enum value maps for UpdateStrategy.
var (
	UpdateStrategy_name = map[int32]string{
		0: "AtOnce",
		1: "Rolling",
	}
	UpdateStrategy_value = map[string]int32{
		"AtOnce":  0,
		"Rolling": 1,
	}
)

func (x UpdateStrategy) Enum() *UpdateStrategy {
	p := new(UpdateStrategy)
	*p = x
	return p
}

func (x UpdateStrategy) String() string {
	return protoimpl.X.EnumStringOf(x.Descriptor(), protoreflect.EnumNumber(x))
}

func (UpdateStrategy) Descriptor() protoreflect.EnumDescriptor {
	return file_external_habitat_event_proto_enumTypes[0].Descriptor()
}

func (UpdateStrategy) Type() protoreflect.EnumType {
	return &file_external_habitat_event_proto_enumTypes[0]
}

func (x UpdateStrategy) Number() protoreflect.EnumNumber {
	return protoreflect.EnumNumber(x)
}

// Deprecated: Use UpdateStrategy.Descriptor instead.
func (UpdateStrategy) EnumDescriptor() ([]byte, []int) {
	return file_external_habitat_event_proto_rawDescGZIP(), []int{0}
}

type HealthCheckResult int32

const (
	HealthCheckResult_Ok       HealthCheckResult = 0
	HealthCheckResult_Warning  HealthCheckResult = 1
	HealthCheckResult_Critical HealthCheckResult = 2
	HealthCheckResult_Unknown  HealthCheckResult = 3
)

// Enum value maps for HealthCheckResult.
var (
	HealthCheckResult_name = map[int32]string{
		0: "Ok",
		1: "Warning",
		2: "Critical",
		3: "Unknown",
	}
	HealthCheckResult_value = map[string]int32{
		"Ok":       0,
		"Warning":  1,
		"Critical": 2,
		"Unknown":  3,
	}
)

func (x HealthCheckResult) Enum() *HealthCheckResult {
	p := new(HealthCheckResult)
	*p = x
	return p
}

func (x HealthCheckResult) String() string {
	return protoimpl.X.EnumStringOf(x.Descriptor(), protoreflect.EnumNumber(x))
}

func (HealthCheckResult) Descriptor() protoreflect.EnumDescriptor {
	return file_external_habitat_event_proto_enumTypes[1].Descriptor()
}

func (HealthCheckResult) Type() protoreflect.EnumType {
	return &file_external_habitat_event_proto_enumTypes[1]
}

func (x HealthCheckResult) Number() protoreflect.EnumNumber {
	return protoreflect.EnumNumber(x)
}

// Deprecated: Use HealthCheckResult.Descriptor instead.
func (HealthCheckResult) EnumDescriptor() ([]byte, []int) {
	return file_external_habitat_event_proto_rawDescGZIP(), []int{1}
}

type EventMetadata struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// The unique ID of this Supervisor.
	SupervisorId string `protobuf:"bytes,1,opt,name=supervisor_id,json=supervisorId,proto3" json:"supervisor_id,omitempty"`
	// The IP address on which the Supervisor is listening for gossip.
	IpAddress     string                 `protobuf:"bytes,2,opt,name=ip_address,json=ipAddress,proto3" json:"ip_address,omitempty"`
	OccurredAt    *timestamppb.Timestamp `protobuf:"bytes,3,opt,name=occurred_at,json=occurredAt,proto3" json:"occurred_at,omitempty"`
	Application   string                 `protobuf:"bytes,4,opt,name=application,proto3" json:"application,omitempty"`
	Environment   string                 `protobuf:"bytes,5,opt,name=environment,proto3" json:"environment,omitempty"`
	Meta          map[string]string      `protobuf:"bytes,6,rep,name=meta,proto3" json:"meta,omitempty" protobuf_key:"bytes,1,opt,name=key" protobuf_val:"bytes,2,opt,name=value"`
	Fqdn          string                 `protobuf:"bytes,7,opt,name=fqdn,proto3" json:"fqdn,omitempty"`
	Site          string                 `protobuf:"bytes,8,opt,name=site,proto3" json:"site,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *EventMetadata) Reset() {
	*x = EventMetadata{}
	mi := &file_external_habitat_event_proto_msgTypes[0]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *EventMetadata) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*EventMetadata) ProtoMessage() {}

func (x *EventMetadata) ProtoReflect() protoreflect.Message {
	mi := &file_external_habitat_event_proto_msgTypes[0]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use EventMetadata.ProtoReflect.Descriptor instead.
func (*EventMetadata) Descriptor() ([]byte, []int) {
	return file_external_habitat_event_proto_rawDescGZIP(), []int{0}
}

func (x *EventMetadata) GetSupervisorId() string {
	if x != nil {
		return x.SupervisorId
	}
	return ""
}

func (x *EventMetadata) GetIpAddress() string {
	if x != nil {
		return x.IpAddress
	}
	return ""
}

func (x *EventMetadata) GetOccurredAt() *timestamppb.Timestamp {
	if x != nil {
		return x.OccurredAt
	}
	return nil
}

func (x *EventMetadata) GetApplication() string {
	if x != nil {
		return x.Application
	}
	return ""
}

func (x *EventMetadata) GetEnvironment() string {
	if x != nil {
		return x.Environment
	}
	return ""
}

func (x *EventMetadata) GetMeta() map[string]string {
	if x != nil {
		return x.Meta
	}
	return nil
}

func (x *EventMetadata) GetFqdn() string {
	if x != nil {
		return x.Fqdn
	}
	return ""
}

func (x *EventMetadata) GetSite() string {
	if x != nil {
		return x.Site
	}
	return ""
}

type ServiceMetadata struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// The fully-qualified identifier of the package that is currently
	// running.
	PackageIdent string `protobuf:"bytes,1,opt,name=package_ident,json=packageIdent,proto3" json:"package_ident,omitempty"`
	// This is the identifier the service was loaded with (e.g., the
	// `core/redis` in `hab svc load core/redis`). While it _can_ be a
	// fully-qualified package identifier, it is more commonly a 2- or
	// 3-part identifier variant.
	SpecIdent     string        `protobuf:"bytes,2,opt,name=spec_ident,json=specIdent,proto3" json:"spec_ident,omitempty"`
	ServiceGroup  string        `protobuf:"bytes,3,opt,name=service_group,json=serviceGroup,proto3" json:"service_group,omitempty"`
	UpdateConfig  *UpdateConfig `protobuf:"bytes,4,opt,name=update_config,json=updateConfig,proto3" json:"update_config,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *ServiceMetadata) Reset() {
	*x = ServiceMetadata{}
	mi := &file_external_habitat_event_proto_msgTypes[1]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ServiceMetadata) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ServiceMetadata) ProtoMessage() {}

func (x *ServiceMetadata) ProtoReflect() protoreflect.Message {
	mi := &file_external_habitat_event_proto_msgTypes[1]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ServiceMetadata.ProtoReflect.Descriptor instead.
func (*ServiceMetadata) Descriptor() ([]byte, []int) {
	return file_external_habitat_event_proto_rawDescGZIP(), []int{1}
}

func (x *ServiceMetadata) GetPackageIdent() string {
	if x != nil {
		return x.PackageIdent
	}
	return ""
}

func (x *ServiceMetadata) GetSpecIdent() string {
	if x != nil {
		return x.SpecIdent
	}
	return ""
}

func (x *ServiceMetadata) GetServiceGroup() string {
	if x != nil {
		return x.ServiceGroup
	}
	return ""
}

func (x *ServiceMetadata) GetUpdateConfig() *UpdateConfig {
	if x != nil {
		return x.UpdateConfig
	}
	return nil
}

// Encapsulates an update strategy with the channel that is checked
// for updates. Both must be present for either to be meaningful.a
type UpdateConfig struct {
	state    protoimpl.MessageState `protogen:"open.v1"`
	Strategy UpdateStrategy         `protobuf:"varint,1,opt,name=strategy,proto3,enum=chef.habitat.supervisor.event.UpdateStrategy" json:"strategy,omitempty"`
	// The channel the Supervisor will monitor for updates to this
	// service.
	Channel       string `protobuf:"bytes,2,opt,name=channel,proto3" json:"channel,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *UpdateConfig) Reset() {
	*x = UpdateConfig{}
	mi := &file_external_habitat_event_proto_msgTypes[2]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *UpdateConfig) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*UpdateConfig) ProtoMessage() {}

func (x *UpdateConfig) ProtoReflect() protoreflect.Message {
	mi := &file_external_habitat_event_proto_msgTypes[2]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use UpdateConfig.ProtoReflect.Descriptor instead.
func (*UpdateConfig) Descriptor() ([]byte, []int) {
	return file_external_habitat_event_proto_rawDescGZIP(), []int{2}
}

func (x *UpdateConfig) GetStrategy() UpdateStrategy {
	if x != nil {
		return x.Strategy
	}
	return UpdateStrategy_AtOnce
}

func (x *UpdateConfig) GetChannel() string {
	if x != nil {
		return x.Channel
	}
	return ""
}

type ServiceStartedEvent struct {
	state           protoimpl.MessageState `protogen:"open.v1"`
	EventMetadata   *EventMetadata         `protobuf:"bytes,1,opt,name=event_metadata,json=eventMetadata,proto3" json:"event_metadata,omitempty"`
	ServiceMetadata *ServiceMetadata       `protobuf:"bytes,2,opt,name=service_metadata,json=serviceMetadata,proto3" json:"service_metadata,omitempty"`
	unknownFields   protoimpl.UnknownFields
	sizeCache       protoimpl.SizeCache
}

func (x *ServiceStartedEvent) Reset() {
	*x = ServiceStartedEvent{}
	mi := &file_external_habitat_event_proto_msgTypes[3]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ServiceStartedEvent) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ServiceStartedEvent) ProtoMessage() {}

func (x *ServiceStartedEvent) ProtoReflect() protoreflect.Message {
	mi := &file_external_habitat_event_proto_msgTypes[3]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ServiceStartedEvent.ProtoReflect.Descriptor instead.
func (*ServiceStartedEvent) Descriptor() ([]byte, []int) {
	return file_external_habitat_event_proto_rawDescGZIP(), []int{3}
}

func (x *ServiceStartedEvent) GetEventMetadata() *EventMetadata {
	if x != nil {
		return x.EventMetadata
	}
	return nil
}

func (x *ServiceStartedEvent) GetServiceMetadata() *ServiceMetadata {
	if x != nil {
		return x.ServiceMetadata
	}
	return nil
}

type ServiceStoppedEvent struct {
	state           protoimpl.MessageState `protogen:"open.v1"`
	EventMetadata   *EventMetadata         `protobuf:"bytes,1,opt,name=event_metadata,json=eventMetadata,proto3" json:"event_metadata,omitempty"`
	ServiceMetadata *ServiceMetadata       `protobuf:"bytes,2,opt,name=service_metadata,json=serviceMetadata,proto3" json:"service_metadata,omitempty"`
	unknownFields   protoimpl.UnknownFields
	sizeCache       protoimpl.SizeCache
}

func (x *ServiceStoppedEvent) Reset() {
	*x = ServiceStoppedEvent{}
	mi := &file_external_habitat_event_proto_msgTypes[4]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ServiceStoppedEvent) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ServiceStoppedEvent) ProtoMessage() {}

func (x *ServiceStoppedEvent) ProtoReflect() protoreflect.Message {
	mi := &file_external_habitat_event_proto_msgTypes[4]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ServiceStoppedEvent.ProtoReflect.Descriptor instead.
func (*ServiceStoppedEvent) Descriptor() ([]byte, []int) {
	return file_external_habitat_event_proto_rawDescGZIP(), []int{4}
}

func (x *ServiceStoppedEvent) GetEventMetadata() *EventMetadata {
	if x != nil {
		return x.EventMetadata
	}
	return nil
}

func (x *ServiceStoppedEvent) GetServiceMetadata() *ServiceMetadata {
	if x != nil {
		return x.ServiceMetadata
	}
	return nil
}

type ServiceUpdateStartedEvent struct {
	state           protoimpl.MessageState `protogen:"open.v1"`
	EventMetadata   *EventMetadata         `protobuf:"bytes,1,opt,name=event_metadata,json=eventMetadata,proto3" json:"event_metadata,omitempty"`
	ServiceMetadata *ServiceMetadata       `protobuf:"bytes,2,opt,name=service_metadata,json=serviceMetadata,proto3" json:"service_metadata,omitempty"`
	// Details of the service pkg to which
	// we're attempting to update.This is
	// implemented as a string for now,
	// but at some point we'll may want
	// to create a type for this set of
	// attributes (FullyQualifiedPackageIdent)
	UpdatePackageIdent string `protobuf:"bytes,3,opt,name=update_package_ident,json=updatePackageIdent,proto3" json:"update_package_ident,omitempty"`
	unknownFields      protoimpl.UnknownFields
	sizeCache          protoimpl.SizeCache
}

func (x *ServiceUpdateStartedEvent) Reset() {
	*x = ServiceUpdateStartedEvent{}
	mi := &file_external_habitat_event_proto_msgTypes[5]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ServiceUpdateStartedEvent) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ServiceUpdateStartedEvent) ProtoMessage() {}

func (x *ServiceUpdateStartedEvent) ProtoReflect() protoreflect.Message {
	mi := &file_external_habitat_event_proto_msgTypes[5]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ServiceUpdateStartedEvent.ProtoReflect.Descriptor instead.
func (*ServiceUpdateStartedEvent) Descriptor() ([]byte, []int) {
	return file_external_habitat_event_proto_rawDescGZIP(), []int{5}
}

func (x *ServiceUpdateStartedEvent) GetEventMetadata() *EventMetadata {
	if x != nil {
		return x.EventMetadata
	}
	return nil
}

func (x *ServiceUpdateStartedEvent) GetServiceMetadata() *ServiceMetadata {
	if x != nil {
		return x.ServiceMetadata
	}
	return nil
}

func (x *ServiceUpdateStartedEvent) GetUpdatePackageIdent() string {
	if x != nil {
		return x.UpdatePackageIdent
	}
	return ""
}

type HealthCheckEvent struct {
	state           protoimpl.MessageState `protogen:"open.v1"`
	EventMetadata   *EventMetadata         `protobuf:"bytes,1,opt,name=event_metadata,json=eventMetadata,proto3" json:"event_metadata,omitempty"`
	ServiceMetadata *ServiceMetadata       `protobuf:"bytes,2,opt,name=service_metadata,json=serviceMetadata,proto3" json:"service_metadata,omitempty"`
	Result          HealthCheckResult      `protobuf:"varint,3,opt,name=result,proto3,enum=chef.habitat.supervisor.event.HealthCheckResult" json:"result,omitempty"`
	// If the service has a health check hook script, how long it took
	// to execute.
	Execution *durationpb.Duration `protobuf:"bytes,4,opt,name=execution,proto3" json:"execution,omitempty"`
	// The health check hook exit status
	ExitStatus *wrapperspb.Int32Value `protobuf:"bytes,5,opt,name=exit_status,json=exitStatus,proto3" json:"exit_status,omitempty"`
	// The health check hook stdout output
	Stdout *wrapperspb.StringValue `protobuf:"bytes,6,opt,name=stdout,proto3" json:"stdout,omitempty"`
	// The health check hook stderr output
	Stderr        *wrapperspb.StringValue `protobuf:"bytes,7,opt,name=stderr,proto3" json:"stderr,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *HealthCheckEvent) Reset() {
	*x = HealthCheckEvent{}
	mi := &file_external_habitat_event_proto_msgTypes[6]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *HealthCheckEvent) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*HealthCheckEvent) ProtoMessage() {}

func (x *HealthCheckEvent) ProtoReflect() protoreflect.Message {
	mi := &file_external_habitat_event_proto_msgTypes[6]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use HealthCheckEvent.ProtoReflect.Descriptor instead.
func (*HealthCheckEvent) Descriptor() ([]byte, []int) {
	return file_external_habitat_event_proto_rawDescGZIP(), []int{6}
}

func (x *HealthCheckEvent) GetEventMetadata() *EventMetadata {
	if x != nil {
		return x.EventMetadata
	}
	return nil
}

func (x *HealthCheckEvent) GetServiceMetadata() *ServiceMetadata {
	if x != nil {
		return x.ServiceMetadata
	}
	return nil
}

func (x *HealthCheckEvent) GetResult() HealthCheckResult {
	if x != nil {
		return x.Result
	}
	return HealthCheckResult_Ok
}

func (x *HealthCheckEvent) GetExecution() *durationpb.Duration {
	if x != nil {
		return x.Execution
	}
	return nil
}

func (x *HealthCheckEvent) GetExitStatus() *wrapperspb.Int32Value {
	if x != nil {
		return x.ExitStatus
	}
	return nil
}

func (x *HealthCheckEvent) GetStdout() *wrapperspb.StringValue {
	if x != nil {
		return x.Stdout
	}
	return nil
}

func (x *HealthCheckEvent) GetStderr() *wrapperspb.StringValue {
	if x != nil {
		return x.Stderr
	}
	return nil
}

var File_external_habitat_event_proto protoreflect.FileDescriptor

var file_external_habitat_event_proto_rawDesc = []byte{
	0x0a, 0x1c, 0x65, 0x78, 0x74, 0x65, 0x72, 0x6e, 0x61, 0x6c, 0x2f, 0x68, 0x61, 0x62, 0x69, 0x74,
	0x61, 0x74, 0x2f, 0x65, 0x76, 0x65, 0x6e, 0x74, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x12, 0x1d,
	0x63, 0x68, 0x65, 0x66, 0x2e, 0x68, 0x61, 0x62, 0x69, 0x74, 0x61, 0x74, 0x2e, 0x73, 0x75, 0x70,
	0x65, 0x72, 0x76, 0x69, 0x73, 0x6f, 0x72, 0x2e, 0x65, 0x76, 0x65, 0x6e, 0x74, 0x1a, 0x1e, 0x67,
	0x6f, 0x6f, 0x67, 0x6c, 0x65, 0x2f, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x62, 0x75, 0x66, 0x2f, 0x64,
	0x75, 0x72, 0x61, 0x74, 0x69, 0x6f, 0x6e, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x1a, 0x1f, 0x67,
	0x6f, 0x6f, 0x67, 0x6c, 0x65, 0x2f, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x62, 0x75, 0x66, 0x2f, 0x74,
	0x69, 0x6d, 0x65, 0x73, 0x74, 0x61, 0x6d, 0x70, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x1a, 0x1e,
	0x67, 0x6f, 0x6f, 0x67, 0x6c, 0x65, 0x2f, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x62, 0x75, 0x66, 0x2f,
	0x77, 0x72, 0x61, 0x70, 0x70, 0x65, 0x72, 0x73, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x22, 0x81,
	0x03, 0x0a, 0x0d, 0x45, 0x76, 0x65, 0x6e, 0x74, 0x4d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61,
	0x12, 0x23, 0x0a, 0x0d, 0x73, 0x75, 0x70, 0x65, 0x72, 0x76, 0x69, 0x73, 0x6f, 0x72, 0x5f, 0x69,
	0x64, 0x18, 0x01, 0x20, 0x01, 0x28, 0x09, 0x52, 0x0c, 0x73, 0x75, 0x70, 0x65, 0x72, 0x76, 0x69,
	0x73, 0x6f, 0x72, 0x49, 0x64, 0x12, 0x1d, 0x0a, 0x0a, 0x69, 0x70, 0x5f, 0x61, 0x64, 0x64, 0x72,
	0x65, 0x73, 0x73, 0x18, 0x02, 0x20, 0x01, 0x28, 0x09, 0x52, 0x09, 0x69, 0x70, 0x41, 0x64, 0x64,
	0x72, 0x65, 0x73, 0x73, 0x12, 0x3b, 0x0a, 0x0b, 0x6f, 0x63, 0x63, 0x75, 0x72, 0x72, 0x65, 0x64,
	0x5f, 0x61, 0x74, 0x18, 0x03, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x1a, 0x2e, 0x67, 0x6f, 0x6f, 0x67,
	0x6c, 0x65, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x62, 0x75, 0x66, 0x2e, 0x54, 0x69, 0x6d, 0x65,
	0x73, 0x74, 0x61, 0x6d, 0x70, 0x52, 0x0a, 0x6f, 0x63, 0x63, 0x75, 0x72, 0x72, 0x65, 0x64, 0x41,
	0x74, 0x12, 0x20, 0x0a, 0x0b, 0x61, 0x70, 0x70, 0x6c, 0x69, 0x63, 0x61, 0x74, 0x69, 0x6f, 0x6e,
	0x18, 0x04, 0x20, 0x01, 0x28, 0x09, 0x52, 0x0b, 0x61, 0x70, 0x70, 0x6c, 0x69, 0x63, 0x61, 0x74,
	0x69, 0x6f, 0x6e, 0x12, 0x20, 0x0a, 0x0b, 0x65, 0x6e, 0x76, 0x69, 0x72, 0x6f, 0x6e, 0x6d, 0x65,
	0x6e, 0x74, 0x18, 0x05, 0x20, 0x01, 0x28, 0x09, 0x52, 0x0b, 0x65, 0x6e, 0x76, 0x69, 0x72, 0x6f,
	0x6e, 0x6d, 0x65, 0x6e, 0x74, 0x12, 0x4a, 0x0a, 0x04, 0x6d, 0x65, 0x74, 0x61, 0x18, 0x06, 0x20,
	0x03, 0x28, 0x0b, 0x32, 0x36, 0x2e, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x68, 0x61, 0x62, 0x69, 0x74,
	0x61, 0x74, 0x2e, 0x73, 0x75, 0x70, 0x65, 0x72, 0x76, 0x69, 0x73, 0x6f, 0x72, 0x2e, 0x65, 0x76,
	0x65, 0x6e, 0x74, 0x2e, 0x45, 0x76, 0x65, 0x6e, 0x74, 0x4d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74,
	0x61, 0x2e, 0x4d, 0x65, 0x74, 0x61, 0x45, 0x6e, 0x74, 0x72, 0x79, 0x52, 0x04, 0x6d, 0x65, 0x74,
	0x61, 0x12, 0x12, 0x0a, 0x04, 0x66, 0x71, 0x64, 0x6e, 0x18, 0x07, 0x20, 0x01, 0x28, 0x09, 0x52,
	0x04, 0x66, 0x71, 0x64, 0x6e, 0x12, 0x12, 0x0a, 0x04, 0x73, 0x69, 0x74, 0x65, 0x18, 0x08, 0x20,
	0x01, 0x28, 0x09, 0x52, 0x04, 0x73, 0x69, 0x74, 0x65, 0x1a, 0x37, 0x0a, 0x09, 0x4d, 0x65, 0x74,
	0x61, 0x45, 0x6e, 0x74, 0x72, 0x79, 0x12, 0x10, 0x0a, 0x03, 0x6b, 0x65, 0x79, 0x18, 0x01, 0x20,
	0x01, 0x28, 0x09, 0x52, 0x03, 0x6b, 0x65, 0x79, 0x12, 0x14, 0x0a, 0x05, 0x76, 0x61, 0x6c, 0x75,
	0x65, 0x18, 0x02, 0x20, 0x01, 0x28, 0x09, 0x52, 0x05, 0x76, 0x61, 0x6c, 0x75, 0x65, 0x3a, 0x02,
	0x38, 0x01, 0x22, 0xcc, 0x01, 0x0a, 0x0f, 0x53, 0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x4d, 0x65,
	0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x12, 0x23, 0x0a, 0x0d, 0x70, 0x61, 0x63, 0x6b, 0x61, 0x67,
	0x65, 0x5f, 0x69, 0x64, 0x65, 0x6e, 0x74, 0x18, 0x01, 0x20, 0x01, 0x28, 0x09, 0x52, 0x0c, 0x70,
	0x61, 0x63, 0x6b, 0x61, 0x67, 0x65, 0x49, 0x64, 0x65, 0x6e, 0x74, 0x12, 0x1d, 0x0a, 0x0a, 0x73,
	0x70, 0x65, 0x63, 0x5f, 0x69, 0x64, 0x65, 0x6e, 0x74, 0x18, 0x02, 0x20, 0x01, 0x28, 0x09, 0x52,
	0x09, 0x73, 0x70, 0x65, 0x63, 0x49, 0x64, 0x65, 0x6e, 0x74, 0x12, 0x23, 0x0a, 0x0d, 0x73, 0x65,
	0x72, 0x76, 0x69, 0x63, 0x65, 0x5f, 0x67, 0x72, 0x6f, 0x75, 0x70, 0x18, 0x03, 0x20, 0x01, 0x28,
	0x09, 0x52, 0x0c, 0x73, 0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x47, 0x72, 0x6f, 0x75, 0x70, 0x12,
	0x50, 0x0a, 0x0d, 0x75, 0x70, 0x64, 0x61, 0x74, 0x65, 0x5f, 0x63, 0x6f, 0x6e, 0x66, 0x69, 0x67,
	0x18, 0x04, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x2b, 0x2e, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x68, 0x61,
	0x62, 0x69, 0x74, 0x61, 0x74, 0x2e, 0x73, 0x75, 0x70, 0x65, 0x72, 0x76, 0x69, 0x73, 0x6f, 0x72,
	0x2e, 0x65, 0x76, 0x65, 0x6e, 0x74, 0x2e, 0x55, 0x70, 0x64, 0x61, 0x74, 0x65, 0x43, 0x6f, 0x6e,
	0x66, 0x69, 0x67, 0x52, 0x0c, 0x75, 0x70, 0x64, 0x61, 0x74, 0x65, 0x43, 0x6f, 0x6e, 0x66, 0x69,
	0x67, 0x22, 0x73, 0x0a, 0x0c, 0x55, 0x70, 0x64, 0x61, 0x74, 0x65, 0x43, 0x6f, 0x6e, 0x66, 0x69,
	0x67, 0x12, 0x49, 0x0a, 0x08, 0x73, 0x74, 0x72, 0x61, 0x74, 0x65, 0x67, 0x79, 0x18, 0x01, 0x20,
	0x01, 0x28, 0x0e, 0x32, 0x2d, 0x2e, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x68, 0x61, 0x62, 0x69, 0x74,
	0x61, 0x74, 0x2e, 0x73, 0x75, 0x70, 0x65, 0x72, 0x76, 0x69, 0x73, 0x6f, 0x72, 0x2e, 0x65, 0x76,
	0x65, 0x6e, 0x74, 0x2e, 0x55, 0x70, 0x64, 0x61, 0x74, 0x65, 0x53, 0x74, 0x72, 0x61, 0x74, 0x65,
	0x67, 0x79, 0x52, 0x08, 0x73, 0x74, 0x72, 0x61, 0x74, 0x65, 0x67, 0x79, 0x12, 0x18, 0x0a, 0x07,
	0x63, 0x68, 0x61, 0x6e, 0x6e, 0x65, 0x6c, 0x18, 0x02, 0x20, 0x01, 0x28, 0x09, 0x52, 0x07, 0x63,
	0x68, 0x61, 0x6e, 0x6e, 0x65, 0x6c, 0x22, 0xc5, 0x01, 0x0a, 0x13, 0x53, 0x65, 0x72, 0x76, 0x69,
	0x63, 0x65, 0x53, 0x74, 0x61, 0x72, 0x74, 0x65, 0x64, 0x45, 0x76, 0x65, 0x6e, 0x74, 0x12, 0x53,
	0x0a, 0x0e, 0x65, 0x76, 0x65, 0x6e, 0x74, 0x5f, 0x6d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61,
	0x18, 0x01, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x2c, 0x2e, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x68, 0x61,
	0x62, 0x69, 0x74, 0x61, 0x74, 0x2e, 0x73, 0x75, 0x70, 0x65, 0x72, 0x76, 0x69, 0x73, 0x6f, 0x72,
	0x2e, 0x65, 0x76, 0x65, 0x6e, 0x74, 0x2e, 0x45, 0x76, 0x65, 0x6e, 0x74, 0x4d, 0x65, 0x74, 0x61,
	0x64, 0x61, 0x74, 0x61, 0x52, 0x0d, 0x65, 0x76, 0x65, 0x6e, 0x74, 0x4d, 0x65, 0x74, 0x61, 0x64,
	0x61, 0x74, 0x61, 0x12, 0x59, 0x0a, 0x10, 0x73, 0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x5f, 0x6d,
	0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x18, 0x02, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x2e, 0x2e,
	0x63, 0x68, 0x65, 0x66, 0x2e, 0x68, 0x61, 0x62, 0x69, 0x74, 0x61, 0x74, 0x2e, 0x73, 0x75, 0x70,
	0x65, 0x72, 0x76, 0x69, 0x73, 0x6f, 0x72, 0x2e, 0x65, 0x76, 0x65, 0x6e, 0x74, 0x2e, 0x53, 0x65,
	0x72, 0x76, 0x69, 0x63, 0x65, 0x4d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x52, 0x0f, 0x73,
	0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x4d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x22, 0xc5,
	0x01, 0x0a, 0x13, 0x53, 0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x53, 0x74, 0x6f, 0x70, 0x70, 0x65,
	0x64, 0x45, 0x76, 0x65, 0x6e, 0x74, 0x12, 0x53, 0x0a, 0x0e, 0x65, 0x76, 0x65, 0x6e, 0x74, 0x5f,
	0x6d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x18, 0x01, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x2c,
	0x2e, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x68, 0x61, 0x62, 0x69, 0x74, 0x61, 0x74, 0x2e, 0x73, 0x75,
	0x70, 0x65, 0x72, 0x76, 0x69, 0x73, 0x6f, 0x72, 0x2e, 0x65, 0x76, 0x65, 0x6e, 0x74, 0x2e, 0x45,
	0x76, 0x65, 0x6e, 0x74, 0x4d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x52, 0x0d, 0x65, 0x76,
	0x65, 0x6e, 0x74, 0x4d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x12, 0x59, 0x0a, 0x10, 0x73,
	0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x5f, 0x6d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x18,
	0x02, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x2e, 0x2e, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x68, 0x61, 0x62,
	0x69, 0x74, 0x61, 0x74, 0x2e, 0x73, 0x75, 0x70, 0x65, 0x72, 0x76, 0x69, 0x73, 0x6f, 0x72, 0x2e,
	0x65, 0x76, 0x65, 0x6e, 0x74, 0x2e, 0x53, 0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x4d, 0x65, 0x74,
	0x61, 0x64, 0x61, 0x74, 0x61, 0x52, 0x0f, 0x73, 0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x4d, 0x65,
	0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x22, 0xfd, 0x01, 0x0a, 0x19, 0x53, 0x65, 0x72, 0x76, 0x69,
	0x63, 0x65, 0x55, 0x70, 0x64, 0x61, 0x74, 0x65, 0x53, 0x74, 0x61, 0x72, 0x74, 0x65, 0x64, 0x45,
	0x76, 0x65, 0x6e, 0x74, 0x12, 0x53, 0x0a, 0x0e, 0x65, 0x76, 0x65, 0x6e, 0x74, 0x5f, 0x6d, 0x65,
	0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x18, 0x01, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x2c, 0x2e, 0x63,
	0x68, 0x65, 0x66, 0x2e, 0x68, 0x61, 0x62, 0x69, 0x74, 0x61, 0x74, 0x2e, 0x73, 0x75, 0x70, 0x65,
	0x72, 0x76, 0x69, 0x73, 0x6f, 0x72, 0x2e, 0x65, 0x76, 0x65, 0x6e, 0x74, 0x2e, 0x45, 0x76, 0x65,
	0x6e, 0x74, 0x4d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x52, 0x0d, 0x65, 0x76, 0x65, 0x6e,
	0x74, 0x4d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x12, 0x59, 0x0a, 0x10, 0x73, 0x65, 0x72,
	0x76, 0x69, 0x63, 0x65, 0x5f, 0x6d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x18, 0x02, 0x20,
	0x01, 0x28, 0x0b, 0x32, 0x2e, 0x2e, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x68, 0x61, 0x62, 0x69, 0x74,
	0x61, 0x74, 0x2e, 0x73, 0x75, 0x70, 0x65, 0x72, 0x76, 0x69, 0x73, 0x6f, 0x72, 0x2e, 0x65, 0x76,
	0x65, 0x6e, 0x74, 0x2e, 0x53, 0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x4d, 0x65, 0x74, 0x61, 0x64,
	0x61, 0x74, 0x61, 0x52, 0x0f, 0x73, 0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x4d, 0x65, 0x74, 0x61,
	0x64, 0x61, 0x74, 0x61, 0x12, 0x30, 0x0a, 0x14, 0x75, 0x70, 0x64, 0x61, 0x74, 0x65, 0x5f, 0x70,
	0x61, 0x63, 0x6b, 0x61, 0x67, 0x65, 0x5f, 0x69, 0x64, 0x65, 0x6e, 0x74, 0x18, 0x03, 0x20, 0x01,
	0x28, 0x09, 0x52, 0x12, 0x75, 0x70, 0x64, 0x61, 0x74, 0x65, 0x50, 0x61, 0x63, 0x6b, 0x61, 0x67,
	0x65, 0x49, 0x64, 0x65, 0x6e, 0x74, 0x22, 0xef, 0x03, 0x0a, 0x10, 0x48, 0x65, 0x61, 0x6c, 0x74,
	0x68, 0x43, 0x68, 0x65, 0x63, 0x6b, 0x45, 0x76, 0x65, 0x6e, 0x74, 0x12, 0x53, 0x0a, 0x0e, 0x65,
	0x76, 0x65, 0x6e, 0x74, 0x5f, 0x6d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x18, 0x01, 0x20,
	0x01, 0x28, 0x0b, 0x32, 0x2c, 0x2e, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x68, 0x61, 0x62, 0x69, 0x74,
	0x61, 0x74, 0x2e, 0x73, 0x75, 0x70, 0x65, 0x72, 0x76, 0x69, 0x73, 0x6f, 0x72, 0x2e, 0x65, 0x76,
	0x65, 0x6e, 0x74, 0x2e, 0x45, 0x76, 0x65, 0x6e, 0x74, 0x4d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74,
	0x61, 0x52, 0x0d, 0x65, 0x76, 0x65, 0x6e, 0x74, 0x4d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61,
	0x12, 0x59, 0x0a, 0x10, 0x73, 0x65, 0x72, 0x76, 0x69, 0x63, 0x65, 0x5f, 0x6d, 0x65, 0x74, 0x61,
	0x64, 0x61, 0x74, 0x61, 0x18, 0x02, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x2e, 0x2e, 0x63, 0x68, 0x65,
	0x66, 0x2e, 0x68, 0x61, 0x62, 0x69, 0x74, 0x61, 0x74, 0x2e, 0x73, 0x75, 0x70, 0x65, 0x72, 0x76,
	0x69, 0x73, 0x6f, 0x72, 0x2e, 0x65, 0x76, 0x65, 0x6e, 0x74, 0x2e, 0x53, 0x65, 0x72, 0x76, 0x69,
	0x63, 0x65, 0x4d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x52, 0x0f, 0x73, 0x65, 0x72, 0x76,
	0x69, 0x63, 0x65, 0x4d, 0x65, 0x74, 0x61, 0x64, 0x61, 0x74, 0x61, 0x12, 0x48, 0x0a, 0x06, 0x72,
	0x65, 0x73, 0x75, 0x6c, 0x74, 0x18, 0x03, 0x20, 0x01, 0x28, 0x0e, 0x32, 0x30, 0x2e, 0x63, 0x68,
	0x65, 0x66, 0x2e, 0x68, 0x61, 0x62, 0x69, 0x74, 0x61, 0x74, 0x2e, 0x73, 0x75, 0x70, 0x65, 0x72,
	0x76, 0x69, 0x73, 0x6f, 0x72, 0x2e, 0x65, 0x76, 0x65, 0x6e, 0x74, 0x2e, 0x48, 0x65, 0x61, 0x6c,
	0x74, 0x68, 0x43, 0x68, 0x65, 0x63, 0x6b, 0x52, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x52, 0x06, 0x72,
	0x65, 0x73, 0x75, 0x6c, 0x74, 0x12, 0x37, 0x0a, 0x09, 0x65, 0x78, 0x65, 0x63, 0x75, 0x74, 0x69,
	0x6f, 0x6e, 0x18, 0x04, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x19, 0x2e, 0x67, 0x6f, 0x6f, 0x67, 0x6c,
	0x65, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x62, 0x75, 0x66, 0x2e, 0x44, 0x75, 0x72, 0x61, 0x74,
	0x69, 0x6f, 0x6e, 0x52, 0x09, 0x65, 0x78, 0x65, 0x63, 0x75, 0x74, 0x69, 0x6f, 0x6e, 0x12, 0x3c,
	0x0a, 0x0b, 0x65, 0x78, 0x69, 0x74, 0x5f, 0x73, 0x74, 0x61, 0x74, 0x75, 0x73, 0x18, 0x05, 0x20,
	0x01, 0x28, 0x0b, 0x32, 0x1b, 0x2e, 0x67, 0x6f, 0x6f, 0x67, 0x6c, 0x65, 0x2e, 0x70, 0x72, 0x6f,
	0x74, 0x6f, 0x62, 0x75, 0x66, 0x2e, 0x49, 0x6e, 0x74, 0x33, 0x32, 0x56, 0x61, 0x6c, 0x75, 0x65,
	0x52, 0x0a, 0x65, 0x78, 0x69, 0x74, 0x53, 0x74, 0x61, 0x74, 0x75, 0x73, 0x12, 0x34, 0x0a, 0x06,
	0x73, 0x74, 0x64, 0x6f, 0x75, 0x74, 0x18, 0x06, 0x20, 0x01, 0x28, 0x0b, 0x32, 0x1c, 0x2e, 0x67,
	0x6f, 0x6f, 0x67, 0x6c, 0x65, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x62, 0x75, 0x66, 0x2e, 0x53,
	0x74, 0x72, 0x69, 0x6e, 0x67, 0x56, 0x61, 0x6c, 0x75, 0x65, 0x52, 0x06, 0x73, 0x74, 0x64, 0x6f,
	0x75, 0x74, 0x12, 0x34, 0x0a, 0x06, 0x73, 0x74, 0x64, 0x65, 0x72, 0x72, 0x18, 0x07, 0x20, 0x01,
	0x28, 0x0b, 0x32, 0x1c, 0x2e, 0x67, 0x6f, 0x6f, 0x67, 0x6c, 0x65, 0x2e, 0x70, 0x72, 0x6f, 0x74,
	0x6f, 0x62, 0x75, 0x66, 0x2e, 0x53, 0x74, 0x72, 0x69, 0x6e, 0x67, 0x56, 0x61, 0x6c, 0x75, 0x65,
	0x52, 0x06, 0x73, 0x74, 0x64, 0x65, 0x72, 0x72, 0x2a, 0x29, 0x0a, 0x0e, 0x55, 0x70, 0x64, 0x61,
	0x74, 0x65, 0x53, 0x74, 0x72, 0x61, 0x74, 0x65, 0x67, 0x79, 0x12, 0x0a, 0x0a, 0x06, 0x41, 0x74,
	0x4f, 0x6e, 0x63, 0x65, 0x10, 0x00, 0x12, 0x0b, 0x0a, 0x07, 0x52, 0x6f, 0x6c, 0x6c, 0x69, 0x6e,
	0x67, 0x10, 0x01, 0x2a, 0x43, 0x0a, 0x11, 0x48, 0x65, 0x61, 0x6c, 0x74, 0x68, 0x43, 0x68, 0x65,
	0x63, 0x6b, 0x52, 0x65, 0x73, 0x75, 0x6c, 0x74, 0x12, 0x06, 0x0a, 0x02, 0x4f, 0x6b, 0x10, 0x00,
	0x12, 0x0b, 0x0a, 0x07, 0x57, 0x61, 0x72, 0x6e, 0x69, 0x6e, 0x67, 0x10, 0x01, 0x12, 0x0c, 0x0a,
	0x08, 0x43, 0x72, 0x69, 0x74, 0x69, 0x63, 0x61, 0x6c, 0x10, 0x02, 0x12, 0x0b, 0x0a, 0x07, 0x55,
	0x6e, 0x6b, 0x6e, 0x6f, 0x77, 0x6e, 0x10, 0x03, 0x42, 0x2f, 0x5a, 0x2d, 0x67, 0x69, 0x74, 0x68,
	0x75, 0x62, 0x2e, 0x63, 0x6f, 0x6d, 0x2f, 0x63, 0x68, 0x65, 0x66, 0x2f, 0x61, 0x75, 0x74, 0x6f,
	0x6d, 0x61, 0x74, 0x65, 0x2f, 0x61, 0x70, 0x69, 0x2f, 0x65, 0x78, 0x74, 0x65, 0x72, 0x6e, 0x61,
	0x6c, 0x2f, 0x68, 0x61, 0x62, 0x69, 0x74, 0x61, 0x74, 0x62, 0x06, 0x70, 0x72, 0x6f, 0x74, 0x6f,
	0x33,
}

var (
	file_external_habitat_event_proto_rawDescOnce sync.Once
	file_external_habitat_event_proto_rawDescData = file_external_habitat_event_proto_rawDesc
)

func file_external_habitat_event_proto_rawDescGZIP() []byte {
	file_external_habitat_event_proto_rawDescOnce.Do(func() {
		file_external_habitat_event_proto_rawDescData = protoimpl.X.CompressGZIP(file_external_habitat_event_proto_rawDescData)
	})
	return file_external_habitat_event_proto_rawDescData
}

var file_external_habitat_event_proto_enumTypes = make([]protoimpl.EnumInfo, 2)
var file_external_habitat_event_proto_msgTypes = make([]protoimpl.MessageInfo, 8)
var file_external_habitat_event_proto_goTypes = []any{
	(UpdateStrategy)(0),               // 0: chef.habitat.supervisor.event.UpdateStrategy
	(HealthCheckResult)(0),            // 1: chef.habitat.supervisor.event.HealthCheckResult
	(*EventMetadata)(nil),             // 2: chef.habitat.supervisor.event.EventMetadata
	(*ServiceMetadata)(nil),           // 3: chef.habitat.supervisor.event.ServiceMetadata
	(*UpdateConfig)(nil),              // 4: chef.habitat.supervisor.event.UpdateConfig
	(*ServiceStartedEvent)(nil),       // 5: chef.habitat.supervisor.event.ServiceStartedEvent
	(*ServiceStoppedEvent)(nil),       // 6: chef.habitat.supervisor.event.ServiceStoppedEvent
	(*ServiceUpdateStartedEvent)(nil), // 7: chef.habitat.supervisor.event.ServiceUpdateStartedEvent
	(*HealthCheckEvent)(nil),          // 8: chef.habitat.supervisor.event.HealthCheckEvent
	nil,                               // 9: chef.habitat.supervisor.event.EventMetadata.MetaEntry
	(*timestamppb.Timestamp)(nil),     // 10: google.protobuf.Timestamp
	(*durationpb.Duration)(nil),       // 11: google.protobuf.Duration
	(*wrapperspb.Int32Value)(nil),     // 12: google.protobuf.Int32Value
	(*wrapperspb.StringValue)(nil),    // 13: google.protobuf.StringValue
}
var file_external_habitat_event_proto_depIdxs = []int32{
	10, // 0: chef.habitat.supervisor.event.EventMetadata.occurred_at:type_name -> google.protobuf.Timestamp
	9,  // 1: chef.habitat.supervisor.event.EventMetadata.meta:type_name -> chef.habitat.supervisor.event.EventMetadata.MetaEntry
	4,  // 2: chef.habitat.supervisor.event.ServiceMetadata.update_config:type_name -> chef.habitat.supervisor.event.UpdateConfig
	0,  // 3: chef.habitat.supervisor.event.UpdateConfig.strategy:type_name -> chef.habitat.supervisor.event.UpdateStrategy
	2,  // 4: chef.habitat.supervisor.event.ServiceStartedEvent.event_metadata:type_name -> chef.habitat.supervisor.event.EventMetadata
	3,  // 5: chef.habitat.supervisor.event.ServiceStartedEvent.service_metadata:type_name -> chef.habitat.supervisor.event.ServiceMetadata
	2,  // 6: chef.habitat.supervisor.event.ServiceStoppedEvent.event_metadata:type_name -> chef.habitat.supervisor.event.EventMetadata
	3,  // 7: chef.habitat.supervisor.event.ServiceStoppedEvent.service_metadata:type_name -> chef.habitat.supervisor.event.ServiceMetadata
	2,  // 8: chef.habitat.supervisor.event.ServiceUpdateStartedEvent.event_metadata:type_name -> chef.habitat.supervisor.event.EventMetadata
	3,  // 9: chef.habitat.supervisor.event.ServiceUpdateStartedEvent.service_metadata:type_name -> chef.habitat.supervisor.event.ServiceMetadata
	2,  // 10: chef.habitat.supervisor.event.HealthCheckEvent.event_metadata:type_name -> chef.habitat.supervisor.event.EventMetadata
	3,  // 11: chef.habitat.supervisor.event.HealthCheckEvent.service_metadata:type_name -> chef.habitat.supervisor.event.ServiceMetadata
	1,  // 12: chef.habitat.supervisor.event.HealthCheckEvent.result:type_name -> chef.habitat.supervisor.event.HealthCheckResult
	11, // 13: chef.habitat.supervisor.event.HealthCheckEvent.execution:type_name -> google.protobuf.Duration
	12, // 14: chef.habitat.supervisor.event.HealthCheckEvent.exit_status:type_name -> google.protobuf.Int32Value
	13, // 15: chef.habitat.supervisor.event.HealthCheckEvent.stdout:type_name -> google.protobuf.StringValue
	13, // 16: chef.habitat.supervisor.event.HealthCheckEvent.stderr:type_name -> google.protobuf.StringValue
	17, // [17:17] is the sub-list for method output_type
	17, // [17:17] is the sub-list for method input_type
	17, // [17:17] is the sub-list for extension type_name
	17, // [17:17] is the sub-list for extension extendee
	0,  // [0:17] is the sub-list for field type_name
}

func init() { file_external_habitat_event_proto_init() }
func file_external_habitat_event_proto_init() {
	if File_external_habitat_event_proto != nil {
		return
	}
	type x struct{}
	out := protoimpl.TypeBuilder{
		File: protoimpl.DescBuilder{
			GoPackagePath: reflect.TypeOf(x{}).PkgPath(),
			RawDescriptor: file_external_habitat_event_proto_rawDesc,
			NumEnums:      2,
			NumMessages:   8,
			NumExtensions: 0,
			NumServices:   0,
		},
		GoTypes:           file_external_habitat_event_proto_goTypes,
		DependencyIndexes: file_external_habitat_event_proto_depIdxs,
		EnumInfos:         file_external_habitat_event_proto_enumTypes,
		MessageInfos:      file_external_habitat_event_proto_msgTypes,
	}.Build()
	File_external_habitat_event_proto = out.File
	file_external_habitat_event_proto_rawDesc = nil
	file_external_habitat_event_proto_goTypes = nil
	file_external_habitat_event_proto_depIdxs = nil
}
