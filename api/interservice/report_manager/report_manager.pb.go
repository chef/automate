// Code generated by protoc-gen-go. DO NOT EDIT.
// versions:
// 	protoc-gen-go v1.36.6
// 	protoc        v3.19.0
// source: interservice/report_manager/report_manager.proto

package report_manager

import (
	context "context"
	grpc "google.golang.org/grpc"
	codes "google.golang.org/grpc/codes"
	status "google.golang.org/grpc/status"
	protoreflect "google.golang.org/protobuf/reflect/protoreflect"
	protoimpl "google.golang.org/protobuf/runtime/protoimpl"
	emptypb "google.golang.org/protobuf/types/known/emptypb"
	timestamppb "google.golang.org/protobuf/types/known/timestamppb"
	reflect "reflect"
	sync "sync"
	unsafe "unsafe"
)

const (
	// Verify that this generated code is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(20 - protoimpl.MinVersion)
	// Verify that runtime/protoimpl is sufficiently up-to-date.
	_ = protoimpl.EnforceVersion(protoimpl.MaxVersion - 20)
)

type ListFilter struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	Values        []string               `protobuf:"bytes,1,rep,name=values,proto3" json:"values,omitempty" toml:"values,omitempty" mapstructure:"values,omitempty"`
	Type          string                 `protobuf:"bytes,2,opt,name=type,proto3" json:"type,omitempty" toml:"type,omitempty" mapstructure:"type,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *ListFilter) Reset() {
	*x = ListFilter{}
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[0]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *ListFilter) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*ListFilter) ProtoMessage() {}

func (x *ListFilter) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[0]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use ListFilter.ProtoReflect.Descriptor instead.
func (*ListFilter) Descriptor() ([]byte, []int) {
	return file_interservice_report_manager_report_manager_proto_rawDescGZIP(), []int{0}
}

func (x *ListFilter) GetValues() []string {
	if x != nil {
		return x.Values
	}
	return nil
}

func (x *ListFilter) GetType() string {
	if x != nil {
		return x.Type
	}
	return ""
}

type StoreReportRequest struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	Content       []byte                 `protobuf:"bytes,1,opt,name=content,proto3" json:"content,omitempty" toml:"content,omitempty" mapstructure:"content,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *StoreReportRequest) Reset() {
	*x = StoreReportRequest{}
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[1]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *StoreReportRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*StoreReportRequest) ProtoMessage() {}

func (x *StoreReportRequest) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[1]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use StoreReportRequest.ProtoReflect.Descriptor instead.
func (*StoreReportRequest) Descriptor() ([]byte, []int) {
	return file_interservice_report_manager_report_manager_proto_rawDescGZIP(), []int{1}
}

func (x *StoreReportRequest) GetContent() []byte {
	if x != nil {
		return x.Content
	}
	return nil
}

type CustomReportRequest struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	RequestorId   string                 `protobuf:"bytes,1,opt,name=requestor_id,json=requestorId,proto3" json:"requestor_id,omitempty" toml:"requestor_id,omitempty" mapstructure:"requestor_id,omitempty"`
	ReportType    string                 `protobuf:"bytes,2,opt,name=report_type,json=reportType,proto3" json:"report_type,omitempty" toml:"report_type,omitempty" mapstructure:"report_type,omitempty"`
	Filters       []*ListFilter          `protobuf:"bytes,3,rep,name=filters,proto3" json:"filters,omitempty" toml:"filters,omitempty" mapstructure:"filters,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *CustomReportRequest) Reset() {
	*x = CustomReportRequest{}
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[2]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *CustomReportRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*CustomReportRequest) ProtoMessage() {}

func (x *CustomReportRequest) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[2]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use CustomReportRequest.ProtoReflect.Descriptor instead.
func (*CustomReportRequest) Descriptor() ([]byte, []int) {
	return file_interservice_report_manager_report_manager_proto_rawDescGZIP(), []int{2}
}

func (x *CustomReportRequest) GetRequestorId() string {
	if x != nil {
		return x.RequestorId
	}
	return ""
}

func (x *CustomReportRequest) GetReportType() string {
	if x != nil {
		return x.ReportType
	}
	return ""
}

func (x *CustomReportRequest) GetFilters() []*ListFilter {
	if x != nil {
		return x.Filters
	}
	return nil
}

type CustomReportResponse struct {
	state             protoimpl.MessageState `protogen:"open.v1"`
	AcknowledgementId string                 `protobuf:"bytes,1,opt,name=acknowledgement_id,json=acknowledgementId,proto3" json:"acknowledgement_id,omitempty" toml:"acknowledgement_id,omitempty" mapstructure:"acknowledgement_id,omitempty"`
	unknownFields     protoimpl.UnknownFields
	sizeCache         protoimpl.SizeCache
}

func (x *CustomReportResponse) Reset() {
	*x = CustomReportResponse{}
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[3]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *CustomReportResponse) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*CustomReportResponse) ProtoMessage() {}

func (x *CustomReportResponse) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[3]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use CustomReportResponse.ProtoReflect.Descriptor instead.
func (*CustomReportResponse) Descriptor() ([]byte, []int) {
	return file_interservice_report_manager_report_manager_proto_rawDescGZIP(), []int{3}
}

func (x *CustomReportResponse) GetAcknowledgementId() string {
	if x != nil {
		return x.AcknowledgementId
	}
	return ""
}

type AllStatusRequest struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	RequestorId   string                 `protobuf:"bytes,1,opt,name=requestor_id,json=requestorId,proto3" json:"requestor_id,omitempty" toml:"requestor_id,omitempty" mapstructure:"requestor_id,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *AllStatusRequest) Reset() {
	*x = AllStatusRequest{}
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[4]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *AllStatusRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*AllStatusRequest) ProtoMessage() {}

func (x *AllStatusRequest) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[4]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use AllStatusRequest.ProtoReflect.Descriptor instead.
func (*AllStatusRequest) Descriptor() ([]byte, []int) {
	return file_interservice_report_manager_report_manager_proto_rawDescGZIP(), []int{4}
}

func (x *AllStatusRequest) GetRequestorId() string {
	if x != nil {
		return x.RequestorId
	}
	return ""
}

type AllStatusResponse struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	Data          []*StatusResponse      `protobuf:"bytes,1,rep,name=data,proto3" json:"data,omitempty" toml:"data,omitempty" mapstructure:"data,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *AllStatusResponse) Reset() {
	*x = AllStatusResponse{}
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[5]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *AllStatusResponse) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*AllStatusResponse) ProtoMessage() {}

func (x *AllStatusResponse) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[5]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use AllStatusResponse.ProtoReflect.Descriptor instead.
func (*AllStatusResponse) Descriptor() ([]byte, []int) {
	return file_interservice_report_manager_report_manager_proto_rawDescGZIP(), []int{5}
}

func (x *AllStatusResponse) GetData() []*StatusResponse {
	if x != nil {
		return x.Data
	}
	return nil
}

type StatusResponse struct {
	state             protoimpl.MessageState `protogen:"open.v1"`
	AcknowledgementId string                 `protobuf:"bytes,1,opt,name=acknowledgement_id,json=acknowledgementId,proto3" json:"acknowledgement_id,omitempty" toml:"acknowledgement_id,omitempty" mapstructure:"acknowledgement_id,omitempty"`
	Status            string                 `protobuf:"bytes,2,opt,name=status,proto3" json:"status,omitempty" toml:"status,omitempty" mapstructure:"status,omitempty"`
	ReportSize        int64                  `protobuf:"varint,3,opt,name=report_size,json=reportSize,proto3" json:"report_size,omitempty" toml:"report_size,omitempty" mapstructure:"report_size,omitempty"`
	ErrMessage        string                 `protobuf:"bytes,4,opt,name=err_message,json=errMessage,proto3" json:"err_message,omitempty" toml:"err_message,omitempty" mapstructure:"err_message,omitempty"`
	CreatedAt         *timestamppb.Timestamp `protobuf:"bytes,5,opt,name=created_at,json=createdAt,proto3" json:"created_at,omitempty" toml:"created_at,omitempty" mapstructure:"created_at,omitempty"`
	EndedAt           *timestamppb.Timestamp `protobuf:"bytes,6,opt,name=ended_at,json=endedAt,proto3" json:"ended_at,omitempty" toml:"ended_at,omitempty" mapstructure:"ended_at,omitempty"`
	Duration          string                 `protobuf:"bytes,7,opt,name=duration,proto3" json:"duration,omitempty" toml:"duration,omitempty" mapstructure:"duration,omitempty"`
	ReportType        string                 `protobuf:"bytes,8,opt,name=report_type,json=reportType,proto3" json:"report_type,omitempty" toml:"report_type,omitempty" mapstructure:"report_type,omitempty"`
	unknownFields     protoimpl.UnknownFields
	sizeCache         protoimpl.SizeCache
}

func (x *StatusResponse) Reset() {
	*x = StatusResponse{}
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[6]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *StatusResponse) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*StatusResponse) ProtoMessage() {}

func (x *StatusResponse) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[6]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use StatusResponse.ProtoReflect.Descriptor instead.
func (*StatusResponse) Descriptor() ([]byte, []int) {
	return file_interservice_report_manager_report_manager_proto_rawDescGZIP(), []int{6}
}

func (x *StatusResponse) GetAcknowledgementId() string {
	if x != nil {
		return x.AcknowledgementId
	}
	return ""
}

func (x *StatusResponse) GetStatus() string {
	if x != nil {
		return x.Status
	}
	return ""
}

func (x *StatusResponse) GetReportSize() int64 {
	if x != nil {
		return x.ReportSize
	}
	return 0
}

func (x *StatusResponse) GetErrMessage() string {
	if x != nil {
		return x.ErrMessage
	}
	return ""
}

func (x *StatusResponse) GetCreatedAt() *timestamppb.Timestamp {
	if x != nil {
		return x.CreatedAt
	}
	return nil
}

func (x *StatusResponse) GetEndedAt() *timestamppb.Timestamp {
	if x != nil {
		return x.EndedAt
	}
	return nil
}

func (x *StatusResponse) GetDuration() string {
	if x != nil {
		return x.Duration
	}
	return ""
}

func (x *StatusResponse) GetReportType() string {
	if x != nil {
		return x.ReportType
	}
	return ""
}

type GetPresignedURLRequest struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	Id            string                 `protobuf:"bytes,1,opt,name=id,proto3" json:"id,omitempty" toml:"id,omitempty" mapstructure:"id,omitempty"`
	RequestorId   string                 `protobuf:"bytes,2,opt,name=requestor_id,json=requestorId,proto3" json:"requestor_id,omitempty" toml:"requestor_id,omitempty" mapstructure:"requestor_id,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *GetPresignedURLRequest) Reset() {
	*x = GetPresignedURLRequest{}
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[7]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *GetPresignedURLRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*GetPresignedURLRequest) ProtoMessage() {}

func (x *GetPresignedURLRequest) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[7]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use GetPresignedURLRequest.ProtoReflect.Descriptor instead.
func (*GetPresignedURLRequest) Descriptor() ([]byte, []int) {
	return file_interservice_report_manager_report_manager_proto_rawDescGZIP(), []int{7}
}

func (x *GetPresignedURLRequest) GetId() string {
	if x != nil {
		return x.Id
	}
	return ""
}

func (x *GetPresignedURLRequest) GetRequestorId() string {
	if x != nil {
		return x.RequestorId
	}
	return ""
}

type GetPresignedURLResponse struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	Url           string                 `protobuf:"bytes,1,opt,name=url,proto3" json:"url,omitempty" toml:"url,omitempty" mapstructure:"url,omitempty"`
	ReportType    string                 `protobuf:"bytes,2,opt,name=report_type,json=reportType,proto3" json:"report_type,omitempty" toml:"report_type,omitempty" mapstructure:"report_type,omitempty"`
	ReportSize    int64                  `protobuf:"varint,3,opt,name=report_size,json=reportSize,proto3" json:"report_size,omitempty" toml:"report_size,omitempty" mapstructure:"report_size,omitempty"`
	EnabledSsl    bool                   `protobuf:"varint,4,opt,name=enabled_ssl,json=enabledSsl,proto3" json:"enabled_ssl,omitempty" toml:"enabled_ssl,omitempty" mapstructure:"enabled_ssl,omitempty"`
	ClientCert    string                 `protobuf:"bytes,5,opt,name=client_cert,json=clientCert,proto3" json:"client_cert,omitempty" toml:"client_cert,omitempty" mapstructure:"client_cert,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *GetPresignedURLResponse) Reset() {
	*x = GetPresignedURLResponse{}
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[8]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *GetPresignedURLResponse) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*GetPresignedURLResponse) ProtoMessage() {}

func (x *GetPresignedURLResponse) ProtoReflect() protoreflect.Message {
	mi := &file_interservice_report_manager_report_manager_proto_msgTypes[8]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use GetPresignedURLResponse.ProtoReflect.Descriptor instead.
func (*GetPresignedURLResponse) Descriptor() ([]byte, []int) {
	return file_interservice_report_manager_report_manager_proto_rawDescGZIP(), []int{8}
}

func (x *GetPresignedURLResponse) GetUrl() string {
	if x != nil {
		return x.Url
	}
	return ""
}

func (x *GetPresignedURLResponse) GetReportType() string {
	if x != nil {
		return x.ReportType
	}
	return ""
}

func (x *GetPresignedURLResponse) GetReportSize() int64 {
	if x != nil {
		return x.ReportSize
	}
	return 0
}

func (x *GetPresignedURLResponse) GetEnabledSsl() bool {
	if x != nil {
		return x.EnabledSsl
	}
	return false
}

func (x *GetPresignedURLResponse) GetClientCert() string {
	if x != nil {
		return x.ClientCert
	}
	return ""
}

var File_interservice_report_manager_report_manager_proto protoreflect.FileDescriptor

const file_interservice_report_manager_report_manager_proto_rawDesc = "" +
	"\n" +
	"0interservice/report_manager/report_manager.proto\x12+chef.automate.domain.report_manager.service\x1a\x1bgoogle/protobuf/empty.proto\x1a\x1fgoogle/protobuf/timestamp.proto\"8\n" +
	"\n" +
	"ListFilter\x12\x16\n" +
	"\x06values\x18\x01 \x03(\tR\x06values\x12\x12\n" +
	"\x04type\x18\x02 \x01(\tR\x04type\".\n" +
	"\x12StoreReportRequest\x12\x18\n" +
	"\acontent\x18\x01 \x01(\fR\acontent\"\xac\x01\n" +
	"\x13CustomReportRequest\x12!\n" +
	"\frequestor_id\x18\x01 \x01(\tR\vrequestorId\x12\x1f\n" +
	"\vreport_type\x18\x02 \x01(\tR\n" +
	"reportType\x12Q\n" +
	"\afilters\x18\x03 \x03(\v27.chef.automate.domain.report_manager.service.ListFilterR\afilters\"E\n" +
	"\x14CustomReportResponse\x12-\n" +
	"\x12acknowledgement_id\x18\x01 \x01(\tR\x11acknowledgementId\"5\n" +
	"\x10AllStatusRequest\x12!\n" +
	"\frequestor_id\x18\x01 \x01(\tR\vrequestorId\"d\n" +
	"\x11AllStatusResponse\x12O\n" +
	"\x04data\x18\x01 \x03(\v2;.chef.automate.domain.report_manager.service.StatusResponseR\x04data\"\xc8\x02\n" +
	"\x0eStatusResponse\x12-\n" +
	"\x12acknowledgement_id\x18\x01 \x01(\tR\x11acknowledgementId\x12\x16\n" +
	"\x06status\x18\x02 \x01(\tR\x06status\x12\x1f\n" +
	"\vreport_size\x18\x03 \x01(\x03R\n" +
	"reportSize\x12\x1f\n" +
	"\verr_message\x18\x04 \x01(\tR\n" +
	"errMessage\x129\n" +
	"\n" +
	"created_at\x18\x05 \x01(\v2\x1a.google.protobuf.TimestampR\tcreatedAt\x125\n" +
	"\bended_at\x18\x06 \x01(\v2\x1a.google.protobuf.TimestampR\aendedAt\x12\x1a\n" +
	"\bduration\x18\a \x01(\tR\bduration\x12\x1f\n" +
	"\vreport_type\x18\b \x01(\tR\n" +
	"reportType\"K\n" +
	"\x16GetPresignedURLRequest\x12\x0e\n" +
	"\x02id\x18\x01 \x01(\tR\x02id\x12!\n" +
	"\frequestor_id\x18\x02 \x01(\tR\vrequestorId\"\xaf\x01\n" +
	"\x17GetPresignedURLResponse\x12\x10\n" +
	"\x03url\x18\x01 \x01(\tR\x03url\x12\x1f\n" +
	"\vreport_type\x18\x02 \x01(\tR\n" +
	"reportType\x12\x1f\n" +
	"\vreport_size\x18\x03 \x01(\x03R\n" +
	"reportSize\x12\x1f\n" +
	"\venabled_ssl\x18\x04 \x01(\bR\n" +
	"enabledSsl\x12\x1f\n" +
	"\vclient_cert\x18\x05 \x01(\tR\n" +
	"clientCert2\xdc\x04\n" +
	"\x14ReportManagerService\x12j\n" +
	"\vStoreReport\x12?.chef.automate.domain.report_manager.service.StoreReportRequest\x1a\x16.google.protobuf.Empty\"\x00(\x01\x12\x9c\x01\n" +
	"\x13PrepareCustomReport\x12@.chef.automate.domain.report_manager.service.CustomReportRequest\x1aA.chef.automate.domain.report_manager.service.CustomReportResponse\"\x00\x12\x97\x01\n" +
	"\x14GetAllRequestsStatus\x12=.chef.automate.domain.report_manager.service.AllStatusRequest\x1a>.chef.automate.domain.report_manager.service.AllStatusResponse\"\x00\x12\x9e\x01\n" +
	"\x0fGetPresignedURL\x12C.chef.automate.domain.report_manager.service.GetPresignedURLRequest\x1aD.chef.automate.domain.report_manager.service.GetPresignedURLResponse\"\x00B:Z8github.com/chef/automate/api/interservice/report_managerb\x06proto3"

var (
	file_interservice_report_manager_report_manager_proto_rawDescOnce sync.Once
	file_interservice_report_manager_report_manager_proto_rawDescData []byte
)

func file_interservice_report_manager_report_manager_proto_rawDescGZIP() []byte {
	file_interservice_report_manager_report_manager_proto_rawDescOnce.Do(func() {
		file_interservice_report_manager_report_manager_proto_rawDescData = protoimpl.X.CompressGZIP(unsafe.Slice(unsafe.StringData(file_interservice_report_manager_report_manager_proto_rawDesc), len(file_interservice_report_manager_report_manager_proto_rawDesc)))
	})
	return file_interservice_report_manager_report_manager_proto_rawDescData
}

var file_interservice_report_manager_report_manager_proto_msgTypes = make([]protoimpl.MessageInfo, 9)
var file_interservice_report_manager_report_manager_proto_goTypes = []any{
	(*ListFilter)(nil),              // 0: chef.automate.domain.report_manager.service.ListFilter
	(*StoreReportRequest)(nil),      // 1: chef.automate.domain.report_manager.service.StoreReportRequest
	(*CustomReportRequest)(nil),     // 2: chef.automate.domain.report_manager.service.CustomReportRequest
	(*CustomReportResponse)(nil),    // 3: chef.automate.domain.report_manager.service.CustomReportResponse
	(*AllStatusRequest)(nil),        // 4: chef.automate.domain.report_manager.service.AllStatusRequest
	(*AllStatusResponse)(nil),       // 5: chef.automate.domain.report_manager.service.AllStatusResponse
	(*StatusResponse)(nil),          // 6: chef.automate.domain.report_manager.service.StatusResponse
	(*GetPresignedURLRequest)(nil),  // 7: chef.automate.domain.report_manager.service.GetPresignedURLRequest
	(*GetPresignedURLResponse)(nil), // 8: chef.automate.domain.report_manager.service.GetPresignedURLResponse
	(*timestamppb.Timestamp)(nil),   // 9: google.protobuf.Timestamp
	(*emptypb.Empty)(nil),           // 10: google.protobuf.Empty
}
var file_interservice_report_manager_report_manager_proto_depIdxs = []int32{
	0,  // 0: chef.automate.domain.report_manager.service.CustomReportRequest.filters:type_name -> chef.automate.domain.report_manager.service.ListFilter
	6,  // 1: chef.automate.domain.report_manager.service.AllStatusResponse.data:type_name -> chef.automate.domain.report_manager.service.StatusResponse
	9,  // 2: chef.automate.domain.report_manager.service.StatusResponse.created_at:type_name -> google.protobuf.Timestamp
	9,  // 3: chef.automate.domain.report_manager.service.StatusResponse.ended_at:type_name -> google.protobuf.Timestamp
	1,  // 4: chef.automate.domain.report_manager.service.ReportManagerService.StoreReport:input_type -> chef.automate.domain.report_manager.service.StoreReportRequest
	2,  // 5: chef.automate.domain.report_manager.service.ReportManagerService.PrepareCustomReport:input_type -> chef.automate.domain.report_manager.service.CustomReportRequest
	4,  // 6: chef.automate.domain.report_manager.service.ReportManagerService.GetAllRequestsStatus:input_type -> chef.automate.domain.report_manager.service.AllStatusRequest
	7,  // 7: chef.automate.domain.report_manager.service.ReportManagerService.GetPresignedURL:input_type -> chef.automate.domain.report_manager.service.GetPresignedURLRequest
	10, // 8: chef.automate.domain.report_manager.service.ReportManagerService.StoreReport:output_type -> google.protobuf.Empty
	3,  // 9: chef.automate.domain.report_manager.service.ReportManagerService.PrepareCustomReport:output_type -> chef.automate.domain.report_manager.service.CustomReportResponse
	5,  // 10: chef.automate.domain.report_manager.service.ReportManagerService.GetAllRequestsStatus:output_type -> chef.automate.domain.report_manager.service.AllStatusResponse
	8,  // 11: chef.automate.domain.report_manager.service.ReportManagerService.GetPresignedURL:output_type -> chef.automate.domain.report_manager.service.GetPresignedURLResponse
	8,  // [8:12] is the sub-list for method output_type
	4,  // [4:8] is the sub-list for method input_type
	4,  // [4:4] is the sub-list for extension type_name
	4,  // [4:4] is the sub-list for extension extendee
	0,  // [0:4] is the sub-list for field type_name
}

func init() { file_interservice_report_manager_report_manager_proto_init() }
func file_interservice_report_manager_report_manager_proto_init() {
	if File_interservice_report_manager_report_manager_proto != nil {
		return
	}
	type x struct{}
	out := protoimpl.TypeBuilder{
		File: protoimpl.DescBuilder{
			GoPackagePath: reflect.TypeOf(x{}).PkgPath(),
			RawDescriptor: unsafe.Slice(unsafe.StringData(file_interservice_report_manager_report_manager_proto_rawDesc), len(file_interservice_report_manager_report_manager_proto_rawDesc)),
			NumEnums:      0,
			NumMessages:   9,
			NumExtensions: 0,
			NumServices:   1,
		},
		GoTypes:           file_interservice_report_manager_report_manager_proto_goTypes,
		DependencyIndexes: file_interservice_report_manager_report_manager_proto_depIdxs,
		MessageInfos:      file_interservice_report_manager_report_manager_proto_msgTypes,
	}.Build()
	File_interservice_report_manager_report_manager_proto = out.File
	file_interservice_report_manager_report_manager_proto_goTypes = nil
	file_interservice_report_manager_report_manager_proto_depIdxs = nil
}

// Reference imports to suppress errors if they are not otherwise used.
var _ context.Context
var _ grpc.ClientConnInterface

// This is a compile-time assertion to ensure that this generated file
// is compatible with the grpc package it is being compiled against.
const _ = grpc.SupportPackageIsVersion6

// ReportManagerServiceClient is the client API for ReportManagerService service.
//
// For semantics around ctx use and closing/ending streaming RPCs, please refer to https://godoc.org/google.golang.org/grpc#ClientConn.NewStream.
type ReportManagerServiceClient interface {
	StoreReport(ctx context.Context, opts ...grpc.CallOption) (ReportManagerService_StoreReportClient, error)
	PrepareCustomReport(ctx context.Context, in *CustomReportRequest, opts ...grpc.CallOption) (*CustomReportResponse, error)
	GetAllRequestsStatus(ctx context.Context, in *AllStatusRequest, opts ...grpc.CallOption) (*AllStatusResponse, error)
	GetPresignedURL(ctx context.Context, in *GetPresignedURLRequest, opts ...grpc.CallOption) (*GetPresignedURLResponse, error)
}

type reportManagerServiceClient struct {
	cc grpc.ClientConnInterface
}

func NewReportManagerServiceClient(cc grpc.ClientConnInterface) ReportManagerServiceClient {
	return &reportManagerServiceClient{cc}
}

func (c *reportManagerServiceClient) StoreReport(ctx context.Context, opts ...grpc.CallOption) (ReportManagerService_StoreReportClient, error) {
	stream, err := c.cc.NewStream(ctx, &_ReportManagerService_serviceDesc.Streams[0], "/chef.automate.domain.report_manager.service.ReportManagerService/StoreReport", opts...)
	if err != nil {
		return nil, err
	}
	x := &reportManagerServiceStoreReportClient{stream}
	return x, nil
}

type ReportManagerService_StoreReportClient interface {
	Send(*StoreReportRequest) error
	CloseAndRecv() (*emptypb.Empty, error)
	grpc.ClientStream
}

type reportManagerServiceStoreReportClient struct {
	grpc.ClientStream
}

func (x *reportManagerServiceStoreReportClient) Send(m *StoreReportRequest) error {
	return x.ClientStream.SendMsg(m)
}

func (x *reportManagerServiceStoreReportClient) CloseAndRecv() (*emptypb.Empty, error) {
	if err := x.ClientStream.CloseSend(); err != nil {
		return nil, err
	}
	m := new(emptypb.Empty)
	if err := x.ClientStream.RecvMsg(m); err != nil {
		return nil, err
	}
	return m, nil
}

func (c *reportManagerServiceClient) PrepareCustomReport(ctx context.Context, in *CustomReportRequest, opts ...grpc.CallOption) (*CustomReportResponse, error) {
	out := new(CustomReportResponse)
	err := c.cc.Invoke(ctx, "/chef.automate.domain.report_manager.service.ReportManagerService/PrepareCustomReport", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *reportManagerServiceClient) GetAllRequestsStatus(ctx context.Context, in *AllStatusRequest, opts ...grpc.CallOption) (*AllStatusResponse, error) {
	out := new(AllStatusResponse)
	err := c.cc.Invoke(ctx, "/chef.automate.domain.report_manager.service.ReportManagerService/GetAllRequestsStatus", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

func (c *reportManagerServiceClient) GetPresignedURL(ctx context.Context, in *GetPresignedURLRequest, opts ...grpc.CallOption) (*GetPresignedURLResponse, error) {
	out := new(GetPresignedURLResponse)
	err := c.cc.Invoke(ctx, "/chef.automate.domain.report_manager.service.ReportManagerService/GetPresignedURL", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// ReportManagerServiceServer is the server API for ReportManagerService service.
type ReportManagerServiceServer interface {
	StoreReport(ReportManagerService_StoreReportServer) error
	PrepareCustomReport(context.Context, *CustomReportRequest) (*CustomReportResponse, error)
	GetAllRequestsStatus(context.Context, *AllStatusRequest) (*AllStatusResponse, error)
	GetPresignedURL(context.Context, *GetPresignedURLRequest) (*GetPresignedURLResponse, error)
}

// UnimplementedReportManagerServiceServer can be embedded to have forward compatible implementations.
type UnimplementedReportManagerServiceServer struct {
}

func (*UnimplementedReportManagerServiceServer) StoreReport(ReportManagerService_StoreReportServer) error {
	return status.Errorf(codes.Unimplemented, "method StoreReport not implemented")
}
func (*UnimplementedReportManagerServiceServer) PrepareCustomReport(context.Context, *CustomReportRequest) (*CustomReportResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method PrepareCustomReport not implemented")
}
func (*UnimplementedReportManagerServiceServer) GetAllRequestsStatus(context.Context, *AllStatusRequest) (*AllStatusResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method GetAllRequestsStatus not implemented")
}
func (*UnimplementedReportManagerServiceServer) GetPresignedURL(context.Context, *GetPresignedURLRequest) (*GetPresignedURLResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method GetPresignedURL not implemented")
}

func RegisterReportManagerServiceServer(s *grpc.Server, srv ReportManagerServiceServer) {
	s.RegisterService(&_ReportManagerService_serviceDesc, srv)
}

func _ReportManagerService_StoreReport_Handler(srv interface{}, stream grpc.ServerStream) error {
	return srv.(ReportManagerServiceServer).StoreReport(&reportManagerServiceStoreReportServer{stream})
}

type ReportManagerService_StoreReportServer interface {
	SendAndClose(*emptypb.Empty) error
	Recv() (*StoreReportRequest, error)
	grpc.ServerStream
}

type reportManagerServiceStoreReportServer struct {
	grpc.ServerStream
}

func (x *reportManagerServiceStoreReportServer) SendAndClose(m *emptypb.Empty) error {
	return x.ServerStream.SendMsg(m)
}

func (x *reportManagerServiceStoreReportServer) Recv() (*StoreReportRequest, error) {
	m := new(StoreReportRequest)
	if err := x.ServerStream.RecvMsg(m); err != nil {
		return nil, err
	}
	return m, nil
}

func _ReportManagerService_PrepareCustomReport_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(CustomReportRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(ReportManagerServiceServer).PrepareCustomReport(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: "/chef.automate.domain.report_manager.service.ReportManagerService/PrepareCustomReport",
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(ReportManagerServiceServer).PrepareCustomReport(ctx, req.(*CustomReportRequest))
	}
	return interceptor(ctx, in, info, handler)
}

func _ReportManagerService_GetAllRequestsStatus_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(AllStatusRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(ReportManagerServiceServer).GetAllRequestsStatus(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: "/chef.automate.domain.report_manager.service.ReportManagerService/GetAllRequestsStatus",
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(ReportManagerServiceServer).GetAllRequestsStatus(ctx, req.(*AllStatusRequest))
	}
	return interceptor(ctx, in, info, handler)
}

func _ReportManagerService_GetPresignedURL_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(GetPresignedURLRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(ReportManagerServiceServer).GetPresignedURL(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: "/chef.automate.domain.report_manager.service.ReportManagerService/GetPresignedURL",
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(ReportManagerServiceServer).GetPresignedURL(ctx, req.(*GetPresignedURLRequest))
	}
	return interceptor(ctx, in, info, handler)
}

var _ReportManagerService_serviceDesc = grpc.ServiceDesc{
	ServiceName: "chef.automate.domain.report_manager.service.ReportManagerService",
	HandlerType: (*ReportManagerServiceServer)(nil),
	Methods: []grpc.MethodDesc{
		{
			MethodName: "PrepareCustomReport",
			Handler:    _ReportManagerService_PrepareCustomReport_Handler,
		},
		{
			MethodName: "GetAllRequestsStatus",
			Handler:    _ReportManagerService_GetAllRequestsStatus_Handler,
		},
		{
			MethodName: "GetPresignedURL",
			Handler:    _ReportManagerService_GetPresignedURL_Handler,
		},
	},
	Streams: []grpc.StreamDesc{
		{
			StreamName:    "StoreReport",
			Handler:       _ReportManagerService_StoreReport_Handler,
			ClientStreams: true,
		},
	},
	Metadata: "interservice/report_manager/report_manager.proto",
}
