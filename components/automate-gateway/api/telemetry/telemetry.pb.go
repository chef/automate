// Code generated by protoc-gen-go. DO NOT EDIT.
// versions:
// 	protoc-gen-go v1.36.6
// 	protoc        v3.19.0
// source: automate-gateway/api/telemetry/telemetry.proto

package telemetry

import (
	context "context"
	_ "github.com/chef/automate/api/external/annotations/iam"
	_ "google.golang.org/genproto/googleapis/api/annotations"
	grpc "google.golang.org/grpc"
	codes "google.golang.org/grpc/codes"
	status "google.golang.org/grpc/status"
	protoreflect "google.golang.org/protobuf/reflect/protoreflect"
	protoimpl "google.golang.org/protobuf/runtime/protoimpl"
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

type TelemetryRequest struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *TelemetryRequest) Reset() {
	*x = TelemetryRequest{}
	mi := &file_automate_gateway_api_telemetry_telemetry_proto_msgTypes[0]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *TelemetryRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*TelemetryRequest) ProtoMessage() {}

func (x *TelemetryRequest) ProtoReflect() protoreflect.Message {
	mi := &file_automate_gateway_api_telemetry_telemetry_proto_msgTypes[0]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use TelemetryRequest.ProtoReflect.Descriptor instead.
func (*TelemetryRequest) Descriptor() ([]byte, []int) {
	return file_automate_gateway_api_telemetry_telemetry_proto_rawDescGZIP(), []int{0}
}

type TelemetryResponse struct {
	state            protoimpl.MessageState `protogen:"open.v1"`
	LicenseId        string                 `protobuf:"bytes,1,opt,name=license_id,json=licenseId,proto3" json:"license_id,omitempty"`
	CustomerName     string                 `protobuf:"bytes,4,opt,name=customer_name,json=customerName,proto3" json:"customer_name,omitempty"`
	CustomerId       string                 `protobuf:"bytes,5,opt,name=customer_id,json=customerId,proto3" json:"customer_id,omitempty"`
	LicenseType      string                 `protobuf:"bytes,6,opt,name=license_type,json=licenseType,proto3" json:"license_type,omitempty"`
	TelemetryEnabled bool                   `protobuf:"varint,7,opt,name=telemetry_enabled,json=telemetryEnabled,proto3" json:"telemetry_enabled,omitempty"`
	TelemetryUrl     string                 `protobuf:"bytes,8,opt,name=telemetry_url,json=telemetryUrl,proto3" json:"telemetry_url,omitempty"`
	MaxNodes         int64                  `protobuf:"varint,9,opt,name=max_nodes,json=maxNodes,proto3" json:"max_nodes,omitempty"`
	DeploymentId     string                 `protobuf:"bytes,10,opt,name=deployment_id,json=deploymentId,proto3" json:"deployment_id,omitempty"`
	DeploymentType   string                 `protobuf:"bytes,11,opt,name=deployment_type,json=deploymentType,proto3" json:"deployment_type,omitempty"`
	unknownFields    protoimpl.UnknownFields
	sizeCache        protoimpl.SizeCache
}

func (x *TelemetryResponse) Reset() {
	*x = TelemetryResponse{}
	mi := &file_automate_gateway_api_telemetry_telemetry_proto_msgTypes[1]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *TelemetryResponse) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*TelemetryResponse) ProtoMessage() {}

func (x *TelemetryResponse) ProtoReflect() protoreflect.Message {
	mi := &file_automate_gateway_api_telemetry_telemetry_proto_msgTypes[1]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use TelemetryResponse.ProtoReflect.Descriptor instead.
func (*TelemetryResponse) Descriptor() ([]byte, []int) {
	return file_automate_gateway_api_telemetry_telemetry_proto_rawDescGZIP(), []int{1}
}

func (x *TelemetryResponse) GetLicenseId() string {
	if x != nil {
		return x.LicenseId
	}
	return ""
}

func (x *TelemetryResponse) GetCustomerName() string {
	if x != nil {
		return x.CustomerName
	}
	return ""
}

func (x *TelemetryResponse) GetCustomerId() string {
	if x != nil {
		return x.CustomerId
	}
	return ""
}

func (x *TelemetryResponse) GetLicenseType() string {
	if x != nil {
		return x.LicenseType
	}
	return ""
}

func (x *TelemetryResponse) GetTelemetryEnabled() bool {
	if x != nil {
		return x.TelemetryEnabled
	}
	return false
}

func (x *TelemetryResponse) GetTelemetryUrl() string {
	if x != nil {
		return x.TelemetryUrl
	}
	return ""
}

func (x *TelemetryResponse) GetMaxNodes() int64 {
	if x != nil {
		return x.MaxNodes
	}
	return 0
}

func (x *TelemetryResponse) GetDeploymentId() string {
	if x != nil {
		return x.DeploymentId
	}
	return ""
}

func (x *TelemetryResponse) GetDeploymentType() string {
	if x != nil {
		return x.DeploymentType
	}
	return ""
}

var File_automate_gateway_api_telemetry_telemetry_proto protoreflect.FileDescriptor

const file_automate_gateway_api_telemetry_telemetry_proto_rawDesc = "" +
	"\n" +
	".automate-gateway/api/telemetry/telemetry.proto\x12\x1bchef.automate.api.telemetry\x1a\x1cgoogle/api/annotations.proto\x1a*external/annotations/iam/annotations.proto\"\x12\n" +
	"\x10TelemetryRequest\"\xd8\x02\n" +
	"\x11TelemetryResponse\x12\x1d\n" +
	"\n" +
	"license_id\x18\x01 \x01(\tR\tlicenseId\x12#\n" +
	"\rcustomer_name\x18\x04 \x01(\tR\fcustomerName\x12\x1f\n" +
	"\vcustomer_id\x18\x05 \x01(\tR\n" +
	"customerId\x12!\n" +
	"\flicense_type\x18\x06 \x01(\tR\vlicenseType\x12+\n" +
	"\x11telemetry_enabled\x18\a \x01(\bR\x10telemetryEnabled\x12#\n" +
	"\rtelemetry_url\x18\b \x01(\tR\ftelemetryUrl\x12\x1b\n" +
	"\tmax_nodes\x18\t \x01(\x03R\bmaxNodes\x12#\n" +
	"\rdeployment_id\x18\n" +
	" \x01(\tR\fdeploymentId\x12'\n" +
	"\x0fdeployment_type\x18\v \x01(\tR\x0edeploymentType2\xd9\x01\n" +
	"\tTelemetry\x12\xcb\x01\n" +
	"\x19GetTelemetryConfiguration\x12-.chef.automate.api.telemetry.TelemetryRequest\x1a..chef.automate.api.telemetry.TelemetryResponse\"O\x8a\xb5\x18+\n" +
	"\rsystem:config\x12\x1asystem:telemetryConfig:get\x82\xd3\xe4\x93\x02\x1a\x12\x18/api/v0/telemetry/configBDZBgithub.com/chef/automate/components/automate-gateway/api/telemetryb\x06proto3"

var (
	file_automate_gateway_api_telemetry_telemetry_proto_rawDescOnce sync.Once
	file_automate_gateway_api_telemetry_telemetry_proto_rawDescData []byte
)

func file_automate_gateway_api_telemetry_telemetry_proto_rawDescGZIP() []byte {
	file_automate_gateway_api_telemetry_telemetry_proto_rawDescOnce.Do(func() {
		file_automate_gateway_api_telemetry_telemetry_proto_rawDescData = protoimpl.X.CompressGZIP(unsafe.Slice(unsafe.StringData(file_automate_gateway_api_telemetry_telemetry_proto_rawDesc), len(file_automate_gateway_api_telemetry_telemetry_proto_rawDesc)))
	})
	return file_automate_gateway_api_telemetry_telemetry_proto_rawDescData
}

var file_automate_gateway_api_telemetry_telemetry_proto_msgTypes = make([]protoimpl.MessageInfo, 2)
var file_automate_gateway_api_telemetry_telemetry_proto_goTypes = []any{
	(*TelemetryRequest)(nil),  // 0: chef.automate.api.telemetry.TelemetryRequest
	(*TelemetryResponse)(nil), // 1: chef.automate.api.telemetry.TelemetryResponse
}
var file_automate_gateway_api_telemetry_telemetry_proto_depIdxs = []int32{
	0, // 0: chef.automate.api.telemetry.Telemetry.GetTelemetryConfiguration:input_type -> chef.automate.api.telemetry.TelemetryRequest
	1, // 1: chef.automate.api.telemetry.Telemetry.GetTelemetryConfiguration:output_type -> chef.automate.api.telemetry.TelemetryResponse
	1, // [1:2] is the sub-list for method output_type
	0, // [0:1] is the sub-list for method input_type
	0, // [0:0] is the sub-list for extension type_name
	0, // [0:0] is the sub-list for extension extendee
	0, // [0:0] is the sub-list for field type_name
}

func init() { file_automate_gateway_api_telemetry_telemetry_proto_init() }
func file_automate_gateway_api_telemetry_telemetry_proto_init() {
	if File_automate_gateway_api_telemetry_telemetry_proto != nil {
		return
	}
	type x struct{}
	out := protoimpl.TypeBuilder{
		File: protoimpl.DescBuilder{
			GoPackagePath: reflect.TypeOf(x{}).PkgPath(),
			RawDescriptor: unsafe.Slice(unsafe.StringData(file_automate_gateway_api_telemetry_telemetry_proto_rawDesc), len(file_automate_gateway_api_telemetry_telemetry_proto_rawDesc)),
			NumEnums:      0,
			NumMessages:   2,
			NumExtensions: 0,
			NumServices:   1,
		},
		GoTypes:           file_automate_gateway_api_telemetry_telemetry_proto_goTypes,
		DependencyIndexes: file_automate_gateway_api_telemetry_telemetry_proto_depIdxs,
		MessageInfos:      file_automate_gateway_api_telemetry_telemetry_proto_msgTypes,
	}.Build()
	File_automate_gateway_api_telemetry_telemetry_proto = out.File
	file_automate_gateway_api_telemetry_telemetry_proto_goTypes = nil
	file_automate_gateway_api_telemetry_telemetry_proto_depIdxs = nil
}

// Reference imports to suppress errors if they are not otherwise used.
var _ context.Context
var _ grpc.ClientConnInterface

// This is a compile-time assertion to ensure that this generated file
// is compatible with the grpc package it is being compiled against.
const _ = grpc.SupportPackageIsVersion6

// TelemetryClient is the client API for Telemetry service.
//
// For semantics around ctx use and closing/ending streaming RPCs, please refer to https://godoc.org/google.golang.org/grpc#ClientConn.NewStream.
type TelemetryClient interface {
	GetTelemetryConfiguration(ctx context.Context, in *TelemetryRequest, opts ...grpc.CallOption) (*TelemetryResponse, error)
}

type telemetryClient struct {
	cc grpc.ClientConnInterface
}

func NewTelemetryClient(cc grpc.ClientConnInterface) TelemetryClient {
	return &telemetryClient{cc}
}

func (c *telemetryClient) GetTelemetryConfiguration(ctx context.Context, in *TelemetryRequest, opts ...grpc.CallOption) (*TelemetryResponse, error) {
	out := new(TelemetryResponse)
	err := c.cc.Invoke(ctx, "/chef.automate.api.telemetry.Telemetry/GetTelemetryConfiguration", in, out, opts...)
	if err != nil {
		return nil, err
	}
	return out, nil
}

// TelemetryServer is the server API for Telemetry service.
type TelemetryServer interface {
	GetTelemetryConfiguration(context.Context, *TelemetryRequest) (*TelemetryResponse, error)
}

// UnimplementedTelemetryServer can be embedded to have forward compatible implementations.
type UnimplementedTelemetryServer struct {
}

func (*UnimplementedTelemetryServer) GetTelemetryConfiguration(context.Context, *TelemetryRequest) (*TelemetryResponse, error) {
	return nil, status.Errorf(codes.Unimplemented, "method GetTelemetryConfiguration not implemented")
}

func RegisterTelemetryServer(s *grpc.Server, srv TelemetryServer) {
	s.RegisterService(&_Telemetry_serviceDesc, srv)
}

func _Telemetry_GetTelemetryConfiguration_Handler(srv interface{}, ctx context.Context, dec func(interface{}) error, interceptor grpc.UnaryServerInterceptor) (interface{}, error) {
	in := new(TelemetryRequest)
	if err := dec(in); err != nil {
		return nil, err
	}
	if interceptor == nil {
		return srv.(TelemetryServer).GetTelemetryConfiguration(ctx, in)
	}
	info := &grpc.UnaryServerInfo{
		Server:     srv,
		FullMethod: "/chef.automate.api.telemetry.Telemetry/GetTelemetryConfiguration",
	}
	handler := func(ctx context.Context, req interface{}) (interface{}, error) {
		return srv.(TelemetryServer).GetTelemetryConfiguration(ctx, req.(*TelemetryRequest))
	}
	return interceptor(ctx, in, info, handler)
}

var _Telemetry_serviceDesc = grpc.ServiceDesc{
	ServiceName: "chef.automate.api.telemetry.Telemetry",
	HandlerType: (*TelemetryServer)(nil),
	Methods: []grpc.MethodDesc{
		{
			MethodName: "GetTelemetryConfiguration",
			Handler:    _Telemetry_GetTelemetryConfiguration_Handler,
		},
	},
	Streams:  []grpc.StreamDesc{},
	Metadata: "automate-gateway/api/telemetry/telemetry.proto",
}
