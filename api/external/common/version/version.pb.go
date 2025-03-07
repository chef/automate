// Code generated by protoc-gen-go. DO NOT EDIT.
// versions:
// 	protoc-gen-go v1.36.0
// 	protoc        v3.19.0
// source: external/common/version/version.proto

package version

import (
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

type VersionInfo struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	Name          string                 `protobuf:"bytes,1,opt,name=name,proto3" json:"name,omitempty"`
	Version       string                 `protobuf:"bytes,2,opt,name=version,proto3" json:"version,omitempty"`
	Sha           string                 `protobuf:"bytes,3,opt,name=sha,proto3" json:"sha,omitempty"`
	Built         string                 `protobuf:"bytes,4,opt,name=built,proto3" json:"built,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *VersionInfo) Reset() {
	*x = VersionInfo{}
	mi := &file_external_common_version_version_proto_msgTypes[0]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *VersionInfo) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*VersionInfo) ProtoMessage() {}

func (x *VersionInfo) ProtoReflect() protoreflect.Message {
	mi := &file_external_common_version_version_proto_msgTypes[0]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use VersionInfo.ProtoReflect.Descriptor instead.
func (*VersionInfo) Descriptor() ([]byte, []int) {
	return file_external_common_version_version_proto_rawDescGZIP(), []int{0}
}

func (x *VersionInfo) GetName() string {
	if x != nil {
		return x.Name
	}
	return ""
}

func (x *VersionInfo) GetVersion() string {
	if x != nil {
		return x.Version
	}
	return ""
}

func (x *VersionInfo) GetSha() string {
	if x != nil {
		return x.Sha
	}
	return ""
}

func (x *VersionInfo) GetBuilt() string {
	if x != nil {
		return x.Built
	}
	return ""
}

type VersionInfoRequest struct {
	state         protoimpl.MessageState `protogen:"open.v1"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *VersionInfoRequest) Reset() {
	*x = VersionInfoRequest{}
	mi := &file_external_common_version_version_proto_msgTypes[1]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *VersionInfoRequest) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*VersionInfoRequest) ProtoMessage() {}

func (x *VersionInfoRequest) ProtoReflect() protoreflect.Message {
	mi := &file_external_common_version_version_proto_msgTypes[1]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use VersionInfoRequest.ProtoReflect.Descriptor instead.
func (*VersionInfoRequest) Descriptor() ([]byte, []int) {
	return file_external_common_version_version_proto_rawDescGZIP(), []int{1}
}

var File_external_common_version_version_proto protoreflect.FileDescriptor

var file_external_common_version_version_proto_rawDesc = []byte{
	0x0a, 0x25, 0x65, 0x78, 0x74, 0x65, 0x72, 0x6e, 0x61, 0x6c, 0x2f, 0x63, 0x6f, 0x6d, 0x6d, 0x6f,
	0x6e, 0x2f, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x2f, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f,
	0x6e, 0x2e, 0x70, 0x72, 0x6f, 0x74, 0x6f, 0x12, 0x20, 0x63, 0x68, 0x65, 0x66, 0x2e, 0x61, 0x75,
	0x74, 0x6f, 0x6d, 0x61, 0x74, 0x65, 0x2e, 0x61, 0x70, 0x69, 0x2e, 0x63, 0x6f, 0x6d, 0x6d, 0x6f,
	0x6e, 0x2e, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x22, 0x63, 0x0a, 0x0b, 0x56, 0x65, 0x72,
	0x73, 0x69, 0x6f, 0x6e, 0x49, 0x6e, 0x66, 0x6f, 0x12, 0x12, 0x0a, 0x04, 0x6e, 0x61, 0x6d, 0x65,
	0x18, 0x01, 0x20, 0x01, 0x28, 0x09, 0x52, 0x04, 0x6e, 0x61, 0x6d, 0x65, 0x12, 0x18, 0x0a, 0x07,
	0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x18, 0x02, 0x20, 0x01, 0x28, 0x09, 0x52, 0x07, 0x76,
	0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x12, 0x10, 0x0a, 0x03, 0x73, 0x68, 0x61, 0x18, 0x03, 0x20,
	0x01, 0x28, 0x09, 0x52, 0x03, 0x73, 0x68, 0x61, 0x12, 0x14, 0x0a, 0x05, 0x62, 0x75, 0x69, 0x6c,
	0x74, 0x18, 0x04, 0x20, 0x01, 0x28, 0x09, 0x52, 0x05, 0x62, 0x75, 0x69, 0x6c, 0x74, 0x22, 0x14,
	0x0a, 0x12, 0x56, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x49, 0x6e, 0x66, 0x6f, 0x52, 0x65, 0x71,
	0x75, 0x65, 0x73, 0x74, 0x42, 0x36, 0x5a, 0x34, 0x67, 0x69, 0x74, 0x68, 0x75, 0x62, 0x2e, 0x63,
	0x6f, 0x6d, 0x2f, 0x63, 0x68, 0x65, 0x66, 0x2f, 0x61, 0x75, 0x74, 0x6f, 0x6d, 0x61, 0x74, 0x65,
	0x2f, 0x61, 0x70, 0x69, 0x2f, 0x65, 0x78, 0x74, 0x65, 0x72, 0x6e, 0x61, 0x6c, 0x2f, 0x63, 0x6f,
	0x6d, 0x6d, 0x6f, 0x6e, 0x2f, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6f, 0x6e, 0x62, 0x06, 0x70, 0x72,
	0x6f, 0x74, 0x6f, 0x33,
}

var (
	file_external_common_version_version_proto_rawDescOnce sync.Once
	file_external_common_version_version_proto_rawDescData = file_external_common_version_version_proto_rawDesc
)

func file_external_common_version_version_proto_rawDescGZIP() []byte {
	file_external_common_version_version_proto_rawDescOnce.Do(func() {
		file_external_common_version_version_proto_rawDescData = protoimpl.X.CompressGZIP(file_external_common_version_version_proto_rawDescData)
	})
	return file_external_common_version_version_proto_rawDescData
}

var file_external_common_version_version_proto_msgTypes = make([]protoimpl.MessageInfo, 2)
var file_external_common_version_version_proto_goTypes = []any{
	(*VersionInfo)(nil),        // 0: chef.automate.api.common.version.VersionInfo
	(*VersionInfoRequest)(nil), // 1: chef.automate.api.common.version.VersionInfoRequest
}
var file_external_common_version_version_proto_depIdxs = []int32{
	0, // [0:0] is the sub-list for method output_type
	0, // [0:0] is the sub-list for method input_type
	0, // [0:0] is the sub-list for extension type_name
	0, // [0:0] is the sub-list for extension extendee
	0, // [0:0] is the sub-list for field type_name
}

func init() { file_external_common_version_version_proto_init() }
func file_external_common_version_version_proto_init() {
	if File_external_common_version_version_proto != nil {
		return
	}
	type x struct{}
	out := protoimpl.TypeBuilder{
		File: protoimpl.DescBuilder{
			GoPackagePath: reflect.TypeOf(x{}).PkgPath(),
			RawDescriptor: file_external_common_version_version_proto_rawDesc,
			NumEnums:      0,
			NumMessages:   2,
			NumExtensions: 0,
			NumServices:   0,
		},
		GoTypes:           file_external_common_version_version_proto_goTypes,
		DependencyIndexes: file_external_common_version_version_proto_depIdxs,
		MessageInfos:      file_external_common_version_version_proto_msgTypes,
	}.Build()
	File_external_common_version_version_proto = out.File
	file_external_common_version_version_proto_rawDesc = nil
	file_external_common_version_version_proto_goTypes = nil
	file_external_common_version_version_proto_depIdxs = nil
}
