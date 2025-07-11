// Code generated by protoc-gen-go. DO NOT EDIT.
// versions:
// 	protoc-gen-go v1.36.6
// 	protoc        v3.19.0
// source: external/infra_proxy/request/policyfiles.proto

package request

import (
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

type Policyfiles struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// Chef Organization ID.
	OrgId string `protobuf:"bytes,1,opt,name=org_id,json=orgId,proto3" json:"org_id,omitempty"`
	// Chef Infra Server ID.
	ServerId      string `protobuf:"bytes,2,opt,name=server_id,json=serverId,proto3" json:"server_id,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *Policyfiles) Reset() {
	*x = Policyfiles{}
	mi := &file_external_infra_proxy_request_policyfiles_proto_msgTypes[0]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *Policyfiles) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*Policyfiles) ProtoMessage() {}

func (x *Policyfiles) ProtoReflect() protoreflect.Message {
	mi := &file_external_infra_proxy_request_policyfiles_proto_msgTypes[0]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use Policyfiles.ProtoReflect.Descriptor instead.
func (*Policyfiles) Descriptor() ([]byte, []int) {
	return file_external_infra_proxy_request_policyfiles_proto_rawDescGZIP(), []int{0}
}

func (x *Policyfiles) GetOrgId() string {
	if x != nil {
		return x.OrgId
	}
	return ""
}

func (x *Policyfiles) GetServerId() string {
	if x != nil {
		return x.ServerId
	}
	return ""
}

type Policyfile struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// Chef Organization ID.
	OrgId string `protobuf:"bytes,1,opt,name=org_id,json=orgId,proto3" json:"org_id,omitempty"`
	// Chef Infra Server ID.
	ServerId string `protobuf:"bytes,2,opt,name=server_id,json=serverId,proto3" json:"server_id,omitempty"`
	// Policyfile name.
	Name string `protobuf:"bytes,3,opt,name=name,proto3" json:"name,omitempty"`
	// Policyfile revision ID.
	RevisionId    string `protobuf:"bytes,4,opt,name=revision_id,json=revisionId,proto3" json:"revision_id,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *Policyfile) Reset() {
	*x = Policyfile{}
	mi := &file_external_infra_proxy_request_policyfiles_proto_msgTypes[1]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *Policyfile) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*Policyfile) ProtoMessage() {}

func (x *Policyfile) ProtoReflect() protoreflect.Message {
	mi := &file_external_infra_proxy_request_policyfiles_proto_msgTypes[1]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use Policyfile.ProtoReflect.Descriptor instead.
func (*Policyfile) Descriptor() ([]byte, []int) {
	return file_external_infra_proxy_request_policyfiles_proto_rawDescGZIP(), []int{1}
}

func (x *Policyfile) GetOrgId() string {
	if x != nil {
		return x.OrgId
	}
	return ""
}

func (x *Policyfile) GetServerId() string {
	if x != nil {
		return x.ServerId
	}
	return ""
}

func (x *Policyfile) GetName() string {
	if x != nil {
		return x.Name
	}
	return ""
}

func (x *Policyfile) GetRevisionId() string {
	if x != nil {
		return x.RevisionId
	}
	return ""
}

type DeletePolicyfile struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// Chef Organization ID.
	OrgId string `protobuf:"bytes,1,opt,name=org_id,json=orgId,proto3" json:"org_id,omitempty"`
	// Chef Infra Server ID.
	ServerId string `protobuf:"bytes,2,opt,name=server_id,json=serverId,proto3" json:"server_id,omitempty"`
	// Policyfile name.
	Name          string `protobuf:"bytes,3,opt,name=name,proto3" json:"name,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *DeletePolicyfile) Reset() {
	*x = DeletePolicyfile{}
	mi := &file_external_infra_proxy_request_policyfiles_proto_msgTypes[2]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *DeletePolicyfile) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*DeletePolicyfile) ProtoMessage() {}

func (x *DeletePolicyfile) ProtoReflect() protoreflect.Message {
	mi := &file_external_infra_proxy_request_policyfiles_proto_msgTypes[2]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use DeletePolicyfile.ProtoReflect.Descriptor instead.
func (*DeletePolicyfile) Descriptor() ([]byte, []int) {
	return file_external_infra_proxy_request_policyfiles_proto_rawDescGZIP(), []int{2}
}

func (x *DeletePolicyfile) GetOrgId() string {
	if x != nil {
		return x.OrgId
	}
	return ""
}

func (x *DeletePolicyfile) GetServerId() string {
	if x != nil {
		return x.ServerId
	}
	return ""
}

func (x *DeletePolicyfile) GetName() string {
	if x != nil {
		return x.Name
	}
	return ""
}

type PolicyfileRevisions struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// Chef Organization ID.
	OrgId string `protobuf:"bytes,1,opt,name=org_id,json=orgId,proto3" json:"org_id,omitempty"`
	// Chef Infra Server ID.
	ServerId string `protobuf:"bytes,2,opt,name=server_id,json=serverId,proto3" json:"server_id,omitempty"`
	// Policyfile name.
	Name          string `protobuf:"bytes,3,opt,name=name,proto3" json:"name,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *PolicyfileRevisions) Reset() {
	*x = PolicyfileRevisions{}
	mi := &file_external_infra_proxy_request_policyfiles_proto_msgTypes[3]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *PolicyfileRevisions) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*PolicyfileRevisions) ProtoMessage() {}

func (x *PolicyfileRevisions) ProtoReflect() protoreflect.Message {
	mi := &file_external_infra_proxy_request_policyfiles_proto_msgTypes[3]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use PolicyfileRevisions.ProtoReflect.Descriptor instead.
func (*PolicyfileRevisions) Descriptor() ([]byte, []int) {
	return file_external_infra_proxy_request_policyfiles_proto_rawDescGZIP(), []int{3}
}

func (x *PolicyfileRevisions) GetOrgId() string {
	if x != nil {
		return x.OrgId
	}
	return ""
}

func (x *PolicyfileRevisions) GetServerId() string {
	if x != nil {
		return x.ServerId
	}
	return ""
}

func (x *PolicyfileRevisions) GetName() string {
	if x != nil {
		return x.Name
	}
	return ""
}

type Policygroup struct {
	state protoimpl.MessageState `protogen:"open.v1"`
	// Chef Organization ID.
	OrgId string `protobuf:"bytes,1,opt,name=org_id,json=orgId,proto3" json:"org_id,omitempty"`
	// Chef Infra Server ID.
	ServerId string `protobuf:"bytes,2,opt,name=server_id,json=serverId,proto3" json:"server_id,omitempty"`
	// Policygroup name.
	Name          string `protobuf:"bytes,3,opt,name=name,proto3" json:"name,omitempty"`
	unknownFields protoimpl.UnknownFields
	sizeCache     protoimpl.SizeCache
}

func (x *Policygroup) Reset() {
	*x = Policygroup{}
	mi := &file_external_infra_proxy_request_policyfiles_proto_msgTypes[4]
	ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
	ms.StoreMessageInfo(mi)
}

func (x *Policygroup) String() string {
	return protoimpl.X.MessageStringOf(x)
}

func (*Policygroup) ProtoMessage() {}

func (x *Policygroup) ProtoReflect() protoreflect.Message {
	mi := &file_external_infra_proxy_request_policyfiles_proto_msgTypes[4]
	if x != nil {
		ms := protoimpl.X.MessageStateOf(protoimpl.Pointer(x))
		if ms.LoadMessageInfo() == nil {
			ms.StoreMessageInfo(mi)
		}
		return ms
	}
	return mi.MessageOf(x)
}

// Deprecated: Use Policygroup.ProtoReflect.Descriptor instead.
func (*Policygroup) Descriptor() ([]byte, []int) {
	return file_external_infra_proxy_request_policyfiles_proto_rawDescGZIP(), []int{4}
}

func (x *Policygroup) GetOrgId() string {
	if x != nil {
		return x.OrgId
	}
	return ""
}

func (x *Policygroup) GetServerId() string {
	if x != nil {
		return x.ServerId
	}
	return ""
}

func (x *Policygroup) GetName() string {
	if x != nil {
		return x.Name
	}
	return ""
}

var File_external_infra_proxy_request_policyfiles_proto protoreflect.FileDescriptor

const file_external_infra_proxy_request_policyfiles_proto_rawDesc = "" +
	"\n" +
	".external/infra_proxy/request/policyfiles.proto\x12%chef.automate.api.infra_proxy.request\"A\n" +
	"\vPolicyfiles\x12\x15\n" +
	"\x06org_id\x18\x01 \x01(\tR\x05orgId\x12\x1b\n" +
	"\tserver_id\x18\x02 \x01(\tR\bserverId\"u\n" +
	"\n" +
	"Policyfile\x12\x15\n" +
	"\x06org_id\x18\x01 \x01(\tR\x05orgId\x12\x1b\n" +
	"\tserver_id\x18\x02 \x01(\tR\bserverId\x12\x12\n" +
	"\x04name\x18\x03 \x01(\tR\x04name\x12\x1f\n" +
	"\vrevision_id\x18\x04 \x01(\tR\n" +
	"revisionId\"Z\n" +
	"\x10DeletePolicyfile\x12\x15\n" +
	"\x06org_id\x18\x01 \x01(\tR\x05orgId\x12\x1b\n" +
	"\tserver_id\x18\x02 \x01(\tR\bserverId\x12\x12\n" +
	"\x04name\x18\x03 \x01(\tR\x04name\"]\n" +
	"\x13PolicyfileRevisions\x12\x15\n" +
	"\x06org_id\x18\x01 \x01(\tR\x05orgId\x12\x1b\n" +
	"\tserver_id\x18\x02 \x01(\tR\bserverId\x12\x12\n" +
	"\x04name\x18\x03 \x01(\tR\x04name\"U\n" +
	"\vPolicygroup\x12\x15\n" +
	"\x06org_id\x18\x01 \x01(\tR\x05orgId\x12\x1b\n" +
	"\tserver_id\x18\x02 \x01(\tR\bserverId\x12\x12\n" +
	"\x04name\x18\x03 \x01(\tR\x04nameB;Z9github.com/chef/automate/api/external/infra_proxy/requestb\x06proto3"

var (
	file_external_infra_proxy_request_policyfiles_proto_rawDescOnce sync.Once
	file_external_infra_proxy_request_policyfiles_proto_rawDescData []byte
)

func file_external_infra_proxy_request_policyfiles_proto_rawDescGZIP() []byte {
	file_external_infra_proxy_request_policyfiles_proto_rawDescOnce.Do(func() {
		file_external_infra_proxy_request_policyfiles_proto_rawDescData = protoimpl.X.CompressGZIP(unsafe.Slice(unsafe.StringData(file_external_infra_proxy_request_policyfiles_proto_rawDesc), len(file_external_infra_proxy_request_policyfiles_proto_rawDesc)))
	})
	return file_external_infra_proxy_request_policyfiles_proto_rawDescData
}

var file_external_infra_proxy_request_policyfiles_proto_msgTypes = make([]protoimpl.MessageInfo, 5)
var file_external_infra_proxy_request_policyfiles_proto_goTypes = []any{
	(*Policyfiles)(nil),         // 0: chef.automate.api.infra_proxy.request.Policyfiles
	(*Policyfile)(nil),          // 1: chef.automate.api.infra_proxy.request.Policyfile
	(*DeletePolicyfile)(nil),    // 2: chef.automate.api.infra_proxy.request.DeletePolicyfile
	(*PolicyfileRevisions)(nil), // 3: chef.automate.api.infra_proxy.request.PolicyfileRevisions
	(*Policygroup)(nil),         // 4: chef.automate.api.infra_proxy.request.Policygroup
}
var file_external_infra_proxy_request_policyfiles_proto_depIdxs = []int32{
	0, // [0:0] is the sub-list for method output_type
	0, // [0:0] is the sub-list for method input_type
	0, // [0:0] is the sub-list for extension type_name
	0, // [0:0] is the sub-list for extension extendee
	0, // [0:0] is the sub-list for field type_name
}

func init() { file_external_infra_proxy_request_policyfiles_proto_init() }
func file_external_infra_proxy_request_policyfiles_proto_init() {
	if File_external_infra_proxy_request_policyfiles_proto != nil {
		return
	}
	type x struct{}
	out := protoimpl.TypeBuilder{
		File: protoimpl.DescBuilder{
			GoPackagePath: reflect.TypeOf(x{}).PkgPath(),
			RawDescriptor: unsafe.Slice(unsafe.StringData(file_external_infra_proxy_request_policyfiles_proto_rawDesc), len(file_external_infra_proxy_request_policyfiles_proto_rawDesc)),
			NumEnums:      0,
			NumMessages:   5,
			NumExtensions: 0,
			NumServices:   0,
		},
		GoTypes:           file_external_infra_proxy_request_policyfiles_proto_goTypes,
		DependencyIndexes: file_external_infra_proxy_request_policyfiles_proto_depIdxs,
		MessageInfos:      file_external_infra_proxy_request_policyfiles_proto_msgTypes,
	}.Build()
	File_external_infra_proxy_request_policyfiles_proto = out.File
	file_external_infra_proxy_request_policyfiles_proto_goTypes = nil
	file_external_infra_proxy_request_policyfiles_proto_depIdxs = nil
}
