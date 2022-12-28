package server

import (
	"log"
	"testing"

	"github.com/chef/automate/api/interservice/infra_proxy/request"
	chef "github.com/go-chef/chef"
	"github.com/stretchr/testify/require"
	"google.golang.org/protobuf/types/known/structpb"
)

func Test_nodeAttributeFromParams(t *testing.T) {
	val, err := structpb.NewStruct(map[string]interface{}{
		"id":    "SometestId001",
		"value": "SomeTestVal001",
	})
	require.NoError(t, err)

	var autoAttribure1 = map[string]interface{}{
		"id": "SometestId001", "value": "SomeTestVal001",
	}

	val2, err := structpb.NewStruct(map[string]interface{}{
		"id":    1,
		"value": 33,
	})
	require.NoError(t, err)

	var autoAttribure2 = map[string]interface{}{
		"id": 1, "value": 33,
	}

	type args struct {
		req *request.NodeDetails
	}
	tests := []struct {
		name    string
		args    args
		want    *chef.Node
		wantErr bool
	}{
		{
			name: "Test 1 map[string]string",
			args: args{
				req: &request.NodeDetails{
					OrgId:               "org001",
					ServerId:            "server001",
					Name:                "node001",
					Environment:         "testEnv001",
					PolicyName:          "policy001",
					PolicyGroup:         "group001",
					RunList:             []string{"list001", "list002"},
					AutomaticAttributes: val,
					NormalAttributes:    val,
					DefaultAttributes:   val,
					OverrideAttributes:  val,
				},
			},
			want: &chef.Node{
				Name:                "node001",
				Environment:         "testEnv001",
				ChefType:            "",
				AutomaticAttributes: autoAttribure1,
				NormalAttributes:    autoAttribure1,
				DefaultAttributes:   autoAttribure1,
				OverrideAttributes:  autoAttribure1,
				JsonClass:           "",
				RunList:             []string{"list001", "list002"},
				PolicyName:          "policy001",
				PolicyGroup:         "group001",
			},
			wantErr: false,
		},

		{
			name: "Test 2 map[string]int",
			args: args{
				req: &request.NodeDetails{
					OrgId:               "org002",
					ServerId:            "server002",
					Name:                "node002",
					Environment:         "testEnv002",
					PolicyName:          "policy002",
					PolicyGroup:         "group002",
					RunList:             []string{"list001", "list002"},
					AutomaticAttributes: val2,
					NormalAttributes:    val2,
					DefaultAttributes:   val2,
					OverrideAttributes:  val2,
				},
			},
			want: &chef.Node{
				Name:                "node002",
				Environment:         "testEnv002",
				AutomaticAttributes: autoAttribure2,
				NormalAttributes:    autoAttribure2,
				DefaultAttributes:   autoAttribure2,
				OverrideAttributes:  autoAttribure2,
				RunList:             []string{"list001", "list002"},
				PolicyName:          "policy002",
				PolicyGroup:         "group002",
			},
			wantErr: false,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := nodeAttributeFromParams(tt.args.req)

			if err != nil && tt.wantErr {
				require.Error(t, err)
				require.True(t, tt.wantErr)
				return
			}

			require.NoError(t, err)
			require.Equal(t, tt.args.req.Name, got.Name)
			require.Equal(t, tt.args.req.Environment, got.Environment)
			require.Equal(t, tt.args.req.PolicyGroup, got.PolicyGroup)
			require.Equal(t, tt.args.req.PolicyName, got.PolicyName)
			require.Equal(t, len(tt.args.req.RunList), len(got.RunList))

			require.NotNil(t, got.AutomaticAttributes)
			require.NotEmpty(t, got.AutomaticAttributes)

			require.NotNil(t, got.NormalAttributes)
			require.NotEmpty(t, got.NormalAttributes)

			require.NotNil(t, got.DefaultAttributes)
			require.NotEmpty(t, got.DefaultAttributes)

			require.NotNil(t, got.OverrideAttributes)
			require.NotEmpty(t, got.OverrideAttributes)
		})
	}
}
func Test_responseNodeObject(t *testing.T) {
	node := &chef.Node{
		Name:                "node001",
		Environment:         "env001",
		ChefType:            "type001",
		AutomaticAttributes: nil,
	}
	got, err := responseNodeObject(node)
	require.NoError(t, err)
	require.NotEmpty(t, got)
	require.NotNil(t, got)
	require.Equal(t, node.Name, got.Name)
	require.Equal(t, node.Environment, got.Environment)

	log.Printf("GOT: %+v", got)

}

// func Test_responseNodeObject(t *testing.T) {
// 	var autoAttribure1 = make(map[string]interface{})
// 	autoAttribure1["attr"] = nil

// 	type args struct {
// 		node *chef.Node
// 	}
// 	tests := []struct {
// 		name    string
// 		args    args
// 		want    *response.Node
// 		wantErr bool
// 	}{
// 		{
// 			name: "Test 1",
// 			args: args{node: &chef.Node{
// 				Name:                "node001",
// 				Environment:         "env001",
// 				ChefType:            "type001",
// 				AutomaticAttributes: autoAttribure1,
// 			}},
// 			want:    &response.Node{},
// 			wantErr: true,
// 		},
// 	}
// 	for _, tt := range tests {
// 		t.Run(tt.name, func(t *testing.T) {
// 			got, err := responseNodeObject(tt.args.node)
// 			log.Printf("GOT: %+v", got)
// 			if (err != nil) != tt.wantErr {
// 				t.Errorf("responseNodeObject() error = %v, wantErr %v", err, tt.wantErr)
// 				return
// 			}
// 			if !reflect.DeepEqual(got, tt.want) {
// 				t.Errorf("responseNodeObject() = %v, want %v", got, tt.want)
// 			}
// 		})
// 	}
// }
