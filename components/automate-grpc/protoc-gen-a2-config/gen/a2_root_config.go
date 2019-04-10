package gen

import (
	"bytes"
	"fmt"

	"github.com/dave/jennifer/jen"
	"github.com/golang/protobuf/proto"
	pgs "github.com/lyft/protoc-gen-star"
	pgsgo "github.com/lyft/protoc-gen-star/lang/go"

	"github.com/chef/automate/components/automate-grpc/protoc-gen-a2-config/api/a2conf"
)

const automateConfigPkg = "github.com/chef/automate/api/config/shared"

type A2RootConfigModule struct {
	*pgs.ModuleBase
	ctx pgsgo.Context
}

func NewA2RootConfigModule() pgs.Module {
	return &A2RootConfigModule{
		ModuleBase: &pgs.ModuleBase{},
	}
}

func (m *A2RootConfigModule) InitContext(c pgs.BuildContext) {
	m.ModuleBase.InitContext(c)
	m.ctx = pgsgo.InitContext(c.Parameters())
}

func (m *A2RootConfigModule) Name() string { return "a2_root_config" }

type rootMember struct {
	importPath  string
	pkgName     string
	msgName     string
	fieldName   string
	serviceName string
}

func (m *A2RootConfigModule) Execute(targets map[string]pgs.File, pkgs map[string]pgs.Package) []pgs.Artifact {

	for _, f := range targets {
		messages := f.AllMessages()

		for _, msg := range messages {
			if msg.Name().String() == "AutomateConfig" {
				fields := msg.Fields()

				acc := []rootMember{}
				for _, field := range fields {
					fieldMsg := field.Type().Embed()
					msgImportPath := m.ctx.ImportPath(fieldMsg)
					msgPkgName := m.ctx.PackageName(fieldMsg)
					msgName := m.ctx.Name(fieldMsg)
					fieldName := m.ctx.Name(field)

					serviceName := ""
					if proto.HasExtension(fieldMsg.Descriptor().GetOptions(), a2conf.E_ServiceConfig) {
						iext, err := proto.GetExtension(fieldMsg.Descriptor().GetOptions(), a2conf.E_ServiceConfig)
						m.CheckErr(err)
						serviceConfigInfo := iext.(*a2conf.ServiceConfig)
						serviceName = serviceConfigInfo.GetName()
					}

					acc = append(acc, rootMember{
						pkgName:     msgPkgName.String(),
						importPath:  msgImportPath.String(),
						msgName:     msgName.String(),
						fieldName:   fieldName.String(),
						serviceName: serviceName,
					})
				}
				out := bytes.Buffer{}
				m.applyTemplate(&out, f.File().Name().String(), msg, acc)
				generatedFileName := m.ctx.OutputPath(f).SetExt(".a2svc.go").String()
				m.Debugf("%s", generatedFileName)
				m.AddGeneratorFile(generatedFileName, out.String())
			}
		}
	}

	return m.Artifacts()
}

func (m *A2RootConfigModule) applyTemplate(
	buf *bytes.Buffer,
	protoFileName string,
	message pgs.Message,
	acc []rootMember) {

	importPath := m.ctx.ImportPath(message).String()
	pkgName := m.ctx.PackageName(message).String()
	msgName := m.ctx.Name(message).String()

	f := jen.NewFilePathName(importPath, pkgName)
	f.HeaderComment(fmt.Sprintf(commentFormat, protoFileName))

	f.Comment("NewAutomateConfig returns a new instance of AutomateConfig with zero values.")
	f.Add(m.generateNewAutomateConfigFunc(msgName, importPath, acc))
	f.Comment("DefaultAutomateConfig returns a new instance of Automate config with default values.")
	f.Add(m.generateDefaultAutomateConfigFunc(msgName, importPath, acc))
	f.Comment(`Validate verifies that all required configuration keys are present
and enforces other invariants on configuration option values.

If the configuration is valid, the returned error is nil.`)
	f.Add(m.generateValidateFunc(msgName, importPath, acc))
	f.Comment("SetGlobalConfig iterates over the AutomateConfig and applies global configuration")
	f.Add(m.generateSetGlobalConfigFunc(msgName, importPath, acc))
	f.Comment("PlatformServiceConfigForService gets the config for the service by name")
	f.Add(m.generatePlatformServiceConfigForService(msgName, importPath, acc))

	m.CheckErr(f.Render(buf))
}

func (m *A2RootConfigModule) generateNewAutomateConfigFunc(
	msgName string, importPath string, acc []rootMember) *jen.Statement {
	return jen.Func().Id("NewAutomateConfig").Params().Params(jen.Qual(importPath, "*"+msgName)).BlockFunc(
		func(g *jen.Group) {
			g.Return().Op("&").Id(msgName).Values(jen.DictFunc(func(d jen.Dict) {
				for _, member := range acc {
					if member.fieldName == "Global" {
						d[jen.Id(member.fieldName)] = jen.Qual(member.importPath, "NewGlobalConfig").Call()
					} else {
						d[jen.Id(member.fieldName)] = jen.Qual(member.importPath, "NewConfigRequest").Call()
					}
				}
			}))
		})
}

func (m *A2RootConfigModule) generateDefaultAutomateConfigFunc(
	msgName string, importPath string, acc []rootMember) *jen.Statement {
	return jen.Func().Id("DefaultAutomateConfig").Params().Params(jen.Qual(importPath, "*"+msgName)).BlockFunc(
		func(g *jen.Group) {
			g.Return().Op("&").Id(msgName).Values(jen.DictFunc(func(d jen.Dict) {
				for _, member := range acc {
					if member.fieldName == "Global" {
						d[jen.Id(member.fieldName)] = jen.Qual(member.importPath, "DefaultGlobalConfig").Call()
					} else {
						d[jen.Id(member.fieldName)] = jen.Qual(member.importPath, "DefaultConfigRequest").Call()
					}
				}
			}))
		})
}

func (m *A2RootConfigModule) generateValidateFunc(
	msgName string, importPath string, acc []rootMember) *jen.Statement {
	return jen.Func().Params(jen.Id("c").Qual(importPath, "*"+msgName)).Id("Validate").Params().Params(
		jen.Error()).BlockFunc(func(g *jen.Group) {
		g.Id("err").Op(":=").Qual(automateConfigPkg, "Validate").CallFunc(func(cg *jen.Group) {
			for _, member := range acc {
				cg.Id("c").Dot(member.fieldName).Dot("Validate").Call()
			}
		})
		g.If(jen.Id("err").Op("==").Nil().BlockFunc(func(ifg *jen.Group) {
			ifg.Return(jen.Nil())
		}))
		g.List(jen.Id("cfgErr"), jen.Id("ok")).Op(":=").Id("err").Assert(jen.Qual(automateConfigPkg, "Error"))
		g.If(jen.Id("ok")).BlockFunc(func(ifg *jen.Group) {
			ifg.If(jen.Id("cfgErr").Dot("IsEmpty").Call()).BlockFunc(func(ifg *jen.Group) {
				ifg.Return(jen.Nil())
			})
		})
		g.Return(jen.Id("err"))
	})
}

func (m *A2RootConfigModule) generateSetGlobalConfigFunc(
	msgName string, importPath string, acc []rootMember) *jen.Statement {
	return jen.Func().Params(jen.Id("c").Qual(importPath, "*"+msgName)).Id("SetGlobalConfig").Params().BlockFunc(
		func(g *jen.Group) {
			for _, member := range acc {
				if member.fieldName != "Global" {
					g.Id("c").Dot(member.fieldName).Dot("SetGlobalConfig").Call(jen.Id("c").Dot("Global"))
				}
			}
		})
}

func (m *A2RootConfigModule) generatePlatformServiceConfigForService(
	msgName string, importPath string, acc []rootMember) *jen.Statement {
	return jen.Func().Params(jen.Id("c").Qual(importPath, "*"+msgName)).Id("PlatformServiceConfigForService").
		Params(jen.Id("serviceName").String()).Params(
		jen.Qual(
			automateConfigPkg,
			"PlatformServiceConfigurable"), jen.Bool()).BlockFunc(func(g *jen.Group) {
		g.Switch(jen.Id("serviceName")).BlockFunc(func(cg *jen.Group) {
			for _, member := range acc {
				if member.serviceName == "" {
					continue
				}
				cg.Case(jen.Lit(member.serviceName)).Block(
					jen.Return().List(
						jen.Id("c").Dot(member.fieldName),
						jen.True(),
					),
				)
			}
			cg.Default().Block(
				jen.Return().List(jen.Nil(), jen.False()),
			)
		})

	})

}
