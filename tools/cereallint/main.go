package main

import (
	"fmt"
	"go/ast"
	"go/token"
	"go/types"
	"os"
	"sync"

	"golang.org/x/tools/go/packages"
)

func fatal(err error, msg string) {
	fmt.Fprintf(os.Stderr, "%s: %s", err.Error(), msg)
	os.Exit(1)
}

func main() {
	pkgSpecs := os.Args[1:]

	cfg := &packages.Config{
		Mode:  packages.LoadAllSyntax,
		Tests: false,
	}

	pkgs, err := packages.Load(cfg, pkgSpecs...)
	if err != nil {
		fatal(err, "failed to load packages")
	}

	errs := []error{}
	for _, pkg := range pkgs {
		for _, e := range pkg.Errors {
			errs = append(errs, e)
		}
	}
	if len(errs) > 0 {
		for _, e := range errs {
			fmt.Fprintf(os.Stderr, "%s\n", e.Error())
		}
		fatal(fmt.Errorf("found %d compilation errors", len(errs)), "failed to load packages")
	}

	err = lintPackages(pkgs)
	if err != nil {
		fatal(err, "your cereal is soggy")
	}
}

func lintPackages(pkgs []*packages.Package) error {
	wg := sync.WaitGroup{}
	lintFindings := &LintFindings{}

	for _, pkg := range pkgs {
		wg.Add(1)
		go func(pkg *packages.Package) {
			defer wg.Done()
			v := &visitor{
				pkg: pkg,
			}
			for _, astFile := range v.pkg.Syntax {
				ast.Walk(v, astFile)
			}

			lintFindings.Add(v.findings...)
		}(pkg)
	}

	wg.Wait()

	if len(lintFindings.findings) > 0 {
		for _, f := range lintFindings.findings {
			fmt.Printf("%s:%d: %s\n", f.pos.Filename, f.pos.Line, f.err.Error())
		}
		return fmt.Errorf("cereallint found %d possible problems", len(lintFindings.findings))
	}
	return nil
}

type LintFinding struct {
	pos token.Position
	err error
}

type LintFindings struct {
	sync.Mutex
	findings []LintFinding
}

func (l *LintFindings) Add(findings ...LintFinding) {
	l.Lock()
	l.findings = append(l.findings, findings...)
	l.Unlock()
}

type visitor struct {
	pkg      *packages.Package
	findings []LintFinding
}

func (v *visitor) Visit(n ast.Node) ast.Visitor {
	v.checkNode(n)
	return v
}

func (v *visitor) addFinding(position token.Pos, call *ast.CallExpr, err error) {
	pos := v.pkg.Fset.Position(position)
	v.findings = append(v.findings, LintFinding{pos, err})
}

func (v *visitor) checkNode(n ast.Node) {
	switch n.(type) {
	case ast.Stmt:
	case ast.Expr:
		exp := n.(ast.Expr)
		switch exp.(type) {
		case *ast.CallExpr:
			call := exp.(*ast.CallExpr)
			v.checkCallsForStringLiterals(call)
		}
	default:
	}
}

func (v *visitor) checkArgForLiteralOrUntypeConst(call *ast.CallExpr, argPos int, name string, funName string) {
	arg := call.Args[argPos]
	switch arg.(type) {
	case *ast.BasicLit:
		literal := arg.(*ast.BasicLit)
		err := fmt.Errorf("string literal %s used as %s in %s, consider using a typed constant",
			literal.Value,
			name,
			funName)
		v.addFinding(literal.ValuePos, call, err)
	case *ast.Ident:
		ident := arg.(*ast.Ident)
		t := v.pkg.TypesInfo.ObjectOf(ident).Type()
		if b, ok := t.(*types.Basic); ok {
			if b.Kind() == types.UntypedString {
				err := fmt.Errorf("untyped constant %s used as %s in %s, consider using a typed constant",
					ident.Name,
					name,
					funName)
				v.addFinding(ident.NamePos, call, err)

			}
		}
	}
}

func (v *visitor) checkCallsForStringLiterals(call *ast.CallExpr) {
	if selector, ok := call.Fun.(*ast.SelectorExpr); ok {
		fn, ok := v.pkg.TypesInfo.ObjectOf(selector.Sel).(*types.Func)
		if !ok {
			return
		}

		switch fn.FullName() {
		case "(*github.com/chef/automate/lib/cereal.Manager).RegisterTaskExecutor":
			v.checkArgForLiteralOrUntypeConst(call, 0, "task name", "RegisterTaskExecutor")
		case "(*github.com/chef/automate/lib/cereal.Manager).RegisterWorkflowExecutor":
			v.checkArgForLiteralOrUntypeConst(call, 0, "workflow name", "RegisterWorkflowExecutor")
		case "(*github.com/chef/automate/lib/cereal.Manager).EnqueueWorkflow":
			v.checkArgForLiteralOrUntypeConst(call, 1, "workflow name", "EnqueueWorkflow")
		case "(*github.com/chef/automate/lib/cereal.Manager).GetWorkflowInstanceByName":
			v.checkArgForLiteralOrUntypeConst(call, 2, "workflow name", "GetWorkflowInstanceByName")
		case "(*github.com/chef/automate/lib/cereal.Manager).GetWorkflowScheduleByName":
			v.checkArgForLiteralOrUntypeConst(call, 2, "workflow name", "GetWorkflowScheduleByName")
		case "(*github.com/chef/automate/lib/cereal.Manager).UpdateWorkflowScheduleByName":
			v.checkArgForLiteralOrUntypeConst(call, 2, "workflow name", "UpdateWorkflowScheduleByName")
		case "(*github.com/chef/automate/lib/cereal.Manager).CancelWorkflow":
			v.checkArgForLiteralOrUntypeConst(call, 1, "workflow name", "CancelWorkflow")
		case "(*github.com/chef/automate/lib/cereal.Manager).KillWorkflow":
			v.checkArgForLiteralOrUntypeConst(call, 1, "workflow name", "KillWorkflow")
		case "(github.com/chef/automate/lib/cereal.WorkflowInstance).EnqueueTask":
			v.checkArgForLiteralOrUntypeConst(call, 0, "task name", "EnqueueTask")
		}
	}
}
