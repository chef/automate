package main

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"time"

	"github.com/boltdb/bolt"
	"github.com/spf13/cobra"

	"github.com/chef/automate/components/automate-deployment/pkg/deployment"
	"github.com/chef/automate/components/automate-deployment/pkg/persistence/boltdb"
)

func init() {
	RootCmd.AddCommand(inspectDBCmd())
}

const inspectDBCaveats = `NOTE:
This command cannot run on an open database. You must first create a copy of
the database with the dumpdb command or else stop the deployment service when
using this tool.

In addition, this command requires the database file to be located on a
filesystem that supports memory-mapped files. In particular, VBox shared
filesystems are known to not support memory mapping.
`

func inspectDBCmd() *cobra.Command {
	return &cobra.Command{
		Use:   "inspect-db DATABASE_FILE",
		Short: "Print a JSON-like representation of a deployment service db",
		Long:  "Print a JSON-like representation of a deployment service db\n" + inspectDBCaveats,
		Run:   runInspectDB,
	}
}

func fatal(msg string, err error) {
	fmt.Fprintf(os.Stderr, "%s: %s\n", msg, err.Error())
	os.Exit(1)
}

func runInspectDB(cmd *cobra.Command, args []string) {
	if len(args) < 1 {
		fmt.Fprintln(os.Stderr, "You must provide the DATABASE_FILE to inspect")
		os.Exit(1)
	}

	dbFile, err := filepath.Abs(args[0])
	if err != nil {
		fatal("failed to expand path to boltdb file", err)
	}

	bolt.DefaultOptions.Timeout = 5 * time.Second

	db, err := bolt.Open(dbFile, 0600, bolt.DefaultOptions)
	if err != nil {
		// Also print inspectDBCaveats here in case someone tries to
		// run this tool on a live bolt.db and gets a timeout on open
		fmt.Fprintf(os.Stderr, "Failed to open database: %s\n", err.Error())
		fmt.Fprintln(os.Stderr, inspectDBCaveats)
		os.Exit(1)
	}

	store := boltdb.NewDeploymentStore(db)
	deployment, version, err := store.TryRead()
	if err != nil {
		fallbackDump(db, err)
	} else {
		fmt.Printf("Version: %s\n", version.Name())
		fmt.Printf("Deployment:\n")
		prettyPrintDeploy(deployment)
	}
}

func prettyPrintDeploy(d *deployment.Deployment) {
	buf := new(bytes.Buffer)
	pretty(buf, reflect.ValueOf(d), 0)
	fmt.Println(buf.String())
}

//
// Why?  I just wanted newlines in our pretty-printing.
//
// Some inspiration from:
//  https://gist.github.com/justincase/5469009
//
//
func pretty(buf io.Writer, v reflect.Value, depth int) {
	indent := strings.Repeat("  ", depth)
	nextIndent := strings.Repeat("  ", depth+1)

	if !v.IsValid() {
		fmt.Fprint(buf, "(nil)")
		return
	}

	// https://golang.org/pkg/reflect/#Kind
	//
	// A bit out of order because I put the aggregate types all at
	// the bottom.
	switch v.Kind() {
	case reflect.Bool:
		fmt.Fprintf(buf, "%v", v.Bool())
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		fmt.Fprintf(buf, "%v", v.Int())
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		fmt.Fprintf(buf, "%v", v.Uint())
	case reflect.Float32, reflect.Float64:
		fmt.Fprintf(buf, "%v", v.Float())
	case reflect.Complex64, reflect.Complex128:
		fmt.Fprintf(buf, "%v", v.Complex())
	case reflect.String:
		fmt.Fprintf(buf, "\"%v\"", v.String())
	case reflect.Interface, reflect.Ptr:
		pretty(buf, v.Elem(), depth)
	case reflect.Array, reflect.Slice:
		if v.Len() == 0 {
			fmt.Fprint(buf, "[]")
			return
		}

		fmt.Fprint(buf, "[\n")
		for i := 0; i < v.Len(); i++ {
			fmt.Fprint(buf, nextIndent)
			pretty(buf, v.Index(i), depth+1)
			fmt.Fprint(buf, ",\n")
		}
		fmt.Fprint(buf, indent)
		fmt.Fprint(buf, "]")
	case reflect.Struct:
		if v.CanInterface() {
			// This lets us print things like timestamps
			// that already know how to print themselves.
			// Interface() panics on non-public fields so
			// only call it if CanInterface is true.
			if iface, ok := v.Interface().(fmt.Stringer); ok {
				fmt.Fprint(buf, iface.String())
				return
			}
		}

		if v.NumField() == 0 {
			fmt.Fprint(buf, "{}")
			return
		}

		fmt.Fprint(buf, "{\n")
		for i := 0; i < v.NumField(); i++ {
			field := v.Field(i)
			fieldName := v.Type().Field(i).Name
			if skipPBInternal(fieldName, field) {
				continue
			}

			fmt.Fprint(buf, nextIndent)
			fmt.Fprint(buf, fieldName)
			fmt.Fprint(buf, ": ")
			pretty(buf, field, depth+1)
			fmt.Fprint(buf, ",\n")
		}
		fmt.Fprint(buf, indent)
		fmt.Fprint(buf, "}")
	case reflect.Map:
		if v.Len() == 0 {
			fmt.Fprint(buf, "{}")
			return
		}

		fmt.Fprint(buf, "{\n")
		for _, k := range v.MapKeys() {
			fmt.Fprint(buf, indent)
			fmt.Fprint(buf, k.String())
			fmt.Fprint(buf, ": ")
			pretty(buf, v.MapIndex(k), depth+1)
			fmt.Fprint(buf, ",\n")
		}
		fmt.Fprint(buf, indent)
		fmt.Fprint(buf, "}")
	default: // Chan, Func, UnsafePointer, and anything new, just print. String() is special and always returns us /something/
		fmt.Fprint(buf, v.String())
	}
}

// skipPBInternal returns true if the field looks like protobuf
// internal state that isn't useful to print.
func skipPBInternal(fieldName string, field reflect.Value) bool {
	switch fieldName {
	case "state":
		typeName := field.Type().Name()
		if typeName == "MessageState" {
			return true
		}
	case "sizeCache":
		return true
	case "unknownFields":
		if field.Kind() == reflect.Slice && field.Len() == 0 {
			return true
		}
	}
	return false
}

func fallbackDump(db *bolt.DB, err error) {
	fmt.Fprintf(os.Stderr, "BoltDB database file does not not match known formats:\n%s\n", err.Error())
	err = db.View(func(tx *bolt.Tx) error {
		err := tx.ForEach(func(name []byte, b *bolt.Bucket) error {
			fmt.Printf("Bucket: %s\n", name)
			err := b.ForEach(func(key, value []byte) error {
				fmt.Printf("  Key: %s\n", key)
				// Printing the values will dump a lot to the screen.
				// Punt for now, we can always use `strings`
				return nil
			})
			return err
		})
		return err
	})
	if err != nil {
		fatal("error reading database", err)
	}
}
