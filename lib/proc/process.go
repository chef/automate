package proc

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"path"
	"strconv"

	"github.com/pkg/errors"
)

// ErrNotFound is returned if the process is not found
var ErrNotFound = errors.New("Process not found")

// Process represents a process from /proc
type Process struct {
	// Pid is the process ID.
	Pid int

	basePath string
}

// ProcessStat contains (some of) the contents of /proc/pid/stat
type ProcessStat struct {
	Pid   int
	Comm  string
	State string
	PPid  int
}

type listAllConf struct {
	mountPoint string
}

// ListAllOpt are functional options for listing processes
type ListAllOpt func(*listAllConf)

// WithProcMount sets the mount point for proc
func WithProcMount(mountPoint string) ListAllOpt {
	return func(conf *listAllConf) {
		if mountPoint != "" {
			conf.mountPoint = mountPoint
		}
	}
}

// ListAll lists all the processes under the proc mount
func ListAll(opts ...ListAllOpt) ([]*Process, error) {
	conf := listAllConf{
		mountPoint: "/proc",
	}

	for _, o := range opts {
		o(&conf)
	}

	files, err := ioutil.ReadDir(conf.mountPoint)
	if err != nil {
		return nil, err
	}

	procs := make([]*Process, 0, len(files))
	for _, f := range files {
		if f.IsDir() {
			if pid, err := strconv.ParseInt(f.Name(), 10, 32); err == nil {
				basePath := path.Join(conf.mountPoint, f.Name())
				proc := Process{
					Pid:      int(pid),
					basePath: basePath,
				}
				procs = append(procs, &proc)
			}
		}
	}

	return procs, nil
}

// Stat returns the contents of /proc/pid/stat
func (proc *Process) Stat() (*ProcessStat, error) {
	statPath := path.Join(proc.basePath, "stat")
	data, err := ioutil.ReadFile(statPath)
	if err != nil {
		return nil, err
	}

	commStart := bytes.Index(data, []byte("("))
	commEnd := bytes.LastIndex(data, []byte(")"))

	if commStart < 0 || commEnd < 0 || commEnd < commStart {
		return nil, errors.Errorf("Could not parse %s", statPath)
	}

	procStat := ProcessStat{}
	_, err = fmt.Fscan(
		bytes.NewBuffer(data[commEnd+2:]),
		&procStat.State,
		&procStat.PPid)

	if err != nil {
		return nil, err
	}

	procStat.Pid = proc.Pid
	procStat.Comm = string(data[commStart+1 : commEnd])

	return &procStat, nil
}

// ProcessTreeProc is a Process with Children
type ProcessTreeProc struct {
	Process
	Children []*ProcessTreeProc
}

// ProcessTree organizes the processes in a tree
// you can follow each pid, and each of the processes that have
// that pid as a parent will be listed as a child
type ProcessTree map[int]*ProcessTreeProc

// Tree gets the process tree
func Tree(opts ...ListAllOpt) (ProcessTree, error) {
	procs, err := ListAll(opts...)
	if err != nil {
		return nil, err
	}

	tree := make(ProcessTree)

	// Create root for each pid
	for _, proc := range procs {
		tree[proc.Pid] = &ProcessTreeProc{
			Process:  *proc,
			Children: []*ProcessTreeProc{},
		}
	}

	// Create parent child links
	for _, proc := range tree {
		if stat, err := proc.Stat(); err == nil {
			ppid := stat.PPid
			if pproc, exists := tree[ppid]; exists {
				pproc.Children = append(pproc.Children, proc)
			}
		} else {
			// We're going to allow path errors because none of
			// this is atomic and things can be pulled out from
			// underneath us
			if _, isPathErr := err.(*os.PathError); !isPathErr {
				return nil, err
			}
		}
	}

	return tree, nil
}
