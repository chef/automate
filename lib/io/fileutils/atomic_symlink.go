package fileutils

import (
	"math/rand"
	"os"
	"path"
)

func AtomicSymlink(oldname string, newname string) error {
	tmpName := newname + ".tmp." + randSeq(6)
	exists, err := PathExists(tmpName)
	if err != nil {
		return err
	}

	if exists {
		if err := os.Remove(tmpName); err != nil {
			return err
		}
	}

	if err := os.Symlink(oldname, tmpName); err != nil {
		return err
	}

	if err := os.Rename(tmpName, newname); err != nil {
		return err
	}

	if err := syncDir(path.Dir(newname)); err != nil {
		return err
	}
	return nil
}

var letters = []rune("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")

func randSeq(n int) string {
	b := make([]rune, n)
	for i := range b {
		b[i] = letters[rand.Intn(len(letters))]
	}
	return string(b)
}
