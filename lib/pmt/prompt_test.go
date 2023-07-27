package pmt

import (
	"bytes"
	"errors"
	"fmt"
	"io"
	"math"
	"os"
	"strconv"
	"strings"
	"testing"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/chef/automate/lib/ioutils"
	"github.com/stretchr/testify/assert"
)

func pad(siz int, buf *bytes.Buffer) {
	pu := make([]byte, 4096-siz)
	for i := 0; i < 4096-siz; i++ {
		pu[i] = 97
	}
	buf.Write(pu)
}

func input(buf *bytes.Buffer, input string) {
	buf.WriteString(input)
	pad(len(input), buf)
}

func TestConfirm(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	input(b, moveDown+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)
	choice, err := p.Confirm("Do you want to test", "yes", "no")
	assert.NoError(t, err)
	assert.Equal(t, false, choice)
}

func TestSelect(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	input(b, moveDown+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)
	index, choice, err := p.Select("Do you want to test", "yes", "no")
	assert.NoError(t, err)
	assert.Equal(t, "no", choice)
	assert.Equal(t, 1, index)
}

func TestSelectSearch(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	input(b, "may"+moveDown+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)
	index, choice, err := p.SelectSearch("Do you want to test", "yes", "no", "may be no", "may be yes", "must be", "could be")
	assert.NoError(t, err)
	assert.Equal(t, "may be yes", choice)
	assert.Equal(t, 3, index)
}

func TestInputInt64(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	var num int64 = math.MaxInt64
	input(b, fmt.Sprint(num)+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)
	choice, err := p.InputInt64("give int64 of your choice")
	assert.NoError(t, err)
	assert.Equal(t, num, choice)
}

func TestInputInt(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	var num int = math.MaxInt32
	input(b, fmt.Sprint(num)+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)
	choice, err := p.InputInt("give int of your choice")
	assert.NoError(t, err)
	assert.Equal(t, num, choice)
}

func TestInputIntDefault(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	defNum := 1000
	input(b, "\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)
	choice, err := p.InputIntDefault("give int of your choice", defNum)
	assert.NoError(t, err)
	assert.Equal(t, defNum, choice)
}

func TestInputIntDefaultValidation(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	defNum := 99
	input(b, "\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)

	min := 10
	max := 100
	validationFunc := func(input string) (err error) {
		val, err := strconv.Atoi(input)
		if err != nil {
			return errors.New(INVALID_INT)
		}
		if val < min || val > max {
			return fmt.Errorf("number should be in range: %v <= N <= %v", min, max)
		}
		return nil
	}
	choice, err := p.InputIntDefaultValidation("give int of your choice", defNum, validationFunc)
	assert.NoError(t, err)
	assert.Equal(t, defNum, choice)
}

func TestInputIntRange(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	defNum := 99
	input(b, fmt.Sprint(defNum)+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)

	min := 10
	max := 100

	choice, err := p.InputIntRange("give int of your choice", min, max)
	assert.NoError(t, err)
	assert.Equal(t, defNum, choice)
}

func TestInputIntDefaultRange(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	defNum := 99
	input(b, fmt.Sprint(defNum)+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)

	min := 10
	max := 100

	choice, err := p.InputIntDefaultRange("give int of your choice", defNum, min, max)
	assert.NoError(t, err)
	assert.Equal(t, defNum, choice)
}

func TestInputFloat64(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	defNum := 99.9
	input(b, fmt.Sprint(defNum)+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)

	choice, err := p.InputFloat64("give float64 of your choice")
	assert.NoError(t, err)
	assert.Equal(t, defNum, choice)
}

func TestInputStringMinMax(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	valid := "abcde"
	input(b, valid+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)

	choice, err := p.InputStringMinMax("give string of your choice", 5, 10)
	assert.NoError(t, err)
	assert.Equal(t, valid, choice)
}

func TestInputWordDefault(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	valid := "abcde"
	input(b, "\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)

	choice, err := p.InputWordDefault("give string of your choice", valid)
	assert.NoError(t, err)
	assert.Equal(t, valid, choice)
}

func TestInputWord(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	valid := "abcde"
	input(b, valid+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)

	choice, err := p.InputWord("give string of your choice")
	assert.NoError(t, err)
	assert.Equal(t, valid, choice)
}

func TestInputStringRegex(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	valid := "abcdez"
	input(b, valid+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)

	choice, err := p.InputStringRegex("give string of your choice", "^a.*z$")
	assert.NoError(t, err)
	assert.Equal(t, valid, choice)
}

func TestInputString(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	valid := "abcdez"
	input(b, valid+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)

	choice, err := p.InputString("give string of your choice")
	assert.NoError(t, err)
	assert.Equal(t, valid, choice)
}

func TestInputStringRequired(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	valid := " d "
	input(b, valid+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)

	choice, err := p.InputStringRequired("give string of your choice")
	assert.NoError(t, err)
	assert.Equal(t, strings.TrimSpace(valid), choice)
}

func TestInputPassword(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	valid := " d "
	input(b, valid+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)

	choice, err := p.InputPassword("give password of your choice")
	assert.NoError(t, err)
	assert.Equal(t, valid, choice)
}

func TestInputPasswordRegex(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	valid := " d "
	input(b, valid+"\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)

	choice, err := p.InputPasswordRegex("give password of your choice", "^.{3,10}$")
	assert.NoError(t, err)
	assert.Equal(t, valid, choice)
}

func TestInputStringRegexDefault(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	valid := " d "
	input(b, "\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)

	choice, err := p.InputStringRegexDefault("give string of your choice", "^.{3,10}$", valid)
	assert.NoError(t, err)
	assert.Equal(t, strings.TrimSpace(valid), choice)
}

func TestInputStringDefault(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	valid := " d "
	input(b, "\r")

	mfsu := &fileutils.MockFileSystemUtils{}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)

	choice, err := p.InputStringDefault("give string of your choice", valid)
	assert.NoError(t, err)
	assert.Equal(t, strings.TrimSpace(valid), choice)
}

func TestInputExistingFilePathDefault(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	validFilePath := "test.pem"
	validFileContent := "TEST"
	input(b, "\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == validFilePath {
				data = []byte(validFileContent)
			} else {
				err = errors.New("file not found")
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == validFilePath {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)

	choice, err := p.InputExistingFilePathDefault("give file of your choice", validFilePath)

	assert.NoError(t, err)
	assert.Equal(t, validFilePath, choice)

	choiceContent, err := mfsu.ReadFile(choice)
	assert.NoError(t, err)
	assert.Equal(t, validFileContent, string(choiceContent))
}

func TestInputExistingFilePath(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	validFilePath := "test.pem"
	validFileContent := "TEST"
	input(b, validFilePath+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == validFilePath {
				data = []byte(validFileContent)
			} else {
				err = errors.New("file not found")
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == validFilePath {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)

	choice, err := p.InputExistingFilePath("give file of your choice")

	assert.NoError(t, err)
	assert.Equal(t, validFilePath, choice)

	choiceContent, err := mfsu.ReadFile(choice)
	assert.NoError(t, err)
	assert.Equal(t, validFileContent, string(choiceContent))
}

func TestInputExistingFilePathHomeDir(t *testing.T) {
	b := bytes.NewBuffer([]byte(""))
	in := io.NopCloser(b)
	validFilePath := "~/test.pem"
	validFileContent := "TEST"
	input(b, validFilePath+"\r")

	mfsu := &fileutils.MockFileSystemUtils{
		ReadFileFunc: func(filepath string) (data []byte, err error) {
			if filepath == validFilePath {
				data = []byte(validFileContent)
			} else {
				err = errors.New("file not found")
			}
			return
		},
		StatFunc: func(name string) (fi os.FileInfo, err error) {
			if name == os.Getenv("HOME")+"/test.pem" {
				fi = fileutils.MockFileInfo{IsDirFunc: func() bool { return false }}
			} else {
				err = errors.New("file not found")
			}
			return
		},
	}
	outBuf := &bytes.Buffer{}
	out := ioutils.NewNopWriteCloser(outBuf)
	p := PromptFactory(in, out, mfsu)

	choice, err := p.InputExistingFilePath("give file of your choice")

	assert.NoError(t, err)
	assert.Equal(t, validFilePath, choice)

	choiceContent, err := mfsu.ReadFile(choice)
	assert.NoError(t, err)
	assert.Equal(t, validFileContent, string(choiceContent))
}
