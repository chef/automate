package pmt

import (
	"errors"
	"fmt"
	"io"
	"os"
	"regexp"
	"regexp/syntax"
	"strconv"
	"strings"

	"github.com/chef/automate/lib/io/fileutils"
	"github.com/manifoldco/promptui"
)

const (
	FAIL_MSG                            = "prompt failed %v"
	INVALID_INT                         = "invalid int"
	PARSE_INT_FAIL                      = "parse int failed %v"
	REGEX_CHECK_FAIL                    = "regex check failed (%v) "
	REGEX_CHECK_FAIL_WITH_SAMPLE_STRING = "regex check failed. Sample Value: %v"
)
const esc = "\033["
const moveDown = esc + "1B"
const moveUp = esc + "1A"
const selectKey = "\r"
const clearLine = esc + "2K\n"

type Prompt interface {
	Confirm(question, trueOption, falseOption string) (result bool, err error)
	Select(question string, options ...string) (index int, result string, err error)
	SelectSearch(question string, options ...string) (index int, result string, err error)
	SelectAdd(question string, addLabel string, options ...string) (index int, result string, err error)
	InputInt64(label string) (resultVal int64, err error)
	InputInt(label string) (resultVal int, err error)
	InputIntDefault(label string, defVal int) (resultVal int, err error)
	InputIntDefaultValidation(label string, defVal int, valFunc func(input string) error) (resultVal int, err error)
	InputIntRange(label string, min, max int) (resultVal int, err error)
	InputIntDefaultRange(label string, defVal, min, max int) (resultVal int, err error)
	InputFloat64(label string) (resultVal float64, err error)
	InputStringMinMax(label string, minlen int, maxlen int) (result string, err error)
	InputWordDefault(label string, defaultVal string) (result string, err error)
	InputWord(label string) (result string, err error)
	InputStringRegex(label string, regexCheck string) (result string, err error)
	InputString(label string) (result string, err error)
	InputStringRequired(label string) (result string, err error)
	InputPassword(label string) (result string, err error)
	InputPasswordRegex(label string, regexCheck string) (result string, err error)
	InputStringRegexDefault(label string, regexCheck string, defaultVal string) (result string, err error)
	InputStringDefault(label string, defaultVal string) (result string, err error)
	InputExistingFilePathDefault(label string, defaultVal string) (result string, err error)
	InputExistingFilePath(label string) (result string, err error)
}

type PromptImp struct {
	Stdin     io.ReadCloser
	Stdout    io.WriteCloser
	FileUtils fileutils.FileUtils
}

func PromptFactory(in io.ReadCloser, out io.WriteCloser, fu fileutils.FileUtils) *PromptImp {
	return &PromptImp{
		Stdin:     in,
		Stdout:    out,
		FileUtils: fu,
	}
}

func (p *PromptImp) Confirm(question, trueOption, falseOption string) (resultVal bool, err error) {
	prompt := promptui.Select{
		Label:  question,
		Items:  []string{trueOption, falseOption},
		Stdin:  p.Stdin,
		Stdout: p.Stdout,
	}

	_, result, err := prompt.Run()
	if err != nil {
		err = fmt.Errorf(FAIL_MSG, err)
		return
	}

	switch result {
	case trueOption:
		return true, nil
	case falseOption:
		return false, nil
	default:
		return false, nil
	}
}

func (p *PromptImp) SelectSearch(question string, options ...string) (index int, result string, err error) {
	return p.selector(question, true, options...)
}

func (p *PromptImp) Select(question string, options ...string) (index int, result string, err error) {
	return p.selector(question, false, options...)
}

func (p *PromptImp) selector(question string, searchMode bool, options ...string) (index int, result string, err error) {
	searcher := func(input string, index int) bool {
		item := options[index]
		name := strings.Replace(strings.ToLower(item), " ", "", -1)
		input = strings.Replace(strings.ToLower(input), " ", "", -1)

		return strings.Contains(name, input)
	}
	prompt := promptui.Select{
		Label:             question,
		Items:             options,
		Stdin:             p.Stdin,
		Stdout:            p.Stdout,
		Searcher:          searcher,
		StartInSearchMode: searchMode,
	}

	return promptSelectRun(prompt, false)
}

func (p *PromptImp) SelectAdd(question string, addLabel string, options ...string) (index int, result string, err error) {
	index = -1

	for index < 0 {
		prompt := promptui.SelectWithAdd{
			Label:    question,
			Items:    options,
			AddLabel: addLabel,
		}

		index, result, err = prompt.Run()
		if index == -1 {
			options = append(options, result)
		}
	}
	if err != nil {
		err = fmt.Errorf(FAIL_MSG, err)
		return
	}
	return
}

func (p *PromptImp) InputInt64(label string) (resultVal int64, err error) {
	validate := func(input string) error {
		_, err := strconv.ParseInt(input, 10, 64)
		if err != nil {
			return errors.New("invalid int64")
		}
		return nil
	}

	prompt := promptui.Prompt{
		Label:    label,
		Validate: validate,
		Stdin:    p.Stdin,
		Stdout:   p.Stdout,
	}

	result, err := prompt.Run()

	if err != nil {
		err = fmt.Errorf(FAIL_MSG, err)
		return
	}

	resultVal, err = strconv.ParseInt(result, 10, 64)
	if err != nil {
		err = fmt.Errorf("parse int64 failed %v", err)
		return
	}
	return
}

func (p *PromptImp) InputIntRange(label string, min, max int) (resultVal int, err error) {
	validate := func(input string) error {
		val, err := strconv.Atoi(input)
		if err != nil {
			return errors.New(INVALID_INT)
		}
		if val < min || val > max {
			return fmt.Errorf("number should be in range: %v <= N <= %v", min, max)
		}
		return nil
	}

	prompt := promptui.Prompt{
		Label:    label,
		Validate: validate,
		Stdin:    p.Stdin,
		Stdout:   p.Stdout,
	}

	result, err := prompt.Run()

	if err != nil {
		err = fmt.Errorf(FAIL_MSG, err)
		return
	}

	resultVal, err = strconv.Atoi(result)
	if err != nil {
		err = fmt.Errorf(PARSE_INT_FAIL, err)
		return
	}
	return
}

func (p *PromptImp) InputIntDefaultValidation(label string, defVal int, valFunc func(input string) error) (resultVal int, err error) {
	prompt := promptui.Prompt{
		Label:    label,
		Validate: valFunc,
		Stdin:    p.Stdin,
		Stdout:   p.Stdout,
		Default:  fmt.Sprint(defVal),
	}

	result, err := prompt.Run()

	if err != nil {
		err = fmt.Errorf(FAIL_MSG, err)
		return
	}

	resultVal, err = strconv.Atoi(result)
	if err != nil {
		err = fmt.Errorf(PARSE_INT_FAIL, err)
		return
	}
	return
}

func (p *PromptImp) InputIntDefaultRange(label string, defVal, min, max int) (resultVal int, err error) {
	validate := func(input string) error {
		val, err := strconv.Atoi(input)
		if err != nil {
			return errors.New(INVALID_INT)
		}
		if val < min || val > max {
			return fmt.Errorf("number should be in range: %v <= N <= %v", min, max)
		}
		return nil
	}

	prompt := promptui.Prompt{
		Label:    label,
		Validate: validate,
		Stdin:    p.Stdin,
		Stdout:   p.Stdout,
		Default:  fmt.Sprint(defVal),
	}

	result, err := prompt.Run()

	if err != nil {
		err = fmt.Errorf(FAIL_MSG, err)
		return
	}

	resultVal, err = strconv.Atoi(result)
	if err != nil {
		err = fmt.Errorf(PARSE_INT_FAIL, err)
		return
	}
	return
}

func (p *PromptImp) InputInt(label string) (resultVal int, err error) {
	validate := func(input string) error {
		_, err := strconv.Atoi(input)
		if err != nil {
			return errors.New(INVALID_INT)
		}
		return nil
	}

	prompt := promptui.Prompt{
		Label:    label,
		Validate: validate,
		Stdin:    p.Stdin,
		Stdout:   p.Stdout,
	}

	result, err := prompt.Run()

	if err != nil {
		err = fmt.Errorf(FAIL_MSG, err)
		return
	}

	resultVal, err = strconv.Atoi(result)
	if err != nil {
		err = fmt.Errorf(PARSE_INT_FAIL, err)
		return
	}
	return
}

func (p *PromptImp) InputIntDefault(label string, defVal int) (resultVal int, err error) {
	validate := func(input string) error {
		_, err := strconv.Atoi(input)
		if err != nil {
			return errors.New(INVALID_INT)
		}
		return nil
	}

	prompt := promptui.Prompt{
		Label:    label,
		Validate: validate,
		Stdin:    p.Stdin,
		Stdout:   p.Stdout,
		Default:  fmt.Sprint(defVal),
	}

	result, err := prompt.Run()

	if err != nil {
		err = fmt.Errorf(FAIL_MSG, err)
		return
	}

	resultVal, err = strconv.Atoi(result)
	if err != nil {
		err = fmt.Errorf(PARSE_INT_FAIL, err)
		return
	}
	return
}

func (p *PromptImp) InputFloat64(label string) (resultVal float64, err error) {
	validate := func(input string) error {
		_, err := strconv.ParseFloat(input, 64)
		if err != nil {
			return errors.New("invalid float64")
		}
		return nil
	}

	prompt := promptui.Prompt{
		Label:    label,
		Validate: validate,
		Stdin:    p.Stdin,
		Stdout:   p.Stdout,
	}

	result, err := prompt.Run()

	if err != nil {
		err = fmt.Errorf(FAIL_MSG, err)
		return
	}

	resultVal, err = strconv.ParseFloat(result, 64)
	if err != nil {
		err = fmt.Errorf("parse float64 failed %v", err)
		return
	}
	return
}

func (p *PromptImp) InputWord(label string) (result string, err error) {
	return p.InputWordDefault(label, "")
}

func (p *PromptImp) InputWordDefault(label string, defaultVal string) (result string, err error) {
	validate := func(input string) error {
		isWord := true
		for _, v := range input {
			isWord = syntax.IsWordChar(v)
			if !isWord {
				return fmt.Errorf("invalid word %v", err)
			}
		}
		return nil
	}

	prompt := promptui.Prompt{
		Label:    label,
		Validate: validate,
		Default:  defaultVal,
		Stdin:    p.Stdin,
		Stdout:   p.Stdout,
	}

	return promptRun(prompt, true)
}

func (p *PromptImp) InputStringMinMax(label string, minlen int, maxlen int) (result string, err error) {
	validate := func(input string) error {
		trimInput := strings.TrimSpace(input)
		if len(trimInput) < minlen {
			return fmt.Errorf("smaller than %v", minlen)
		} else if len(trimInput) > maxlen {
			return fmt.Errorf("larger than %v", maxlen)
		}
		return nil
	}

	prompt := promptui.Prompt{
		Label:    label,
		Validate: validate,
		Stdin:    p.Stdin,
		Stdout:   p.Stdout,
	}

	return promptRun(prompt, true)
}

func (p *PromptImp) InputStringRegex(label string, regexCheck string) (result string, err error) {
	validate := func(input string) error {
		var check = regexp.MustCompile(regexCheck).MatchString
		if !check(input) {
			return fmt.Errorf(REGEX_CHECK_FAIL, regexCheck)
		}
		return nil
	}

	prompt := promptui.Prompt{
		Label:    label,
		Validate: validate,
		Stdin:    p.Stdin,
		Stdout:   p.Stdout,
	}

	return promptRun(prompt, true)
}

func (p *PromptImp) InputStringRegexDefault(label string, regexCheck string, defaultVal string) (result string, err error) {
	validate := func(input string) error {
		var check = regexp.MustCompile(regexCheck).MatchString
		if !check(input) {
			return fmt.Errorf(REGEX_CHECK_FAIL, regexCheck)
		}
		return nil
	}

	prompt := promptui.Prompt{
		Label:    label,
		Validate: validate,
		Default:  defaultVal,
		Stdin:    p.Stdin,
		Stdout:   p.Stdout,
	}

	return promptRun(prompt, true)
}

func (p *PromptImp) InputStringRequired(label string) (result string, err error) {
	validate := func(input string) error {
		trimInput := strings.TrimSpace(input)
		if len(trimInput) < 1 {
			return fmt.Errorf("this is a required field")
		}
		return nil
	}

	prompt := promptui.Prompt{
		Label:    label,
		Stdin:    p.Stdin,
		Stdout:   p.Stdout,
		Validate: validate,
	}

	return promptRun(prompt, true)
}

func promptRun(prompt promptui.Prompt, trim bool) (result string, err error) {
	result, err = prompt.Run()

	if err != nil {
		err = fmt.Errorf(FAIL_MSG, err)
		return
	}
	if trim {
		result = strings.TrimSpace(result)
	}
	return
}

func promptSelectRun(prompt promptui.Select, trim bool) (index int, result string, err error) {
	index, result, err = prompt.Run()

	if err != nil {
		err = fmt.Errorf(FAIL_MSG, err)
		return
	}
	if trim {
		result = strings.TrimSpace(result)
	}
	return
}

func (p *PromptImp) InputString(label string) (result string, err error) {
	prompt := promptui.Prompt{
		Label:  label,
		Stdin:  p.Stdin,
		Stdout: p.Stdout,
	}

	return promptRun(prompt, true)
}
func (p *PromptImp) InputPasswordRegex(label string, regexCheck string) (result string, err error) {
	validate := func(input string) error {
		var check = regexp.MustCompile(regexCheck).MatchString
		if !check(input) {
			return fmt.Errorf(REGEX_CHECK_FAIL, regexCheck)
		}
		return nil
	}

	prompt := promptui.Prompt{
		Label:    label,
		Stdin:    p.Stdin,
		Stdout:   p.Stdout,
		Mask:     '*',
		Validate: validate,
	}

	return promptRun(prompt, false)
}
func (p *PromptImp) InputPassword(label string) (result string, err error) {
	prompt := promptui.Prompt{
		Label:  label,
		Stdin:  p.Stdin,
		Stdout: p.Stdout,
		Mask:   '*',
	}

	return promptRun(prompt, false)
}

func (p *PromptImp) InputStringDefault(label string, defaultVal string) (result string, err error) {
	prompt := promptui.Prompt{
		Label:   label,
		Default: defaultVal,
		Stdin:   p.Stdin,
		Stdout:  p.Stdout,
	}

	return promptRun(prompt, true)
}

func (p *PromptImp) InputExistingFilePathDefault(label string, defaultVal string) (result string, err error) {
	prompt := promptui.Prompt{
		Label:    label,
		Validate: p.validateFilePath,
		Default:  defaultVal,
		Stdin:    p.Stdin,
		Stdout:   p.Stdout,
	}

	return promptRun(prompt, false)
}

func (p *PromptImp) validateFilePath(input string) error {
	home := os.Getenv("HOME")
	path := input
	if strings.Index(path, "~") == 0 {
		path = strings.Replace(path, "~", home, 1)
	}
	f, err := p.FileUtils.Stat(path)
	if err != nil {
		return errors.New("file does not exist")
	}
	if f.IsDir() {
		return errors.New("this is a directory")
	}
	return nil
}

func (p *PromptImp) InputExistingFilePath(label string) (result string, err error) {

	prompt := promptui.Prompt{
		Label:    label,
		Validate: p.validateFilePath,
		Stdin:    p.Stdin,
		Stdout:   p.Stdout,
	}

	return promptRun(prompt, false)
}
