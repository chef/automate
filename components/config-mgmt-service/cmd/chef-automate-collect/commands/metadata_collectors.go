package commands

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
	"net/url"
	"os"
	fpath "path/filepath"
	"strings"

	req "github.com/chef/automate/api/external/cfgmgmt/request"
	c "github.com/chef/automate/lib/platform/command"
)

var SCMType_UNKNOWN_SCM = req.SCMType_name[int32(req.SCMType_SCM_TYPE_UNKNOWN_SCM)]
var SCMType_GIT = req.SCMType_name[int32(req.SCMType_SCM_TYPE_GIT)]
var SCMWebType_UNKNOWN_SCM_WEB = req.SCMWebType_name[int32(req.SCMWebType_SCM_WEB_TYPE_UNKNOWN_SCM_WEB)]
var SCMWebType_GITHUB = req.SCMWebType_name[int32(req.SCMWebType_SCM_WEB_TYPE_GITHUB)]

type RolloutMetadata struct {
	policyLockPath   string
	PolicyName       string `json:"policy_name"`
	PolicyNodeGroup  string `json:"policy_node_group"`
	PolicyRevisionID string `json:"policy_revision_id"`
	PolicyDomainURL  string `json:"policy_domain_url"`
	Description      string `json:"description"`
	SCMMetadata
	CiMetadata
}

type SCMMetadata struct {
	policyLockPath         string
	SCMType                string `json:"scm_type"`
	SCMWebType             string `json:"scm_web_type"`
	PolicySCMURL           string `json:"policy_scm_url"`
	PolicySCMWebURL        string `json:"policy_scm_web_url"`
	PolicySCMCommit        string `json:"policy_scm_commit"`
	PolicySCMCommitMessage string `json:"policy_scm_commit_message"`
	SCMAuthorName          string `json:"scm_author_name"`
	SCMAuthorEmail         string `json:"scm_author_email"`
}

type CiMetadata struct {
	CiJobURL string `json:"ci_job_url"`
	CiJobID  string `json:"ci_job_id"`
}

type PolicyfileLock struct {
	PolicyName       string `json:"name"`
	PolicyRevisionID string `json:"revision_id"`
}

func newRolloutMetadata(policyLockPathIn string) (*RolloutMetadata, error) {
	policyLockPath, err := expandAndValidateLockPath(policyLockPathIn)
	if err != nil {
		return nil, err
	}
	return &RolloutMetadata{policyLockPath: policyLockPath, SCMMetadata: newSCMMetadata(policyLockPath)}, nil
}

func (r *RolloutMetadata) ReadPolicyfileMetadata() error {
	var (
		err      error
		lockData PolicyfileLock
	)

	lockRawData, err := ioutil.ReadFile(r.policyLockPath)
	if err != nil {
		return err
	}

	err = json.Unmarshal(lockRawData, &lockData)
	if err != nil {
		return err
	}

	r.PolicyName = lockData.PolicyName
	r.PolicyRevisionID = lockData.PolicyRevisionID

	return nil
}

func newSCMMetadata(policyLockPath string) SCMMetadata {
	return SCMMetadata{
		policyLockPath: policyLockPath,
		SCMType:        SCMType_UNKNOWN_SCM,
		SCMWebType:     SCMWebType_UNKNOWN_SCM_WEB,
	}
}

func (s *SCMMetadata) ReadGitMetadata() error {
	gitDir := fpath.Dir(s.policyLockPath)

	g := func(args ...string) c.Opt {
		args = append([]string{"-C", gitDir}, args...)
		return c.Args(args...)
	}

	s.SCMType = "UNKNOWN_SCM"
	s.SCMWebType = SCMWebType_UNKNOWN_SCM_WEB

	// test if we are in a git directory, if we are not, exit and leave scmtype set to unknown
	_, err := c.Output("git", g("rev-parse", "--git-dir"))
	if err != nil {
		return nil
	}

	s.SCMType = SCMType_GIT

	// check if the lockfile is checked in to git. We need to handle the case
	// that it was tracked in the past but was removed; in that case, the command
	// `git rev-list -1 Policyfile.lock.json` still returns a commit.

	_, err = c.Output("git", g("ls-files", "--error-unmatch", s.policyLockPath))
	lockfileIsGitTracked := (err == nil)

	var commitOutput string

	if lockfileIsGitTracked {
		// get last commit to the lockfile:
		//   git rev-list -1 HEAD "$file"
		commitOutput, err = c.Output("git", g("rev-list", "-1", "HEAD", s.policyLockPath))
	} else {
		// last commit to the repo:
		//   git rev-list -1 HEAD
		commitOutput, err = c.Output("git", g("rev-list", "-1", "HEAD"))
	}
	if err != nil {
		return err
	}

	commit := strings.Trim(commitOutput, "\n")
	s.PolicySCMCommit = commit

	// get the commit message
	//   git show -s --format=%B "$commit"
	commitMessageOut, err := c.Output("git", g("show", "-s", "--format=%B", commit))
	if err != nil {
		return err
	}
	s.PolicySCMCommitMessage = strings.Trim(commitMessageOut, "\n")

	// get the author/email:
	//   git show -s --format=%an "$commit"
	//   git show -s --format=%ae "$commit"
	commiterNameOut, err := c.Output("git", g("show", "-s", "--format=%an", commit))
	if err != nil {
		return err
	}

	commiterEmailOut, err := c.Output("git", g("show", "-s", "--format=%ae", commit))
	if err != nil {
		return err
	}

	s.SCMAuthorName = strings.Trim(commiterNameOut, "\n")
	s.SCMAuthorEmail = strings.Trim(commiterEmailOut, "\n")

	gitRemoteName := "origin"

	if value, envVarSet := os.LookupEnv(GitRemoteNameEnvVar); envVarSet {
		cliIO.verbose("Found environment %s=%q, using this value to detect git URL", GitRemoteNameEnvVar, value)
		gitRemoteName = value
	}
	// get the URL of the remote
	//   git ls-remote --get-url $origin
	originURLOut, err := c.Output("git", g("ls-remote", "--get-url", gitRemoteName))
	if err != nil {
		return err
	}

	s.PolicySCMURL = strings.Trim(originURLOut, "\n")
	scmURL, err := url.Parse(s.PolicySCMURL)
	if err != nil && strings.Contains(s.PolicySCMURL, "@") {
		// the URL package doesn't handle the nonstandard URLs that git uses for
		// SSH (e.g., git@github.com:danielsdeleo/policyfile-jenkins-demo.git)
		// If we change it to ssh://git@github.com/$PATH then we can parse it.
		var err2 error
		maybeCanParseURL := strings.Replace(s.PolicySCMURL, ":", "/", 1)
		maybeCanParseURL = fmt.Sprintf("ssh://%s", maybeCanParseURL)
		scmURL, err2 = url.Parse(maybeCanParseURL)
		if err2 == nil {
			err = nil
		}
	}
	describeCmdFailErr(err)

	if strings.Contains(scmURL.Host, "github") {
		s.SCMWebType = SCMWebType_GITHUB
		webURL, _ := url.Parse("https://github.com")
		webURL.Path = strings.TrimSuffix(scmURL.Path, ".git")
		s.PolicySCMWebURL = webURL.String()
	}

	return nil
}

func (c *CiMetadata) ReadCIMetadata() {
	switch {
	case haveEnvVar("BUILDKITE"):
		c.ReadBuildkiteMetadata()
	case haveEnvVar("JENKINS_URL"):
		c.ReadJenkinsMetadata()
	default:
		c.ReadDefaultCiMetadata()
	}
}

func (c *CiMetadata) ReadBuildkiteMetadata() {
	c.CiJobID = fmt.Sprintf("%s#%s", os.Getenv("BUILDKITE_PIPELINE_NAME"), os.Getenv("BUILDKITE_BUILD_NUMBER"))
	c.CiJobURL = os.Getenv("BUILDKITE_BUILD_URL")
}

func (c *CiMetadata) ReadJenkinsMetadata() {
	c.CiJobID = os.Getenv("BUILD_TAG")
	c.CiJobURL = os.Getenv("BUILD_URL")
}

func (c *CiMetadata) ReadDefaultCiMetadata() {
	c.CiJobID = os.Getenv("CHEF_CI_JOB_ID")
	c.CiJobURL = os.Getenv("CHEF_CI_JOB_URL")
}

func haveEnvVar(varName string) bool {
	_, haveIt := os.LookupEnv(varName)
	return haveIt
}

func expandAndValidateLockPath(pathIn string) (string, error) {
	candidatePath, err := fpath.Abs(pathIn)
	describeCmdFailErr(err)

	_, err = os.Stat(candidatePath)
	if err != nil {
		return "", err
	}

	return candidatePath, nil
}
