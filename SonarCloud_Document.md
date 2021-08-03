# Sonarcloud Document


## SonarCloud
We have a dedicated static Analysis tool.
SonarCloud is a cloud-based code analysis service designed to detect code quality issues in different programming languages, 
continuously ensuring the maintainability, reliability and security of your code. 
Early detection of problems ensures that fewer issues get through to the later stages of the process and ultimately helps to 
increase the overall quality of your production code. 

## Setting up Sonarcloud

### Set up your organization

In this step, you will set up the SonarCloud organization that with to your GitHub organization or personal account.

SonarCloud will suggest a key for your SonarCloud organization. This is a name unique across all organizations
within SonarCloud. 
Add this Sonar key to your GitHub Repository which you want to analyze.
Go to Settings -> Secret-> Add Sonar token.

**Note:**  Chef Software, Inc Organization is already present and automate repository is added under this organization.

### Connecting to SonarCloud.

Connect your GitHub organization with SonarCloud with GitHub Credentials.
When prompted, install the SonarCloud application on GitHub. This step allows SonarCloud to access your GitHub
organization or personal account.
You can select specific repositories to be connected to SonarCloud or just select all. You can always change 
this setting later.

### Configuring Automate with SonarCloud.

We have configured sonarcloud with automate with GitHub Action method.
First, you need to sign in into sonarcloud with our existing GitHub account on the repository service that hosts the code you
want to analyze.  
Once you have successfully logged in, you will see the SonarCloud welcome screen. Click on Import projects from GitHub.

On the SonarCloud Dashboard, Click on + option at the right-hand side to select  project which you want to analyze.

### Configuring Analysis with SonarCloud
 
It is recommended to Configure analysis with GitHub Action method and follow the steps recommended in SonarCloud Platform.
 
After following all the steps, Analysis will start on the dashboard in fwe minutes.
After any merge in the master, SonarCloud will run the analysis, It will analyze master branch and the open PR's.
 
### File Exclusion.
 
 For exclusion some files to get not get scanned during analysis, We have to set file exclusion property in 
 Sonar-project. properties file.
 Files which we have already excluded.
 
 sonar.exclusions=**/*.pb.*.go, **/*.pb.go, **/test*/**, .gitignore, .git/**, .semgrep/**, **/*.bindata.go, **/*.spec.ts, dev-docs/**, .buildkite/*, .expeditor/**, .studio/*, benchmarks/*, bin/*, cache/*, dev/*, ec2/*, protovendor/*, results/*, scripts/*, terraform/*, tools/*, **/*.sql, **/*.docs-chef-io, **/*.md

 
## Coverage Report 
### Generating Coverage Report
You need to have all the prerequisites installed like Go, NPM, Node

- Run `./tools/coverage_report`

### Displaying Coverage Report
To display code coverage report we have to set Coverage report property in  Sonar-project. properties file.
 
We have set for go and JavaScript files 
 
sonar.go.coverage.reportPaths=Coverage_report/cover.out
sonar.javascript.lcov.reportPaths=Coverage_report/lcov.info

We have to keep the report files in Coverage_report folder at root directory at set the path accordingly in Sonar-project.properties files.

https://docs.sonarqube.org/latest/analysis/coverage/

 
Code coverage report will be displayed in SonarCloud dashboard.
It will show one report for both the files. 
  
 
## Sonarcloud Quality Gate.

By default, Sonarcloud quality gate is set as Sonarway. So, Sonarcloud will analyze master branch and PR's according to the properties set in Sonarway quality gate.
You can set your own quality gate according to your requirements if you want to fail or pass analysis on branch or PR's
Steps: 
- Go to https://sonarcloud.io/organizations/chef/projects 
- Click on Quality gate. 
- Click on Create option. 
- Add Conditions ( Coverage, Duplications)
- Make it as by default Quality Gate.

## Quality profiles

Quality Profiles are collections of rules to apply during an analysis.
For each language there is a default profile. These Quality Profiles are designed by SonarSource with 
rules that are generally applicable for most projects.
Sonarway is by default Quality profile, you can  extend these profiles according to your rule-sets.


