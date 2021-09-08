---
name: Upgrade Story
about: Explain the User story, regarding Dependency Upgrade related changes.
title: 'Upgrade Story'
labels: upgrade-story, user-story
assignees: ''

---

<!-- /!\ Please ensure that you are NOT disclosing any customer information without their consent /!\ -->

## User Story
As a System Manager for Chef Automate,
I want Chef Automate to use this version of this Software.
Because for these Security issues found on old version.
Or Because of these new features which are supported in new version of this Software.

## Software to Upgrade
- name of software
    - Current Version: x.y.z
    - New Version: x1.y1.z1

## Implementation Details
- In this file, modify the version.
- Change this function, as the new version has new function for this. 

## Acceptance Criteria
- These features are running fine after the upgrade.

## Definition of Done
- All things specified in User Story Acceptance Criteria should be fulfilled. 
- All Exceptions are Handled Properly 
- Ensure logs have no unnecessary data. 
- Test coverage for the new feature is done to at least 70% 
- Docs changes PR is Raised. 
- Swagger Documentation updated 
- Smoke Test done. 
- Ensure Build and Integration Pipelines are Green. 
- PR has 2 approvers. 
- All Code Review Comments are Resolved. 
- Test upgrade from n-1 version.