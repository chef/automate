# Use Cases
This is a place to capture who might use delivery and how they might interact with it from the perspective of the UI. We should feel free to add to this as needed. These use cases do not represent what the MVP for Delivery should be, but should instead demonstrate what Delivery could eventually become. These should be cases that we consider when building the product even if all of the features mentioned are not initially implemented.

## Who could use Delivery
* Operators
* Developers
* Managers
* Business Stakeholders
* Compliance Officer
* Executives
* Testers
* Shippers
* Salespeople
* Marketing people

Note: Obviously some of these roles could overlap, but I think it's worth breaking them out.

## What type of product will be built with Delivery
* Web applications
* API's
* A native application
* libraries/gems/modules

## How will Delivery be used
The most important aspect of Delivery will be it's ability to reduce cycle time on new features and fixes for our user's products. Since it requires human interaction to ship new features, notifying users of the state of their merge requests becomes imperative. The following use cases are an attempt to demonstrate how users will interact with Delivery and integrate it into their work flow. Notifications are to be considered the most verbose as possible. Users should be able to reduce or tweak the verbosity of the system to fit their needs.

### Users
* As a user I should be able to subscribe to specific pipelines that I am interested in and how I am to be alerted.
    - In my personal settings I see a list of pipelines that I have access to. I can select when to be alerted as a request travels down the pipeline. Additionally I can select how I am to be alerted: hipchat, email, text message, etc.

### Operators
* As an Operator, I want to be able to create, update and delete (CRUD) Organizations within the Enterprise.
    - Operators will need to be able to create Organizations within their Enterprise, as well as being able to edit details regarding those Orgs or delete them all together. Only a limited number of users should have this privilege.

An example of how a given enterprise could be set up in this manner might be:

Enterprise: CHEF

* Org01: CHEF Engineering
    - Project01.1: CHEF Delivery
    - Project01.2: CHEF Analytics
    - Project01.3: CHEFdk
    - Project etc…

* Org02: CHEF Marketing
    - Project02.1: Download to First Value
    - Project02.2: CHEF Web site
    - Project02.3: CHEF Marketplace
    - Project etc…

* Org03: CHEF HR
    - Project etc…
    - Project etc…
    - Project etc…


### Developers
* As a developer I should be able to submit a merge request and be notified as that request proceeds along the pipeline.
    - After completing work on my new feature I create a merge request using the delivery cli. I receive notification through hipchat that my request is currently in the verify stage. Upon successful completion of verify I receive notification through hipchat that my request is now in the review stage. When comments are made regarding my request I am notified via hipchat and take appropriate action by replying to feedback or updating my request and resubmitting. Once my request has been approved I will be notified that said request has moved into acceptance. Once accepted and shipped I will be notified that my request has shipped.

* As a developer I should be able to expedite requests.
    - After fixing a critical problem with my product I submit a request through the CLI with an expedite flag. The system moves the request to the top of the work queue and runs verify as soon as workers are available. Once verify has passed the appropriate people are notified with a high priority flag on the request. The request moves through the system bubbling to the top of all queues.

* As a reviewer I should be notified when requests are up for review or other actions are necessary.
    - When a request is submitted, and successfully passes verify, to a pipeline I am watching then I should be notified that said feature needs review. As the review process commences I will be notified when comments or changes are made to the request. When review has been satisfied I will be notified that the request has moved on to acceptance.

### Testers
* As a tester I should be notified when requests enter acceptance.
    - Once a request has been reviewed and passed all automated testing it will enter acceptance. As a tester I will be notified when requests move into acceptance. I will be presented with a list of requests that need testing. The list should include a description of the merge request and perhaps a link to the original ticket. I should be provided a binary to download and test or, in the case of a web app, a link to the acceptance environment for testing. Once I am satisfied with the changes I can sign off on the request and it will then be ready to ship.

### Shippers
* As a shipper I should be notified when new features or fixes are ready to be shipped.
    - Once a request passes through acceptance I will be notified that said feature is ready to be shipped. On visiting the shipping page I will be presented with a list of features/fixes to be shipped. I can select those features/fixes that I would like to ship and click the `Ship It!` buttons. I will be notified of the status of those requests as they proceed through Union, Rehearsal and finally Production/Delivered/Showtime.

### Managers
* As a manager I would like to be able to get an overview of all pipelines feeding into projects I am interested in.
    - I should be able to log into Delivery and see a list of all pipelines, that I have permission to see, within the system. I should be able to filter this list to show pipelines associated with a specific project. This list should include all pipelines that construct discrete components of a project, i.e. the webui and the api layer, and the pipelines that are "cared about", i.e. an internal ruby gem. I should be able to drill down into individual pipelines and see metrics specific to that pipeline, i.e. average cycle time, or view metrics aggregated over the project, i.e. the pipelines with the slowest cycle time.

* As a Manager/Operator, I want to be able to create, update and delete (CRUD) Users within an Enterprise.
    - Operators and Managers will need to be able to create Users within an Enterprise, as well as being able to edit details regarding those Users or delete them all together. They will also need to be able to do the following:  Assign Users to an Organization, Assign Users Roles / Permissions  within an Org, Invite Users to a Project

* As a Manager/Operator, I want to be able to create Users in bulk by uploading User data from a CSV or some other document.
    - Nuff said

* As a Manager/Operator/Developer, I want to be able to invite users to participate in a given Project an/or Pipeline.
    - Operators, Managers and Developers should have the ability to invite people to view / participate in Pipelines either as a contributor or an observer. 

### Sales and Marketing
* As a member of the sales or marketing team I would like to see what features are upcoming or have recently been shipped.
    - I should be able to get a list of features and fixes that have recently been shipped and also view features that on deck to be shipped.

## What will Delivery build
Examples of potential pipelines

### A ruby gem
Since gems are generally part of larger applications they tend to not require as many stages before being shipped. Therefore this should be a rather simple case for delivery.

* A developer submits a merge request for a new feature.
* The new feature passes verify and continues to review.
* Once reviewed and updates are made the request moves to acceptance.
* The request is rebased to master.
* In acceptance any integration or smoke tests are ran and then the features moves to the ship it queue.
* A shipper updates the version and then clicks the `Ship It!` button.
* Delivery uploads the new version of the gem to a gem repository.

Note: In this case Union and Rehearsal are a no-op.

### A native application
Native applications will generally provide the end user with some sort of binary that needs to be downloaded and installed on the users machine.

* A developer submits a merge requests for a new feature.
* The new feature passes verify and continues to review.
* Once reviewed and updates are made the request moves to acceptance.
* The request is rebased to master.
* In acceptance integration tests are ran for each configured environment and binaries are produced.
* Once the first stage of acceptance has completed successfully testers are notified.
* Testers will go to the acceptance page and download whatever binaries to be tested.
* Once the testers have completed their verification they sign off on the new feature.
* When a feature has been signed off on a shipper will update the version and then click the `Ship It!` button.
* The new feature will then go through Union and Rehearsal to confirm everything is still good.
* The new set of binaries will then be uploaded for end users to download.

### A Web Application
We applications are often complex application that sometimes require updates to multiple layers in order to ship.

* A developer completes a major feature that facilitates updates in the UI, API and the environment.
* The developer submits a merge request to each pipeline; UI, API and environment.
* The UI request is marked as dependant on the API and environment request.
* The API request is marked as dependant on the environment request.
* Each request separately goes though verify and review.
* Once all dependant requests are reviewed the set moves into acceptance.
* Environment changes are rebased on master.
* An instance of the acceptance environment is updated with the environment changes.
* The API is rebased to master and installed.
* The UI is rebased to master and installed.
* Integration tests are ran.
* Testers are notified.
* Once UAT is complete and the `Ship It!` button has been clicked the new feature moves to Union then Rehearsal and finally Production/Delivered/Showtime
