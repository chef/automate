export default class JobQueuePage {

  get() {
    browser.get('#/runners/jobs');
  }

  get jobEntries() {
    return element.all(by.css('.job-queue-item'));
  }

  get jobProjects() {
    return element.all(by.binding('job.project'));
  }

  get jobChanges() {
    return element.all(by.binding('job.change'));
  }

  get jobStages() {
    return element.all(by.binding('job.stage'));
  }

  get jobPhases() {
    return element.all(by.binding('job.phase'));
  }

  get jobStatus() {
    return element.all(by.binding('job.status'));
  }

  get jobTimeInStates() {
    return element.all(by.binding('job.timeInState'));
  }

  get jobSubmittedAts() {
    return element.all(by.binding('job.submittedAt'));
  }
}
