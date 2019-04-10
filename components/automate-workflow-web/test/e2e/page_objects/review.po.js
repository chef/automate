export default class ReviewPage {

  get(params) {
    let path =
      `#/organizations/${params.org}` +
      `/projects/${params.project}` +
      `/changes/${params.change}` +
      `/review`;

    browser.get(path);
    browser.wait(presenceOf('.change-title'));
  }

  get commentEditorTextArea() {
    return element(by.css('textarea.ace_text-input'));
  }

  get commentForm() {
    return element(by.css('.comment-form'));
  }

  get addCommentBtn() {
    return element(by.buttonText('Add Comment'));
  }

  sendCommandCtrlEnter() {
    // Getting the platform so we can use command+enter on mac. Ctrl+enter
    // on windows/linux.
    if (process.platform == 'darwin') {
      this.commentEditorTextArea.sendKeys(
        protractor.Key.chord(
          protractor.Key.COMMAND,
          protractor.Key.ENTER)
      );
    } else {
      // Must be windows or linux so we expect ctrl+enter.
      this.commentEditorTextArea.sendKeys(
        protractor.Key.chord(
          protractor.Key.CONTROL,
          protractor.Key.ENTER)
      );
    }
  }
}
