export default class ReviewFileDiffPage {

  get(params) {
    let path =
      `#/organizations/${params.org}` +
      `/projects/${params.project}` +
      `/changes/${params.change}` +
      `/review` +
      `?file=${params.file}` +
      `&start=${params.start}` +
      `&end=${params.end}`;

    browser.get(path);
    browser.wait(presenceOf('.change-title'));
  }

  get backBtn() {
    return element(by.css('.back-btn'));
  }
}
