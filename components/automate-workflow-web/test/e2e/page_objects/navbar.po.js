export default class Navbar {

  get() {
    return element(by.css('.navbar'));
  }

  get logo() {
    return this.get().element(by.css('.logo a'));
  }

  get workflow() {
    return this.get().element(by.linkText('Workflow'));
  }

  get nodes() {
    return this.get().element(by.linkText('Nodes'));
  }

  get admin() {
    return this.get().element(by.linkText('Admin'));
  }

  get consoleLink() {
    return this.get().element(by.css('nav ul li:nth-child(4) a'));
  }
}
