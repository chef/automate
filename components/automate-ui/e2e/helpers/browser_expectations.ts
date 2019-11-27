import { browser, $, $$, ElementFinder } from 'protractor';
function countElementsSelected(selector, count) {
  return $$(selector).count().then(c => c === count);
}

export function expectCountElements(selector, count) {
  expect(
    browser.wait(() => countElementsSelected(selector, count),
      5000,
      `expected "${selector}" to select ${count} elements`)).toBeTruthy();
}

export function waitForElement(selector) {
  return browser.wait(() => {
    return $(selector).isPresent();
  }, 5000).then<ElementFinder>((_) => {
    return <ElementFinder>$(selector);
  });
}

export function waitForSpinners() {
  return browser.wait(() => {
    return $$('chef-loading-spinner').filter(s => s.hidden === false).count().then(c => c === 0);
  }, 5000);
}
