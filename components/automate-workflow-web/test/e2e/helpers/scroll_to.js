export default function scrollTo(x, y) {
  let script = `return window.scrollTo(${x},${y});`;
  return browser.executeScript(script);
}

global.scrollTo = scrollTo;
