export default function getScrollPosition() {
  let script = `
    return {
      x: window.scrollX,
      y: window.scrollY
    };
  `;
  return browser.executeScript(script);
}

global.getScrollPosition = getScrollPosition;
