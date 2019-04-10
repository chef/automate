import scrollTo from './scroll_to';

export default function scrollToBottom() {
  return browser
    .executeScript('return window.innerHeight;')
    .then((height) => scrollTo(0, height));
}

global.scrollToBottom = scrollToBottom;
