import scrollTo from './scroll_to';

export default function scrollToTop() {
  return scrollTo(0, 0);
}

global.scrollToTop = scrollToTop;
