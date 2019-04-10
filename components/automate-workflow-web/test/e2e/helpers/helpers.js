import getScrollPosition from './get_scroll_position';
import login from './login';
import logout from './logout';
import scrollTo from './scroll_to';
import scrollToBottom from './scroll_to_bottom';
import scrollToTop from './scroll_to_top';
import featureFlag from './feature_flag';
import ec from './expected_conditions';

export default {
  getScrollPosition: getScrollPosition,
  login: login,
  logout: logout,
  scrollTo: scrollTo,
  scrollToBottom: scrollToBottom,
  scrollToTop: scrollToTop,
  featureFlag: featureFlag,
  presenceOf: ec.presenceOf,
  urlContains: ec.urlContains
};
