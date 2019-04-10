import ng from 'angular';
import anchorScroll from './anchor_scroll/anchor_scroll';
import animate from './animate/animate';
import breadcrumb from './breadcrumb/breadcrumb';
import flash from './flash/flash';
import modal from './modal/modal';
import pageTitle from './page_title/page_title';

export default ng
  .module('cd.common.ui', [
    anchorScroll,
    animate,
    breadcrumb,
    flash,
    modal,
    pageTitle
  ])
  .name;
