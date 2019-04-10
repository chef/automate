import ng from 'angular';
import uiRouter from 'angular-ui-router';

anchorScrollConfig.$inject = ['$anchorScrollProvider', '$uiViewScrollProvider'];

function anchorScrollConfig($anchorScrollProvider, $uiViewScrollProvider) {
  $anchorScrollProvider.disableAutoScrolling();
  $uiViewScrollProvider.useAnchorScroll();
}

export default ng
  .module('common.ui.anchorScroll', [
    uiRouter
  ])
  .config(anchorScrollConfig)
  .name;
