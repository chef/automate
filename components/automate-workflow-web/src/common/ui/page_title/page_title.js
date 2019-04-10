import ng from 'angular';
import pageTitleService from './page_title_service';
import pageTitleInitializer from './page_title_initializer';

export default ng
  .module('cd.common.ui.pageTitle', [
    pageTitleService,
    pageTitleInitializer
  ])
  .name;
