import ng from 'angular';
import pageTitle from './page_title_service';

pageTitleInitializer.$inject = [
  '$rootScope',
  'pageTitle'
];

function pageTitleInitializer($rootScope, pageTitle) {
  $rootScope.$on('$stateChangeSuccess', function () {
    let title = 'Chef Automate';
    pageTitle.set(title);
  });
}

export default ng
  .module('cd.common.ui.pageTitle.initializer', [
    pageTitle
  ])
  .run(pageTitleInitializer)
  .name;
