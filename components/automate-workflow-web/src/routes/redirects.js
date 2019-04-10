import ng from 'angular';
import uiRouter from 'angular-ui-router';

function redirectsInitializer($rootScope, $state) {
  let stateParent = 'main.enterprise.organizations';

  $rootScope.$on('$stateChangeStart', function (event, toState, toParams) {
    // Default ../changes/:change and ../changes/:change/status
    // to ../changes/:change/status/verify
    if (toState.name === `${stateParent}.organization.project.change` ||
        toState.name === `${stateParent}.organization.project.change.status`) {
      event.preventDefault();
      $state.go(`${stateParent}.organization.project.change.status.verify`, toParams);
    }

    if (toState.name === `${stateParent}.organization.project.change.review`) {
      event.preventDefault();
      // Default ../:change/review to review overview
      // Default ../:change/review?file=:file to review file
      var goTo = !toParams.file ?
        `${stateParent}.organization.project.change.review.overview` :
        `${stateParent}.organization.project.change.review.files`;
      $state.go(goTo, toParams);
    }

    if (toState.name === 'main.enterprise') {
      event.preventDefault();
      $state.go('main.enterprise.organizations');
    }

    if (toState.name === `${stateParent}.organization.project`) {
      event.preventDefault();
      $state.go(`${stateParent}.organization.project.changes`, toParams);
    }

    if (toState.name === 'main.enterprise.runners') {
      event.preventDefault();
      $state.go('main.enterprise.runners.queue', toParams);
    }
  });
}

export default ng
  .module('cd.routes.redirects', [
    uiRouter
  ])
  .run(redirectsInitializer)
  .name;
