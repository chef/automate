import ng from 'angular';
import uiRouter from 'angular-ui-router';
import { camelCase, flatten, last, map, filter } from 'lodash';

statusController.$inject = [
  '$scope',
  '$filter',
  '$state',
  'organization',
  'project',
  'change'
];

function statusController($scope, $filter, $state, organization, project, change) {

  $scope.stateParent = 'main.enterprise.organizations.organization.project.change.status';

  $scope.getPatchsetComments = (patchset) => {
    let comments = filter($scope.change.comments, {
      patchset: patchset.sequenceNumber
    });
    let replies = flatten(map(comments, 'children'));
    return comments.concat(replies);
  };

  $scope.rerunLink = (stage) => {
    if (!stage) return;
    let link = $scope.change.links[camelCase(`trigger-${stage.stage}`)];
    if (link && link.href) return link.href;
  };

  $scope.showRerunLink = true;

  $scope.handleRerun = (url) => {
    $scope.showRerunLink = false;
    $scope.change.rerun(url).then(() => {
      $scope.showRerunLink = true;
    });
  };

  $scope.delivered = () => {
    if (($scope.change.deliveredAt.length > 0) ||
          ($scope.change.supersedingChange && $scope.change.supersedingChange.deliveredAt.length > 0)) {
      return true;
    } else {
      return false;
    }
  };

  $scope.autoProgress = true;

  $scope.disableAutoProgress = () => {
    $scope.autoProgress = false;
  };

  $scope.$watchCollection('change.stages', (stages) => {
    if ($scope.autoProgress && stages.length) {
      let latestStage = last(stages).stage;
      $state.go(`${$scope.stateParent}.${latestStage}`);
    }
  });
}

export default ng
  .module('cd.routes.change.status.controller', [
    uiRouter
  ])
  .controller('statusController', statusController)
  .name;
