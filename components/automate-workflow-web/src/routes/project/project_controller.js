import ng from 'angular';
import { find, first, includes, last, merge, reduce, remove } from 'lodash';
import Store from '../../common/store/store';
import Modal from '../../common/ui/modal/modal';
import ApiUrl from '../../common/api/api_url';
import Session from '../../common/auth/session';

projectController.$inject = [
  '$scope',
  '$window',
  '$location',
  'organization',
  'project',
  'changes',
  'dependencies',
  'watchSettings',
  'Modal',
  'Store',
  '$http',
  'ApiUrl'
];

function projectController(
  $scope, $window, $location, organization, project, changes, interdeps,
  watchSettings, Modal, Store, $http, ApiUrl) {

  $scope.now = new Date();
  $scope.organization = organization;
  $scope.project = project;
  $scope.changes = changes;
  $scope.interdeps = interdeps;
  $scope.changeParams = {};
  $scope.showWatchMenu = false;
  $scope.watchSettings = watchSettings;
  applyWatchSettings();

  $scope.watchableCategories = [
    {
      name: 'review',
      description: 'Verify stage passed, change approved, comment added to change.'
    },
    {
      name: 'deliver',
      description: 'Acceptance stage passed, change delivered.'
    },
    {
      name: 'observe',
      description: 'Any failures in Build, Acceptance, Union, Rehearsal, and Delivered stages. Successful completion of Delivered stage.'
    }
  ];

  $scope.newPipeline = {
    base: "master"
  };

  $scope.stateFilters = [
    { value: 'all', text: 'All' },
    { value: 'open', text: 'Open' },
    { value: 'merged', text: 'Merged' }
  ];

  $scope.currentFilter = find($scope.stateFilters, {
    value: $location.search().state || first($scope.stateFilters).value
  });

  $scope.$watch(() => $location.search().state, (newState, prevState) => {
    if (newState !== prevState) {
      Store.put('currentFilter', newState);
    }
  });

  $scope.loadMoreChanges = () => {
    let params = $scope.changeParams;

    if (changes.length) {
      params = merge({ id: last(changes).id }, params);
    }

    changes.$fetch(params);
  };

  $scope.depCount = (pipelines) => {
    return reduce(pipelines, (count, deps) => {
      return count + deps.length;
    }, 0);
  };

  $scope.filterByState = function(state) {
    $location.search('state', state.value);
  };

  $scope.toggleNewPipelineForm = function () {
    $scope.showNewPipelineForm = !$scope.showNewPipelineForm;
  };

  $scope.closeNewPipelineForm = function() {
    $scope.newPipeline.name = null;
    $scope.showNewPipelineForm = false;
  };

  $scope.savePipeline = function (newPipeline) {
    $scope.project.pipelines.$create(newPipeline).$then(function() {
      delete $scope.newPipeline.name;
      $scope.closeNewPipelineForm();
    });
  };

  $scope.deletePipeline = function (pipeline) {
    if ($window.confirm('Are you sure you want to delete this pipeline? This operation cannot be undone.')) {
      pipeline.$destroy();
    }
  };

  $scope.clonesFromDelivery = (scmType) => {
    // view, once we get the git_url 
    let clonesFromDelivery = ['local', 'bitbucket', 'githubV2'];
    return includes(clonesFromDelivery, scmType);
  };

  $scope.toggleWatchMenu = () => {
    $scope.showWatchMenu = !$scope.showWatchMenu;
  };

  $scope.toggleCategory = (toggledCategory) => {

    if (includes($scope.watchSettings.categories, toggledCategory)) {
      remove($scope.watchSettings.categories, (category) => {
        return toggledCategory === category;
      });
    } else {
      $scope.watchSettings.categories.push(toggledCategory);
    }

    $http.put(ApiUrl(`/orgs/${$scope.organization.name}/projects/${$scope.project.name}/notifications/watch`),
      {
        categories: $scope.watchSettings.categories
      })
      .then(() => {
        applyWatchSettings();
      });
  };

  function applyWatchSettings() {

    let settings = $scope.watchSettings,
      icon = 'eyeball-off',
      label = 'Watch Project',
      title = 'Watch this project to be notified of changes by email',
      disabled = false;

    if (!settings.watchable) {
      title = 'An admin has not yet configured email for your enterprise';
      disabled = true;
    }
    else {
      if ($scope.watchSettings.categories.length > 0) {
        icon = 'eyeball';
        title = 'Change your watch settings for this project';
      }
    }

    $scope.watchButton = {
      icon: icon,
      label: label,
      title: title,
      disabled: disabled
    };
  }
}

export default ng
  .module('cd.routes.project.controller', [
    Store,
    Modal
  ])
  .controller('projectController', projectController)
  .name;
