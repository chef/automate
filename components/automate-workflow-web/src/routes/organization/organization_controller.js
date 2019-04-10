import ng from 'angular';
import Breadcrumb from '../../common/ui/breadcrumb/breadcrumb';
import Projects from '../../common/projects/projects';

organizationController.$inject = [
  '$location',
  '$scope',
  '$state',
  'Breadcrumb',
  'Projects',
  'organization',
  'projects',
  'addproject',
  'scmProviders',
  'Flash',
  'ApiUrl',
  '$http'
];

function organizationController($location, $scope, $state,
                                Breadcrumb, Projects, organization, projects,
                                addproject, scmProviders, Flash,
                                ApiUrl, $http) {

  Breadcrumb.setCrumb([{name: organization.name}]);

  $scope.org = organization;
  $scope.projects = projects;
  $scope.scmProviders = scmProviders;
  $scope.reverse = false;

  $scope.sortReverse = function(reverse) {
    $scope.reverse = reverse;
  };

  $scope.$on('org-toggleEditOrgForm', function() {
    $scope.showEditForm = !$scope.showEditForm;
  });

  $scope.saveOrg = function (org) {
    org.$save();
    $scope.showEditForm = false;
  };

  $scope.cancelEdit = function () {
    $scope.showEditForm = false;
  };

  if(addproject) {
    $scope.showNewProjectForm = true;
    $location.search({});
  }

  $scope.newProject = {};

  $scope.toggleNewProjectForm = function () {
    $scope.showNewProjectForm = !$scope.showNewProjectForm;
  };

  $scope.saveAndClose = function (newProject, webhook) {
    Projects.create(newProject).then(() => {
      $scope.createWebhook(webhook, newProject.name);
      $scope.projects.$refresh();
      $scope.cancel();

      let params = { org: $scope.org.name, project: newProject.name };
      let options = { reload: true };
      $state.go('main.enterprise.organizations.organization.project.pipelines', params, options);
    })
    .catch((resp) => {
      Flash.error('Error', resp.data.message);
    });
  };

  $scope.createWebhook = function (webhook, projectName) {
    var org = $scope.org;
    var createUrl = ApiUrl(`/orgs/${org.name}/projects/${projectName}/notifications/slack-webhook`);

    if (webhook.name && webhook.url) {
      $http.put(createUrl, webhook, {'ignoreApiInterceptor':true})
      .catch((resp) => {
        Flash.error(
          'Error',
          'Your new project was created, but there was a problem saving the Slack webhook.'
        );
      });
    }
  };

  $scope.cancel = function () {
    $scope.newProject = null;
    $scope.showNewProjectForm = false;
    $scope.testResult = '';
  };

  $scope.testWebhook = function (webhook) {
    $http.put(ApiUrl('/notifications/slack-webhook/test'), {
      url: webhook.url
    })
      .then(function() {
        $scope.testResult = 'success';
      }, function(response) {
        $scope.testResult = (response.status === 504 ? 'error-504' : 'error-any');
      });
  };
}

export default ng
  .module('cd.routes.organization.controller', [
    Breadcrumb,
    Projects
  ])
  .controller('organizationController', organizationController)
  .name;
