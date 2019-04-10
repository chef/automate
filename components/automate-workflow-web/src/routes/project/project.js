import ng from 'angular';
import uiRouter from 'angular-ui-router';
import projectController from './project_controller';
import projectComponent from './project_component';
import breadcrumbTemplate from './breadcrumb.html';
import changes from './changes/changes';
import pipelines from './pipelines/pipelines';
import dependencies from './dependencies/dependencies';
import consumers from './consumers/consumers';
import clone from './clone/clone';
import { includes } from 'lodash';

export default ng
  .module('cd.routes.project', [
    uiRouter,
    changes,
    pipelines,
    dependencies,
    consumers,
    clone,
    projectController,
    projectComponent
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.enterprise.organizations.organization.project', {
        url: '/projects/:project?state',
        views: {
          'tabbed@main.enterprise': {
            template: '<cd-project>',
            controller: 'projectController',
          }
        },
        resolve: {
          project: ['$stateParams', 'projects',
            function ($stateParams, projects) {
              return projects.$find($stateParams.project).$promise;
            }
          ],
          changes: ['$stateParams', 'project',
            function ($stateParams, project) {
              return project.changes.$refresh({ state: $stateParams.state || 'all' }).$promise;
            }
          ],
          pipelines: ['project', function (project) {
            return project.pipelines.$refresh().$promise;
          }],
          dependencies: ['$http', 'ApiUrl', 'organization', 'project', function ($http, ApiUrl, organization, project) {
            let depsUrl = `/orgs/${organization.name}/projects/${project.name}/dependencies`;
            return $http
              .get(ApiUrl(depsUrl))
              .then((resp) => resp.data)
              .catch((resp) => {});
          }],
          notifications: ['$http', 'ApiUrl', function ($http, ApiUrl) {
            return $http
              .get(ApiUrl('/notifications/'))
              .then((resp) => resp.data.notifications)
              .catch(() => []);
          }],
          watchSettings: ['$http', 'ApiUrl', 'organization', 'project', 'notifications', ($http, ApiUrl, organization, project, notifications) => {
            let configured = includes(notifications, 'smtp');

            return $http
              .get(ApiUrl(`/orgs/${organization.name}/projects/${project.name}/notifications/watch`))
              .then((resp) => {
                return {
                  watchable: configured,
                  categories: resp.data.categories || []
                };
              })
              .catch((resp) => {
                return {
                  watchable: configured,
                  categories: []
                };
              });
          }]
        }
      });
  })
  .name;
