import ng from 'angular';
import uiRouter from 'angular-ui-router';
import organizationController from './organization_controller';
import organizationComponent from './organization_component';
import breadcrumbTemplate from './breadcrumb.html';
import Organization from '../../common/models/organization';
import ApiUrl from '../../common/api/api_url';

export default ng
  .module('cd.routes.organization', [
    uiRouter,
    ApiUrl,
    Organization,
    organizationController,
    organizationComponent
  ])
  .config(($stateProvider) => {
    $stateProvider
      .state('main.enterprise.organizations.organization', {
        url: '/:org?addproject',
        reloadOnSearch: false,
        views: {
          'tabbed@main.enterprise': {
            template: '<cd-organization>',
            controller: 'organizationController'
          }
        },
        resolve: {
          organization: ['$stateParams', 'Organization',
            function ($stateParams, Organization) {
              return Organization.$find($stateParams.org).$promise;
            }
          ],
          projects: ['organization', function (organization) {
            return organization.projects.$refresh().$promise;
          }],
          addproject: ['$stateParams', 'Organization',
            function ($stateParams, Organization) {
              return $stateParams.addproject === true ? true : false;
            }
          ],
          scmProviders: ['$http', '$q', 'ApiUrl', 'organization', function ($http, $q, ApiUrl, organization) {
            let orgPath = `/orgs/${organization.name}`;

            return $http
              .get(ApiUrl(`/scm-providers`))
              .then((resp) => {
                return resp.data.map((provider) => {
                  let uri = ApiUrl(`${orgPath}${provider.projectCreateUri}`);
                  provider.projectCreateUri = uri;
                  return provider;
                });
              })
              .catch((resp) => []);
          }]
        }
      });
  })
  .name;
