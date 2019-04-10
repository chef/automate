import ng from 'angular';
import uiRouter from 'angular-ui-router';
import { filter, includes, sortBy } from 'lodash';
import ApiUrl from '../../common/api/api_url';

quickFindController.$inject = [
  '$http',
  '$state',
  'ApiUrl'
];

function quickFindController($http, $state, ApiUrl) {
  this.getResults = (term = '') => {
    if (typeof term !== 'string') {
      return;
    }

    return $http
      .get(ApiUrl('/pipelines'), { cache: true })
      .then((resp) => {

        let results = filter(resp.data, (pipeline) => {
          return includes(pipeline.project.toLowerCase(), term.toLowerCase());
        });

        return sortBy(results, 'project');
      });
  };

  this.getFormattedItem = (item) => {
    return `<span class="org-name">${item.org}</span>
            <span class="separator"></span>
            <span class="project-name">${item.project}</span>`;
  };

  this.selectResult = (result) => {
    let { org, project } = result;
    let params = { org: org, project: project };
    let options = { reload: true };

    $state.go('main.enterprise.organizations.organization.project', params, options);
  };
}

export default ng
  .module('cd.components.quickFind.controller', [
    uiRouter,
    ApiUrl
  ])
  .controller('quickFindController', quickFindController)
  .name;
