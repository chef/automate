import ng from 'angular';
import { mapKeys, snakeCase } from 'lodash';

Projects.$inject = [
  '$http'
];

function Projects($http) {
  return {
    create(params) {
      return $http.post(params.scm.projectCreateUri, params);
    },

    update(project, params) {
      // This snake_cases the keys in params.scm because restmod has an interesting
      // world view that everything should be camelCase. This makes our service happy.
      let snakeParams = Object.assign({}, params, { scm: mapKeys(params.scm, (value, key) => snakeCase(key))});
      return $http.put(project.$url(), snakeParams);
    }
  };
}

export default ng
  .module('common.projects', [])
  .factory('Projects', Projects)
  .name;
