import ng from 'angular';
import searchesTemplate from './searches.html';
import Flash from '../../common/ui/flash/flash';

searchesComponent.$inject = ['Flash', '$http', 'ApiUrl'];

function searchesComponent(Flash, $http, ApiUrl) {
  function link (scope) {
    scope.submit = function() {
      $http.put(ApiUrl('/searches'), scope.searches)
        .then((resp) => {
          Flash.notify('Success', 'Your search was saved successfully.');
        }, (resp) => {
          Flash.error('Error', 'There was a problem saving your search.');
        });
    };
  }

  return {
    link: link,
    template: searchesTemplate
  };
}

export default ng
  .module('cd.routes.searches.component', [
    Flash
  ])
  .directive('cdSearches', searchesComponent)
  .name;
