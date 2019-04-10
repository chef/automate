import ng from 'angular';
import restmod from 'angular-restmod/dist/angular-restmod-bundle';
import ApiUrl from '../api/api_url';
import NamedModelPacker from './named_model_packer';
import Project from './project';
import Review from './review';

Organization.$inject = [
  'restmod',
  'ApiUrl'
];

function Organization(restmod, ApiUrl) {

  var def = {
    projects: { hasMany: 'Project' },
    reviews: { hasMany: 'Review' },
    PACKER: 'namedModel'
  };

  return restmod.model('/orgs', def, function () {
    this.setPrimaryKey('name');
    this.setUrlPrefix(ApiUrl('/'));
  });
}

export default ng
  .module('cd.common.models.organization', [
    'restmod',
    ApiUrl,
    NamedModelPacker,
    Project,
    Review
  ])
  .factory('Organization', Organization)
  .name;
