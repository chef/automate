import ng from 'angular';
import restmod from 'angular-restmod/dist/angular-restmod-bundle';
import Change from './change';
import Pipeline from './pipeline';
import Session from '../auth/session';

Project.$inject = ['restmod', 'Session', '$location'];

function Project(restmod, Session, $location) {
  var def = {
    changes: { hasMany: 'Change' },
    pipelines: { hasMany: 'Pipeline' },
    cloneCmd: cloneCmd,
    setupCmd: setupCmd,
    PLURAL: 'projects'
  };

  function cloneCmd() {
    return [
      'delivery clone',
      this.name,
      '--ent=' + this.entName,
      '--org=' + this.orgName,
      '--user=' + Session.get().username,
      '--server=' + $location.host()
    ].join(' ');
  }

  function setupCmd() {
    return [
      'delivery setup',
      '--ent=' + this.entName,
      '--org=' + this.orgName,
      '--user=' + Session.get().username,
      '--server=' + $location.host(),
      '--a2-mode'
    ].join(' ');
  }

  return restmod.model(null, def, function () {
    this.setPrimaryKey('name');
  });
}

export default ng
  .module('cd.common.models.project', [
    'restmod',
    Change,
    Pipeline,
    Session
  ])
  .factory('Project', Project)
  .name;
