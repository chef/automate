import ng from 'angular';
import { reduce, filter } from 'lodash';
import diffFilesTemplate from './files.html';

diffFilesComponent.$inject = ['$compile'];

function diffFilesComponent($compile) {

  function link(scope, element, attrs) {

    scope.getFileComments = (file) => {
      return filter(scope.patchsetComments, { type: 'line', file: file[1] });
    };
  }

  return {
    link: link,
    template: diffFilesTemplate
  };
}

export default ng
  .module('cd.components.diff.files', [])
  .directive('cdDiffFiles', diffFilesComponent)
  .name;
