import ng from 'angular';
import { filter } from 'lodash';
import fileCommentsTemplate from './file_comments.html';

function fileCommentsComponent() {

  function link(scope, element, attrs) {

    scope.toggleComments = function () {
      scope.showComments = !scope.showComments;
    };

    scope.getFileComments = function (patchset, file) {
      return filter(scope.patchsetComments, {
        type: 'line',
        patchset: patchset.sequenceNumber,
        file: file[1]
      });
    };
  }

  return {
    template: fileCommentsTemplate,
    link: link
  };
}

export default ng
  .module('cd.components.fileComments', [])
  .directive('cdFileComments', fileCommentsComponent)
  .name;
