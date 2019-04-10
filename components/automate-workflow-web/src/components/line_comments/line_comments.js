import ng from 'angular';
import ngScroll from 'angular-scroll';
import Flash from '../../common/ui/flash/flash';
import lineCommentsTemplate from './line_comments.html';

lineCommentsComponent.$inject = ['Flash'];

function lineCommentsComponent(Flash) {

  function link(scope, element, attrs) {

    scope.$watch('activeRange[0]', function () {
      // Scroll to the top of the comment area when we switch active ranges
      element.scrollTo(0,0);

      scope.newComment = {
        type: 'line',
        patchset: scope.patchset.sequenceNumber,
        file: scope.activeFile[1],
        line_range: scope.activeRange,
        content: null
      };
    });

    scope.addComment = function (comment) {
      scope.change.comments.$create(comment).$promise.then(onCommentCreated, onCommentError);
    };

    function onCommentCreated() {
      scope.newComment.content = null;
    }

    function onCommentError() {
      Flash.error('Error', 'There was an error posting your comment.');
    }
  }

  return {
    template: lineCommentsTemplate,
    link: link
  };
}

export default ng
  .module('cd.components.lineComments', [
    'duScroll',
    Flash
  ])
  .directive('cdLineComments', lineCommentsComponent)
  .name;
