import ng from 'angular';
import Flash from '../../common/ui/flash/flash';
import commentFormTemplate from './comment_form.html';

commentFormDirective.$inject = ['Flash'];

function commentFormDirective(Flash) {

  function link(scope, element, attrs) {

    scope.newComment = {
      type: 'patchset',
      patchset: scope.patchset.sequenceNumber,
      content: null,
      status: 'published'
    };

    scope.onSuccess = () => {
      scope.newComment.content = null;
    };

    scope.onError = () => {
      Flash.error('Error', 'There was an error posting your comment.');
    };

    scope.addComment = (comment) => {
      scope.change.comments
        .$create(comment)
        .$promise
        .then(scope.onSuccess)
        .catch(scope.onError);
    };
  }

  return {
    link: link,
    scope: {
      change: '=',
      patchset: '='
    },
    template: commentFormTemplate
  };
}

export default ng
  .module('cd.components.commentForm', [
    Flash
  ])
  .directive('cdCommentForm', commentFormDirective)
  .name;
