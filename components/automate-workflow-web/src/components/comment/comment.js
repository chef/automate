import ng from 'angular';
import Flash from '../../common/ui/flash/flash';
import CurrentUser from '../../common/auth/current_user';
import commentTemplate from './comment.html';
import Modal from '../../common/ui/modal/modal';
import deleteConfirmationModalTemplate
  from '../../common/ui/modal/delete_anything_confirmation_modal.html';

commentDirective.$inject = ['$compile', '$location', 'Flash', 'CurrentUser', 'Modal'];

function commentDirective($compile, $location, Flash, CurrentUser, Modal) {

  function link(scope, element, attrs) {
    scope.isAuthor = (CurrentUser.user().name === scope.comment.author.name);
    scope.editingComment = false;

    scope.toggleReply = function () {
      scope.draftingReply = !scope.draftingReply;
    };

    scope.cancelReply = function () {
      scope.draftingReply = false;
      scope.reply.content = null;
    };

    scope.submitReply = function (reply) {
      scope.comment.$scope.$create(reply).$promise.then(onReplyCreated, onReplyError);
    };

    scope.submitEditedComment = (comment) => {
      comment.$save()
      .$promise
      .then((resp) => {
        Flash.notify('Success', 'You have edited your comment successfully.');
      })
      .catch((error) => {
        Flash.error(
          'Error',
          'There was an error. Your comment may have not been edited.'
        );
      });
      scope.editingComment = false;
    };

    scope.cancelEdit  = () => {
      scope.editingComment = false;
    };

    function onReplyCreated() {
      scope.draftingReply = false;
      scope.reply.content = null;
    }

    function onReplyError() {
      Flash.error('Error', 'There was an error posting your reply');
    }

    scope.onCommentLinkClick = function (event, comment) {
      event.preventDefault();
      $location.hash('comment-' + comment.id);
    };

    scope.isActive = function () {
      var activeId = parseInt($location.hash().split('comment-')[1]);
      return scope.comment.id === activeId;
    };

    scope.$watch('comment.id', function () {
      scope.reply = {
        type: 'comment',
        parent_id: scope.comment.id,
        content: null
      };

      scope.comment.url = [$location.absUrl().split('#')[0], $location.url().split('#')[0], 'comment-' + scope.comment.id].join('#');
    });

    scope.deleteComment = () => {
      scope.nameOfItem = "comment";
      scope.itemToBeDeleted = scope.comment;
      Modal.open(
        'Confirmation',
        deleteConfirmationModalTemplate,
        'red-modal',
        scope);
    };

    scope.closeModal = () => {
      delete scope.itemToBeDeleted;
      delete scope.nameOfItem;
      Modal.close();
    };

    scope.doDelete = (comment) => {
      comment.$destroy().$promise
        .then((resp) => {
          Flash.notify('Success', 'You have deleted your comment successfully.');
        })
        .catch((error) => {
          Flash.error(
            'Error',
            'There was an error. Your comment may have not been deleted successfully.'
          );
        });
      scope.closeModal();
    };

    scope.render_author_name = () => {
      if (scope.comment.author.first && scope.comment.author.last) {
        return scope.comment.author.first + " " + scope.comment.author.last;
      } else if (scope.comment.author.first) {
        return scope.comment.author.first;
      } else if (scope.comment.author.last) {
        return scope.comment.author.last;
      } else if (scope.comment.author.name) {
        return scope.comment.author.name;
      } else {
        return 'User Deleted';
      }
    };

    scope.editComment = () => {
      scope.editedComment = scope.comment;
      scope.editingComment = true;
    };

    var children = angular.element(
      '<ol class="comments">' +
        '<li class="comment" ng-repeat="comment in comment.children | orderBy:\'datetime\' track by comment.id" cd-comment="comment"></li>' +
      '</ol>'
    );
    element.append(children);
    $compile(children)(scope);
  }

  return {
    template: commentTemplate,
    scope: {
      comment: '=cdComment'
    },
    link: link
  };
}

export default ng
  .module('cd.components.comment', [
    Flash,
    CurrentUser,
    Modal
  ])
  .directive('cdComment', commentDirective)
  .name;
