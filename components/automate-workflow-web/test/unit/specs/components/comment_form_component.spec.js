import ng from 'angular';
import 'angular-mocks';
import commentFormComponent
  from '../../../../src/components/comment_form/comment_form';
import Change from '../../../../src/common/models/change';
import Comment from '../../../../src/common/models/comment';

describe('commentFormComponent', () => {
  let scope, element, isolateScope;

  beforeEach(ng.mock.module(Change));
  beforeEach(ng.mock.module(Comment));
  beforeEach(ng.mock.module(commentFormComponent));

  function createDirective(Change, Comment, $compile, $rootScope) {
    scope = $rootScope.$new();
    scope.change = Change.$buildRaw({ id: 1 });
    scope.change.comments = Comment.$collection();
    scope.patchset = { sequenceNumber: 1 };

    element = $compile(ng.element('<cd-comment-form data-change="change" data-patchset="patchset" />'))(scope);
    isolateScope = element.isolateScope();
  }

  beforeEach(inject(createDirective));

  describe('when a comment is submitted', () => {
    let flash, comment;

    let mocks = {
      success: {
        $promise: {
          then: () => {
            isolateScope.onSuccess();
            return {
              'catch': () => {}
            }
          }
        }
      },
      error: {
        $promise: {
          then: () => {
            return {
              'catch': isolateScope.onError
            }
          }
        }
      }
    }

    beforeEach(inject((Flash) => {
      Flash.register(() => {});
      
      spyOn(Flash, 'error');
      spyOn(Flash, 'notify');

      flash = Flash;
      comment = isolateScope.newComment;
    }))

    describe('and the comment is invalid', () => {

      it('messages the user', () => {
        spyOn(scope.change.comments, '$create').and.returnValue(mocks.error);

        comment.content = '';
        isolateScope.addComment(comment);

        expect(flash.notify).not.toHaveBeenCalled();
        expect(flash.error).toHaveBeenCalled();
      }); 
    });

    describe('and the comment is valid', () => {

      it('does not message the user', () => {
        spyOn(scope.change.comments, '$create').and.returnValue(mocks.success);

        comment.content = ':shipit:';
        isolateScope.addComment(comment);

        expect(flash.notify).not.toHaveBeenCalled();
        expect(flash.error).not.toHaveBeenCalled();
      });      
    });
  });
});
