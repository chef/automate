import ng from 'angular';
import 'angular-mocks';
import commentComponent from '../../../../src/components/comment/comment';
import deleteConfirmationModalTemplate
  from '../../../../src/common/ui/modal/delete_anything_confirmation_modal.html'

describe('commentComponent', () => {
  let scope, element, isolateScope;

  beforeEach(ng.mock.module(commentComponent, ($provide) => {
    let currentUser = { first: 'rainbow', last: 'dash', name: 'rainbowDash' };
    $provide.value('CurrentUser', { user: () => currentUser });
  }));

  function createDirective(author) {

    return inject(($compile, $rootScope) => {
      scope = $rootScope.$new();
      scope.comment = { id: 1,
                        author: author,
                        datetime: 'Wed Sep 14 2016',
                        url: 'https://www.acomment.com',
                        content: 'I have some content'
                      };


      element = $compile(ng.element('<div cd-comment=\'comment\'></div>'))(scope);
      isolateScope = element.isolateScope();
    });
  }

  describe('on initialization', () => {

    describe('scope.isAuthor', () => {
      it('returns true if the session user is the comment user', () => {
        createDirective({first: 'Sgt', last: 'Hatred', name: 'rainbowDash'});
        expect(isolateScope.isAuthor).toBe(true);
      });

      it('returns false if the session user is not the comment user', () => {
        createDirective({first: 'Sgt', last: null, name: 'Courtney Robert Haine'});
        expect(isolateScope.isAuthor).toBe(false);
      });
    });

    it('sets editingComment to false', () => {
      createDirective({first: 'Sgt', last: null, name: 'Courtney Robert Haine'});
      expect(isolateScope.editingComment).toBe(false);
    });
  });

  describe('render_author_name', () => {

    it('returns the user\'s first and last name when there is a first and last name', () => {
      createDirective({first: 'Sgt', last: 'Hatred', name: 'Courtney Robert Haine'});
      expect(isolateScope.render_author_name()).toEqual('Sgt Hatred');
    });

    it('returns the user\'s first name when there is just a first name', () => {
      createDirective({first: 'Sgt', last: null, name: 'Courtney Robert Haine'});
      expect(isolateScope.render_author_name()).toEqual('Sgt');
    });

    it('returns the user\'s last name when there is just a last name', () => {
      createDirective({first: null, last: 'Hatred', name: 'Courtney Robert Haine'});
      expect(isolateScope.render_author_name()).toEqual('Hatred');
    });

    it('returns the user\'s name when there is just a name', () => {
      createDirective({first: null, last: null, name: 'Courtney Robert Haine'});
      expect(isolateScope.render_author_name()).toEqual('Courtney Robert Haine');
    });

    it('returns a default when the user doesn\'t have a name', () => {
      createDirective({first: null, last: null, name: null});
      expect(isolateScope.render_author_name()).toEqual('User Deleted');
    });
  });

  describe('editComment', () => {
    it('sets editingComment to true', () => {
      createDirective({first: null, last: null, name: 'Courtney Robert Haine'});
      isolateScope.editComment();
      expect(isolateScope.editingComment).toBe(true);
    });
  });

  describe('cancelEdit', () => {
    it('sets editingComment to false', () => {
      createDirective({first: null, last: null, name: 'Courtney Robert Haine'});
      isolateScope.editComment();
      isolateScope.cancelEdit();
      expect(isolateScope.editingComment).toBe(false);
    });
  });

  describe('submitEditedComment', () => {
    let comment, deferred;

    beforeEach(inject(($q) => {
      deferred = $q.defer();
      deferred.$promise = deferred.promise
      comment = { $save: function () { return deferred; } };
      createDirective({first: 'Sgt', last: 'Hatred', name: 'rainbowDash'});
    }));

    it('sets scope.editingComment to false', () => {
      isolateScope.submitEditedComment(comment);
      expect(isolateScope.editingComment).toBe(false);
    });

    describe('on success', () => {

      it('gives a flash success message', inject((Flash) => {
        spyOn(Flash, 'notify');

        isolateScope.submitEditedComment(comment);
        deferred.resolve();
        isolateScope.$digest();
        expect(Flash.notify).toHaveBeenCalledWith('Success', 'You have edited your comment successfully.');
      }));
    });

    describe('on failure', () => {
      it('displays an error message', inject((Flash) => {
        spyOn(Flash, 'error');

        isolateScope.submitEditedComment(comment);

        deferred.reject();
        isolateScope.$digest();

        expect(Flash.error).toHaveBeenCalledWith(
          'Error',
          'There was an error. Your comment may have not been edited.'
        );
      }));
    });
  });

  describe('deleteComment', () => {

    it('adds nameOfItem to the scope', () => {
      createDirective({first: 'Sgt', last: 'Hatred', name: 'rainbowDash'});

      isolateScope.deleteComment();
      expect(isolateScope.nameOfItem).toEqual('comment');
      expect(isolateScope.itemToBeDeleted).toEqual(isolateScope.comment);
    });

    it('opens the delete confirmation modal', inject((Modal) => {
      spyOn(Modal, 'open');
      createDirective({first: 'Sgt', last: 'Hatred', name: 'rainbowDash'});

      isolateScope.deleteComment();

      expect(Modal.open).toHaveBeenCalledWith(
        'Confirmation',
        deleteConfirmationModalTemplate,
        'red-modal',
        isolateScope
      );
    }));

    describe('closeModal', () => {

      it('removes nameOfItem from the scope', () =>{
        createDirective({first: 'Sgt', last: 'Hatred', name: 'rainbowDash'});
        isolateScope.deleteComment();
        isolateScope.closeModal();
        expect(isolateScope.nameOfItem).toBeUndefined();
        expect(isolateScope.itemToBeDeleted).toBeUndefined();
      });

      it('closes the modal', inject((Modal) => {
        spyOn(Modal, 'close');
        createDirective({first: 'Sgt', last: 'Hatred', name: 'rainbowDash'});

        isolateScope.closeModal();

        expect(Modal.close).toHaveBeenCalled();
      }));
    });

    describe('doDelete', () => {
      let comment, deferred;

      beforeEach(inject(($q) => {
        deferred = $q.defer();
        deferred.$promise = deferred.promise
        comment = { $destroy: function () { return deferred; } };
        createDirective({first: 'Sgt', last: 'Hatred', name: 'rainbowDash'});
      }));

      it('closes the modal', inject((Modal) => {
        spyOn(Modal, 'close');
        isolateScope.doDelete(comment);
        expect(Modal.close).toHaveBeenCalled();
      }));

      it('removes nameOfItem from the scope', () =>{
        isolateScope.deleteComment();
        isolateScope.doDelete(comment);
        expect(isolateScope.nameOfItem).toBeUndefined();
        expect(isolateScope.itemToBeDeleted).toBeUndefined();
      });

      describe('on success', () => {

        it('gives a flash success message', inject((Flash) => {
          spyOn(Flash, 'notify');

          isolateScope.doDelete(comment);
          deferred.resolve();
          isolateScope.$digest();
          expect(Flash.notify).toHaveBeenCalledWith('Success', 'You have deleted your comment successfully.');
        }));
      });

      describe('on failure', () => {
        it('displays an error message', inject((Flash) => {
          spyOn(Flash, 'error');

          isolateScope.doDelete(comment);

          deferred.reject();
          isolateScope.$digest();

          expect(Flash.error).toHaveBeenCalledWith(
            'Error',
            'There was an error. Your comment may have not been deleted successfully.'
          );
        }));
      });
    });
  });
});
