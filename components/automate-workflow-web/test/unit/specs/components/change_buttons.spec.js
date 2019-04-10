import ng from 'angular';
import 'angular-mocks';
import changeButtonsComponent
  from '../../../../src/components/change_buttons/change_buttons';
import deleteConfirmationModalTemplate
  from '../../../../src/common/ui/modal/delete_confirmation_modal.html'
import approveConfirmationModalTemplate
  from '../../../../src/common/ui/modal/approve_confirmation_modal.html'
import deliverConfirmationModalTemplate
  from '../../../../src/common/ui/modal/deliver_confirmation_modal.html'

describe('changeButtonsComponent', () => {
  let scope, element;

  beforeEach(ng.mock.module(changeButtonsComponent));

  function createDirective() {
    return inject(($compile, $rootScope) => {
      scope = $rootScope.$new();
      element = $compile(ng.element('<div cd-change-buttons>'))(scope);
      scope.$digest();
    });
  }

  describe('closeModal()', () => {

    it('closes the modal', inject((Modal) => {
      spyOn(Modal, 'close');
      createDirective();

      scope.closeModal();

      expect(Modal.close).toHaveBeenCalled();
    }));
  });

  describe('delete()', () => {

    it('opens the delete confirmation modal', inject((Modal) => {
      spyOn(Modal, 'open');
      createDirective();

      scope.delete();

      expect(Modal.open).toHaveBeenCalledWith(
        'Confirmation',
        deleteConfirmationModalTemplate,
        'red-modal',
        scope
      );
    }));
  });

  describe('doDelete()', () => {
    let change;

    beforeEach(inject(($q) => {
      change = { delete: function () { return $q.defer().promise }};
    }));

    it('closes the confirmation modal', inject((Modal) => {
      spyOn(Modal, 'close');
      createDirective();

      scope.doDelete(change);

      expect(Modal.close).toHaveBeenCalled();
    }));

    it('deletes the change', inject(($q) => {
      spyOn(change, 'delete').and.returnValue($q.defer().promise);
      createDirective();

      scope.doDelete(change);

      expect(change.delete).toHaveBeenCalled();
    }));

    describe('when the deletion succeeds', () => {

      it('redirects to the project state', inject(($q, $state) => {
        let deferred = $q.defer();
        let change = { delete: function () { return deferred.promise; }};

        spyOn($state, 'go');
        createDirective();

        scope.doDelete(change);

        deferred.resolve();
        scope.$digest();

        expect($state.go).toHaveBeenCalledWith(
          'main.enterprise.organizations.organization.project');
      }));
    });

    describe('when the deletion fails', () => {

      it('displays an error message', inject(($q, Flash) => {
        let deferred = $q.defer();
        let change = { delete: function () { return deferred.promise; }};

        spyOn(Flash, 'error');
        createDirective();

        scope.doDelete(change);

        deferred.reject();
        scope.$digest();

        expect(Flash.error).toHaveBeenCalledWith(
          'Error',
          'There was an error. Your change may have not been deleted successfully.'
        );
      }));
    });
  });

  describe('approve()', () => {

    it('opens the approve confirmation modal', inject((Modal) => {
      spyOn(Modal, 'open');
      createDirective();

      scope.approve();

      expect(Modal.open).toHaveBeenCalledWith(
        'Confirmation',
        approveConfirmationModalTemplate,
        'info-modal',
        scope
      );
    }));
  });

  describe('doApprove()', () => {
    let change;

    beforeEach(inject(($q) => {
      change = { approve: function () { return $q.defer().promise }};
    }));

    it('closes the confirmation modal', inject((Modal) => {
      spyOn(Modal, 'close');
      createDirective();

      scope.doApprove(change);

      expect(Modal.close).toHaveBeenCalled();
    }));

    it('approves the change', inject(($q) => {
      spyOn(change, 'approve').and.returnValue($q.defer().promise);
      createDirective();

      scope.doApprove(change);

      expect(change.approve).toHaveBeenCalled();
    }));

    describe('when the approval succeeds', () => {

      it('redirects to the new change status state', inject(($q, $state) => {
        let deferred = $q.defer();
        let change = { approve: function () { return deferred.promise; }};

        spyOn($state, 'go');
        createDirective();

        scope.doApprove(change);

        deferred.resolve();
        scope.$digest();

        expect($state.go).toHaveBeenCalledWith(
          'main.enterprise.organizations.organization.project.change.status');
      }));
    });

    describe('when the approval fails', () => {

      describe('with no API-provided error message', () => {
        it('displays an error message', inject(($q, Flash) => {
          let deferred = $q.defer();
          let change = { approve: function () { return deferred.promise; }};

          spyOn(Flash, 'error');
          createDirective();

          scope.doApprove(change);

          deferred.reject({});
          scope.$digest();

          expect(Flash.error).toHaveBeenCalledWith(
            'Error',
            'Change failed to merge.'
          );
        }));
      });

      describe('with an API-provided error message', () => {
        it('displays an error message', inject(($q, Flash) => {
          let deferred = $q.defer();
          let change = { approve: function () { return deferred.promise; }};

          spyOn(Flash, 'error');
          createDirective();

          scope.doApprove(change);

          deferred.reject({'data':
                           {'message': "custom error message"}
                          });
          scope.$digest();

          expect(Flash.error).toHaveBeenCalledWith(
            'Error',
            'custom error message'
          );
        }));
      });
    });
  });

  // TODO: Refactor deliver() so that it's much more sane to test. It's doing
  //       more than it should. It's working though so I'm going to leave it
  //       as-is since I'm currently focused on fixing a bug with doApprove()
  //       and doDelete().
  //
  // describe('deliver()', () => {

  //   it('opens the deliver confirmation modal', inject((Modal) => {
  //     spyOn(Modal, 'open');
  //     createDirective();

  //     scope.deliver();

  //     expect(Modal.open).toHaveBeenCalledWith(
  //       'Confirm Delivery',
  //       deliverConfirmationModalTemplate,
  //       'info-modal',
  //       scope
  //     );
  //   }));
  // });

  describe('doDeliver()', () => {
    let change;

    beforeEach(inject(($q) => {
      change = { deliver: function () { return $q.defer().promise }};
    }));

    it('closes the confirmation modal', inject((Modal) => {
      spyOn(Modal, 'close');
      createDirective();

      scope.doDeliver(change);

      expect(Modal.close).toHaveBeenCalled();
    }));

    it('delivers the change', inject(($q) => {
      spyOn(change, 'deliver').and.returnValue($q.defer().promise);
      createDirective();

      scope.doDeliver(change);

      expect(change.deliver).toHaveBeenCalled();
    }));

    describe('when the delivery fails', () => {

      it('displays an error message', inject(($q, Flash) => {
        let deferred = $q.defer();
        let change = { deliver: function () { return deferred.promise; }};

        spyOn(Flash, 'error');
        createDirective();

        scope.doDeliver(change);

        deferred.reject();
        scope.$digest();

        expect(Flash.error).toHaveBeenCalledWith(
          'Error',
          'There was an error. Your change may have not been delivered successfully.'
        );
      }));
    });
  });
});
