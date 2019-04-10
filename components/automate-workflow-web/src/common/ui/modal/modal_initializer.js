import ng from 'angular';
import ModalService from './modal_service';

modalInitializer.$inject = ['$rootScope', 'Modal'];

function modalInitializer($rootScope, Modal) {
  $rootScope.$on('$stateChangeStart', Modal.close);
}

export default ng
  .module('cd.common.ui.modal.initializer', [
    ModalService
  ])
  .run(modalInitializer)
  .name;
