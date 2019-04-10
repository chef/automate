import ng from 'angular';
import ModalService from './modal_service';
import ModalDirective from './modal_directive';
import modalInitializer from './modal_initializer';

export default ng
  .module('cd.common.ui.modal', [
    ModalService,
    ModalDirective,
    modalInitializer
  ])
  .name;
