import ng from 'angular';
import { isFunction } from 'lodash';

function Modal() {
  let openHandler, closeHandler;

  return {
    register: function (openFunc, closeFunc) {
      if (isFunction(openFunc) && isFunction(closeFunc)) {
        openHandler = openFunc;
        closeHandler = closeFunc;
      } else {
        throw new Error('You must pass a valid open and close function to Modal.register');
      }
    },

    open: function (header, template, class_list, passedScope) {
      if (isFunction(openHandler)) { 
        openHandler(header, template, class_list, passedScope);
      }
    },

    close: function () {
      if (isFunction(closeHandler)) {
        closeHandler();
      }
    }
  };
}

export default ng
  .module('cd.common.ui.modal.service', [])
  .service('Modal', Modal)
  .name;
