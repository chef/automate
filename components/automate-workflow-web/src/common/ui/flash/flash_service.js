import ng from 'angular';
import { isFunction } from 'lodash';
   
function Flash() {
  var handler;
 
  return {
    register: function (func) {
      if (isFunction(func)) {
        handler = func;
      } else {
        throw new Error('You must pass a valid function to Flash.register');
      }
    },
    notify: function (header, message) {
      handler(header, message, 'notify');
    },
    error: function (header, message) {
      handler(header, message, 'error');
    }
  };
}
 
export default ng
  .module('cd.common.ui.flash.service', [])
  .service('Flash', Flash)
  .name;
