import ng from 'angular';
import focusIf from '../../../components/focus_if/focus_if';
import loginTemplate from './login.html';

function loginComponent() {
  return {
    template: loginTemplate
  };
}

export default ng
  .module('cd.routes.authenticate.login.component', [
    focusIf
  ])
  .directive('cdLogin', loginComponent)
  .name;
