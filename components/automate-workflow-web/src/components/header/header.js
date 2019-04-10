import ng from 'angular';
import headerController from './header_controller';
import headerTemplate from './header.html';
import UserDropdown from '../user_dropdown/user_dropdown';

function headerComponent() {
  return {
    template: headerTemplate,
    controller: 'headerController'
  };
}

export default ng
  .module('cd.components.header', [
    headerController,
    UserDropdown
  ])
  .directive('cdHeader', headerComponent)
  .name;
