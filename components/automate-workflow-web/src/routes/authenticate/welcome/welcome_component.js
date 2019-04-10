import ng from 'angular';
import welcomeTemplate from './welcome.html';

function welcomeComponent() {
  return {
    template: welcomeTemplate
  };
}

export default ng
  .module('cd.routes.authenticate.welcome.component', [])
  .directive('cdWelcome', welcomeComponent)
  .name;
