import ng from 'angular';
import filesTemplate from './files.html';

function filesComponent() {
  return {
    template: filesTemplate
  };
}

export default ng
  .module('cd.routes.change.review.files.component', [])
  .directive('cdFiles', filesComponent)
  .name;
