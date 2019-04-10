import ng from 'angular';
import reviewTemplate from './review.html';

function reviewComponent() {
  return {
    template: reviewTemplate
  };
}

export default ng
  .module('cd.routes.change.review.component', [])
  .directive('cdReview', reviewComponent)
  .name;
