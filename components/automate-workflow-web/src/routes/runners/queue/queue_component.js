import ng from 'angular';
import queueTemplate from './queue.html';
import ApiUrl from '../../../common/api/api_url';

function queueComponent() {
  return {
    template: queueTemplate
  };
}

export default ng
  .module('cd.routes.runners.queue.component', [
    ApiUrl
  ])
  .directive('cdJobsQueue', queueComponent)
  .name;
