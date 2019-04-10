import ng from 'angular';
import changeRow from '../../components/dashboard/change_row/change_row';
import projectRow from '../../components/dashboard/project_row/project_row';
import pipeStatusList from '../../components/dashboard/pipe_status_list/pipe_status_list';
import pipeviz from '../../components/dashboard/pipeviz/pipeviz';
import dashboardTemplate from './dashboard.html';

function dashboardComponent() {
  return {
    template: dashboardTemplate
  };
}

export default ng
  .module('cd.routes.dashboard.component', [
    changeRow,
    projectRow,
    pipeStatusList,
    pipeviz
  ])
  .directive('cdDashboard', dashboardComponent)
  .name;
