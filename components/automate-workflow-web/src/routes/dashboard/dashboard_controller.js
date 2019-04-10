import ng from 'angular';
import ngClipboard from 'angular-clipboard';
import { find, includes, keys, merge, sortBy, take, union, without } from 'lodash';
import ApiUrl from '../../common/api/api_url';
import Breadcrumb from '../../common/ui/breadcrumb/breadcrumb';
import Store from '../../common/store/store';
import Session from '../../common/auth/session';
import Modal from '../../common/ui/modal/modal';
import tokenInfoModal
  from '../../common/ui/modal/token_info_modal.html';

dashboardController.$inject = [
  '$scope',
  '$http',
  '$interval',
  '$filter',
  'Session',
  'ApiUrl',
  'Breadcrumb',
  'Store',
  '$location',
  'Modal',
  'clipboard'
];

function dashboardController($scope, $http, $interval, $filter, Session, ApiUrl, Breadcrumb, Store, $location, Modal, clipboard) {
  Breadcrumb.setCrumb([]);

  let allChanges;
  let comparisonObj = {};
  let filterDelim = ' ';
  let filterables = {
    approved: 'approved_by',
    org: 'org',
    change: 'title',
    delivered: 'delivered_by',
    project: 'project',
    status: 'stage_status',
    stage: 'stage',
    submitted: 'submitter'
  };

  setStore('filter', getStore('filter') || '');
  setStore('selected', getStore('selected') || []);

  $scope.pipeStats = {
    union: 'idle',
    rehearsal: 'idle',
    delivered: 'idle'
  };

  $scope.pipeStatus = {
    activeProjects: null,
    acceptedChanges: []
  };

  $scope.clearFilter = () => {
    $scope.filterText = '';
  };

  $scope.$watch('filterText', (text) => {
    parseFilter(text);
    setStore('filter', $scope.filterText);
    filterAndProcess();
  });

  $scope.filterText = getStore('filter');

  $scope.onKeyUp = (evt) => {

    if (evt.keyCode === 27) {   // Esc
      $scope.clearFilter();
    }
  };

  $scope.onKeyDown = (evt) => {

    if (evt.keyCode === 9) {    // Tab
      evt.preventDefault();

      let filterText = $scope.filterText;
      let lastDelim = filterText.lastIndexOf(' ');
      let candidate = filterText.substr(lastDelim + filterDelim.length);

      for (var key in filterables) {
        if (key.match(`^${candidate}`)) {
          $scope.filterText = [filterText.substr(0, lastDelim), filterDelim, key, ':'].join('').trim();
          return;
        }
      }
    }
  };

  $scope.$on('filterClick', (evt, data) => {
    $scope.filterText = data;
  });

  $scope.$on('toggleProject', (evt, project) => {
    let selected = getStore('selected');
    setStore('selected', (project.open ? union(selected, [project.key]) : without(selected, project.key)));
  });

  $scope.openModal = () => {
    Modal.open(
      'Delivery Token',
      tokenInfoModal,
      'info-modal',
      $scope
    );
  };

  $scope.copied = false;

  $scope.copy = () => {
    clipboard.copyText($scope.token);
    $scope.copied = true;
  };

  $scope.closeModal = () => {
    Modal.close();
  };

  function parseFilter(text = '') {

    for (var key in comparisonObj) {
      delete comparisonObj[key];
    }

    let items = text.split(' ');

    items.forEach((o) => {
      let [key, value] = o.split(':');

      if (includes(keys(filterables), key)) {
        comparisonObj[filterables[key]] = value;
      }
    });
  }

  function pipeStats(changes) {

    let stats = {
      verify: {
        running: 0,
        passed: 0,
        failed: 0
      },
      build: {
        running: 0,
        failed: 0
      },
      acceptance: {
        running: 0,
        passed: 0,
        failed: 0
      },
      union: 'passed',
      rehearsal: 'passed',
      delivered: 'passed'
    };

    changes.forEach((change) => {
      if (accepted(change)) {
        stats[change.stage] = change.stage_status;
      }
      else {
        stats[change.stage][change.stage_status] += 1;
      }
    });

    return stats;
  }

  function initProject(project) {
    return merge(project, {
      stats: {
        verify: {
          running: 0,
          passed: 0,
          failed: 0
        },
        build: {
          running: 0,
          failed: 0
        },
        acceptance: {
          running: 0,
          passed: 0,
          failed: 0
        }
      }
    });
  }

  function projectKey(orgName, projectName) {
    return [orgName, projectName].join('/');
  }

  function accepted(change) {
    return includes(['union', 'rehearsal', 'delivered'], change.stage);
  }

  function filter(changes) {
    return $filter('filter')(changes, comparisonObj);
  }

  function process(all) {

    let changes = [],
      projects = [],
      selected = getStore('selected');

    all.forEach((change) => {

      if (accepted(change)) {
        changes.push(change);
      }
      else {
        let project = find(projects, { key: projectKey(change.org, change.project) });

        if (!project) {
          let key = projectKey(change.org, change.project);

          project = initProject({
            key: key,
            name: change.project,
            org: change.org,
            changes: [],
            open: includes(selected, key)
          });

          projects.push(project);
        }

        project.changes.push(change);
        project.stats[change.stage][change.stage_status] += 1;
      }
    });

    return {
      changes: changes,
      projects: projects
    };
  }

  function filterAndProcess() {
    if (allChanges) {
      let filtered = filter(allChanges);
      let processed = process(filtered);

      $scope.pipeStatus.acceptedChanges = processed.changes;
      $scope.pipeStatus.activeProjects = processed.projects;
      $scope.pipeStatus.filtered = filtered.length !== allChanges.length;
      $scope.pipeStats = pipeStats(filtered);
    }
  }

  function getStore(key) {
    return Store.get(`dashboard.${key}`);
  }

  function setStore(key, value) {
    Store.put(`dashboard.${key}`, value);
  }

  function fetchPipelineStatus() {
    return $http.get(ApiUrl('/pipeline_status')).then((resp) => {
      allChanges = sortBy(resp.data, ['project', 'submitted_at']);
      filterAndProcess();
    });
  }

  fetchPipelineStatus();
  let poll = $interval(fetchPipelineStatus, 3000);
  $scope.$on('$destroy', () => $interval.cancel(poll));

  $scope.token = Session.get('token');
  if ($location.search().token) {
    $scope.openModal();
  }
}

export default ng
  .module('cd.routes.dashboard.controller', [
    Session,
    ApiUrl,
    Breadcrumb,
    Store,
    Modal,
    ngClipboard.name
  ])
  .controller('dashboardController', dashboardController)
  .name;
