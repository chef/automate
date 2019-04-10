import ng from 'angular';
import { keyBy } from 'lodash';

function patchsetStatusComponent() {

  function link(scope, element, attrs) {
    scope.$watchCollection('change.stages', function () {
      var statusText;
      var change = scope.change;
      var stages = keyBy(change.stages, 'stage');

      switch (change.stages.length) {
        case 6:
          if (stages.delivered.status === 'passed') {
            statusText = 'Delivered';
          } else if (stages.delivered.status === 'running') {
            statusText = 'Being Delivered';
          } else if (stages.delivered.status === 'failed') {
            statusText = 'Delivery Failure';
          }
          break;
        case 5:
          if (stages.rehearsal.status === 'passed') {
            statusText = 'In Rehearsal';
          } else if (stages.rehearsal.status === 'running') {
            statusText = 'Deploying to Rehearsal';
          } else if (stages.rehearsal.status === 'failed') {
            statusText = 'Rehearsal Failure';
          }
          break;
        case 4:
          if (stages.union.status === 'passed') {
            statusText = 'In union';
          } else if (stages.union.status === 'running') {
            statusText = 'Deploying to Union';
          } else if (stages.union.status === 'failed') {
            statusText = 'Union failure';
          }
          break;
        case 3:
          if (stages.acceptance.status === 'passed') {
            statusText = 'In Acceptance';
          } else if (stages.acceptance.status === 'running') {
            statusText = 'Deploying to Acceptance';
          } else if (stages.acceptance.status === 'failed') {
            statusText = 'Acceptance Failure';
          }
          break;
        case 2:
          if (stages.build.status === 'passed') {
            statusText = 'Successfully Built';
          } else if (stages.build.status === 'running') {
            statusText = 'Building';
          } else if (stages.build.status === 'failed') {
            statusText = 'Build Failure';
          }
          break;
        case 1:
          if (stages.verify.status === 'passed' && change.state === 'merged') {
            statusText = 'Approved';
          } else if (
              stages.verify.status === 'running' ||
              stages.verify.status === 'passed' &&
              change.state === 'open'
            ) {
            statusText = 'In Review';
          } else if (stages.verify.status === 'failed') {
            statusText = 'Verify Failure';
          }
          break;
        default:
          statusText = 'Queued';
      }

      element.text(statusText);
    });
  }

  return {
    link: link,
    scope: {
      change: '=cdPatchsetStatus'
    }
  };
}

export default ng
  .module('cd.components.patchsetStatus', [])
  .directive('cdPatchsetStatus', patchsetStatusComponent)
  .name;
