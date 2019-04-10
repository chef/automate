import ng from 'angular';
import logComplianceTemplate from './log_compliance.html';

function logComplianceComponent() {
  function link(scope, element, attrs) {
    scope.data = {};
    scope.summary = {};
    scope.profile = {};
    scope.showDescription = {};

    function controlImpact(control) {
      let title;
      let indicator = 'Passed';
      let impact = 0;
      let desc = control.desc;
      let results = control.results;
      for (let i = 0; i < results.length; i++) {
        if (results[i].status === 'skipped' || results[i].status === 'pending') {
          indicator = 'Skipped';
          impact = -1;
          title += ': ' + results[i].pending_message;
        } else if (results[i].status !== 'passed') {
            if (control.impact >= 0.7) {
              indicator = 'Critical';
            }
            else if (control.impact >= 0.4) {
              indicator = 'Major';
            }
            else {
              indicator = 'Minor';
            }
          impact = control.impact;
        }
        if (control.title === null) {
          title = '';
          title += control.results[i].code_desc;
        } else {
          title = control.title;
        }
        control.indicator = indicator;
        control.title = title;
      }
      return {
        impact: impact,
        indicator: indicator,
        results: results,
        title: title,
        desc: desc
      };
    }

    function controlsSummary(controls) {
      let summary = { critical: 0, major: 0, minor: 0, passed: 0, skipped: 0, total: 0 };
      controls.forEach((control) => {
        switch(control.indicator) {
          case 'Skipped':  summary.skipped += 1; break;
          case 'Minor':    summary.minor += 1; break;
          case 'Major':    summary.major += 1; break;
          case 'Critical': summary.critical += 1; break;
          case 'Passed':   summary.passed += 1; break;
        }
      });
      summary.total += controls.length;
      return summary;
    }

    scope.$watch('json', (jsonInput = '') => {
      if (jsonInput === null || jsonInput === '') {
        scope.sortedControls = [];
        return;
      }

      if (typeof jsonInput === 'string') {
        scope.data = JSON.parse(jsonInput);
      } else {
        scope.data = jsonInput;
      }

      let profiles = scope.data.profiles;
      let controls = profiles[0].controls;
      controls.map((control) => controlImpact(control));

      // For now, we assume there will always be more than one control (test)
      controls.sort((a, b) => {
        return b.impact - a.impact || a.title.localeCompare(b.title);
      });

      scope.summary = controlsSummary(controls);
      scope.sortedControls = controls;

      // For now, we are only grabbing the first profile result. Work to display
      // multiple profiles is set to happen in a future iteration.
      scope.profiles = profiles[0];
    });

    scope.showDetails = function(control) {
      if (scope.showDescription === control) {
        scope.showDescription = {};
      } else {
        scope.showDescription = control;
      }
    };
  }

  return {
    link: link,
    scope: {
      json:'='
    },
    template: logComplianceTemplate,
  };
}

export default ng
  .module('cd.components.logCompliance', [])
  .directive('cdComplianceReport', logComplianceComponent)
  .name;
