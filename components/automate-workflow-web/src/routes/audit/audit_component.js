import ng from 'angular';
import { keys, includes} from 'lodash';
import auditTemplate from './audit.html';

function auditComponent() {

  return {

    template: auditTemplate,

    link: (scope, element, attrs) => {

      let filterDelim = ' ';

      let filterables = {
        approved: 'approved_by',
        action: 'action',
        change: 'change_title',
        delivered: 'delivered_by',
        project: 'proj',
        status: 'status',
        stage: 'stage_name',
        submitted: 'submitted_by'
      };

      scope.filterText = '';
      scope.comparisonObj = {};

      scope.parseFilterText = () => {

        for (var key in scope.comparisonObj) {
          delete scope.comparisonObj[key];
        }

        let items = scope.filterText.split(' ');

        items.forEach((o) => {
          let pair = o.split(':');
          let key = pair[0];
          let value = pair[1];

          if (includes(keys(filterables), key)) {
            scope.comparisonObj[filterables[key]] = value;
          }
        });
      };

      scope.onFieldClick = (evt) => {
        let el = ng.element(evt.target);

        let fieldContainer = closest(el, 'TD');
        let field = fieldContainer.attr('data-field');
        let value = fieldContainer.attr('data-value');

        if (field && value && !scope.filterText.match(field)) {
          scope.filterText = [scope.filterText, filterDelim, [field, value].join(':')].join('').trim();
          scope.parseFilterText();
        }
      };

      scope.clearFilter = () => {
        scope.filterText = '';
        scope.parseFilterText();
      };

      scope.onKeyUp = (evt) => {

        if (evt.keyCode === 27) {   // Esc
          scope.clearFilter();
        }
      };

      scope.onKeyDown = (evt) => {

        if (evt.keyCode === 9) {    // Tab
          evt.preventDefault();

          let filterText = scope.filterText;
          let lastDelim = filterText.lastIndexOf(' ');
          let candidate = filterText.substr(lastDelim + filterDelim.length);

          for (var key in filterables) {
            if (key.match(`^${candidate}`)) {
              scope.filterText = [filterText.substr(0, lastDelim), filterDelim, key, ':'].join('').trim();
              return;
            }
          }
        }
      };

      function closest(el, tagName) {
        let parent = el.parent();

        if (parent[0] && parent[0].tagName !== tagName) {
          parent = closest(parent, tagName);
        }

        return parent;
      }
    }
  };
}

export default ng
  .module('cd.routes.audit.component', [])
  .directive('cdAudit', auditComponent)
  .name;
