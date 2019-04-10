import ng from 'angular';
import aceTemplate from './ace.html';
import 'angular-ui-ace/src/ui-ace';
import 'brace';
import 'brace/theme/chrome';
import 'brace/mode/markdown';

function aceDirective() {

  function link(scope, element, attrs) {
    scope.aceOption = '';
    scope.placeholder = 'Make a reply';

    scope.aceLoaded = function(editor) {
      scope.editor = editor;

      editor.commands.addCommand({
        name: 'submitCommand',
        bindKey: {
          win: 'Ctrl-Enter',
          mac: 'Command-Enter'
        },
        exec: scope.submit
      });

      scope.setFocus();
    };

    scope.setFocus = function(editor) {
       scope.editor.focus();
    };
  }

  return {
    template: aceTemplate,
    replace: true,
    scope: {
      ngModel: '=',
      submit: '&onSubmit'
    },
    link: link
  };
}

export default ng
  .module('cd.components.ace', [
    'ui.ace',
  ])
  .directive('cdAce', aceDirective)
  .name;
