import ng from 'angular';
import chunkTemplate from './chunk-unified.html';
import normalTemplate from './normal-unified.html';
import deletionTemplate from './deletion-unified.html';
import additionTemplate from './addition-unified.html';
import chunkTemplateSplit from './chunk-split.html';
import normalTemplateSplit from './normal-split.html';
import deletionTemplateSplit from './deletion-split.html';
import additionTemplateSplit from './addition-split.html';

diffLineComponent.$inject = ['$compile', '$location', '$timeout', '$document'];

function diffLineComponent($compile, $location, $timeout, $document) {

  function link(scope, element, attrs) {

    function getTemplate(line) {
      if (scope.diffView === 'split') {
        switch (line.type) {
          case ('chunk'):
            return chunkTemplateSplit;
          case ('normal'):
            return normalTemplateSplit;
          case ('del'):
            return deletionTemplateSplit;
          case ('add'):
            return additionTemplateSplit;
        }
      } else {
        switch (line.type) {
          case ('chunk'):
            return chunkTemplate;
          case ('normal'):
            return normalTemplate;
          case ('del'):
            return deletionTemplate;
          case ('add'):
            return additionTemplate;
        }
      }
    }

    var template = null;
    var line = null;

    function writeLine() {
      template = getTemplate(scope.line);
      line = angular.element($compile(template)(scope));
      element.empty().append(line);

      scope.position = scope.line.position;

      element.attr({
        'class': scope.line.type,
        'id': 'position-' + scope.position
      });

      scope.lineRange = [scope.position, scope.position];

      var lineGutter = angular.element(element[0].querySelectorAll('th'));
      lineGutter.on('mouseenter', scope.toggleIsHovered);
      lineGutter.on('mouseleave', scope.toggleIsHovered);
      lineGutter.on('click', function () {
        scope.selectComment(null);
        if (scope.activeRange && scope.activeRange[0] === scope.lineRange[0]) {
          scope.selectRange(null);
          if (scope.showComments) {
            scope.toggleComments();
          }
        } else {
          scope.navToLineRange(scope.lineRange);
          if (!scope.showComments) {
            scope.toggleComments();
          }
        }
      });
    }

    function writeComments(length) {
      if (length) {
        element.addClass('has-comments');
      } else {
        element.removeClass('has-comments');
      }
    }

    scope.toggleIsHovered = function () {
      element.toggleClass('is-hovered');
    };

    scope.$watch('getLineRangeComments(lineRange).length', function (length) {
      writeComments(length);
    });

    scope.$watch('diffView', function() {
      writeLine();
      writeComments(scope.getLineRangeComments(scope.lineRange).length);
    });

    scope.$watch('activeRange', function (activeRange) {
      if (activeRange && activeRange[0] === scope.lineRange[0]) {
        element.addClass('is-active');
      } else {
        element.removeClass('is-active');
      }
    });

  }

  return {
    link: link
  };
}

export default ng
  .module('cd.components.diff.line', [])
  .directive('cdDiffLine', diffLineComponent)
  .name;
