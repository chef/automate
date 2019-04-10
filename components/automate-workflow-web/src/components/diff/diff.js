import ng from 'angular';
import ngScroll from 'angular-scroll';
import uiRouter from 'angular-ui-router';
import { find, first, last, filter } from 'lodash';
import diffTemplate from './diff.html';
import files from './files/files';
import line from './line/line';

diffComponent.$inject = [
  '$window',
  '$document',
  '$timeout',
  '$location',
  '$stateParams',
  '$q'
];

function diffComponent(
  $window, $document, $timeout, $location, $stateParams, $q) {

  function link(scope, element, attrs) {
    var container = angular.element(element[0].querySelector('.diff-container'));
    var diffFiles = angular.element(element[0].querySelector('.diff-files-container'));
    var diffLines = angular.element(element[0].querySelector('.diff-lines-container'));
    var diffComments = angular.element(element[0].querySelector('.diff-comments-container'));

    // Returns array of line comments within patchset,
    // multi-column sorted by: file -> lineRangeStart -> dateime
    scope.getLineComments = function () {
      // Get all line comments
      var comments = filter(scope.change.comments, {
        type: 'line',
        patchset: scope.patchset.sequenceNumber
      });

      // Sort by datetime
      comments.sort(function (a, b) {
        return Date.parse(a.datetime) - Date.parse(b.datetime);
      });

      // Sort by line range start
      comments.sort(function (a, b) {
        return a.lineRange[0] - b.lineRange[0];
      });

      // Sort by file
      comments.sort(function (a, b) {
        return a.file.localeCompare(b.file);
      });

      return comments;
    };

    // Returns array of line comments within patchset for a specific file,
    // multi-column sorted by: lineRangeStart -> datetime
    scope.getFileComments = function (file) {
      // Get all line comments for file
      var comments = filter(scope.change.comments, {
        type: 'line',
        patchset: scope.patchset.sequenceNumber,
        file: file[1]
      });

      // Sort by datetime
      comments.sort(function (a, b) {
        return Date.parse(a.datetime) - Date.parse(b.datetime);
      });

      // Sort by line range start
      comments.sort(function (a, b) {
        return a.lineRange[0] - b.lineRange[0];
      });

      return comments;
    };

    // Returns array of line comments within patchset for a specific line range of a file,
    // sorted by datetime
    scope.getLineRangeComments = function (lineRange) {
      // Get all line comments for line range
      var comments = filter(scope.change.comments, {
        type: 'line',
        patchset: scope.patchset.sequenceNumber,
        file: scope.activeFile[1],
        lineRange: lineRange
      });

      // Sort by datetime
      comments.sort(function (a, b) {
        return Date.parse(a.datetime) - Date.parse(b.datetime);
      });

      return comments;
    };

    scope.selectFile = function (file) {
      scope.activeFile = file;
      $location.search('file', file[1]);
    };

    scope.selectComment = function (comment) {
      scope.activeComment = comment;
      if (!comment) {
        $location.hash(null);
      } else {
        $location.hash('comment-' + comment.id);
      }
    };

    scope.selectRange = function (lineRange) {
      scope.activeRange = lineRange;
    };

    scope.toggleFiles = function () {
      scope.showFiles = !scope.showFiles;
    };

    scope.navToPatchset = function (patchset) {
      var file = scope.activeFile;
      scope.activeFile = null;
      scope.patchset = patchset;
      return scope.navToFile(file);
    };

    scope.navToFile = function (file) {
      scope.showFiles = false;
      return $q(function (resolve, reject) {
        if (scope.activeFile === file) {
          return resolve();
        }

        scope.selectFile(file);

        return scope.change.compare('base', 'p' + scope.patchset.sequenceNumber, file[1]).then(function (diff) {
          scope.diff = diff;
          return resolve();
        });
      });
    };

    scope.navToPrevFile = function () {
      var files = scope.files;
      var fileIndex = files.indexOf(scope.activeFile);
      var prevFile = fileIndex === 0 ? last(files) : files[fileIndex - 1];

      scope.navToFile(prevFile);

      scope.selectComment(null);
      scope.selectRange(null);
      scope.showComments = false;
    };

    scope.navToNextFile = function () {
      var files = scope.files;
      var fileIndex = files.indexOf(scope.activeFile);
      var nextFile = fileIndex === files.length - 1 ? first(files) : files[fileIndex + 1];

      scope.navToFile(nextFile);

      scope.selectComment(null);
      scope.selectRange(null);
      scope.showComments = false;
    };

    scope.toggleComments = function () {
      scope.showComments = !scope.showComments;
      if (!scope.showComments) {
        scope.selectComment(null);
        scope.selectRange(null);
      }
    };

    scope.navToComment = function (comment) {
      return $q(function (resolve, reject) {
        scope.selectComment(comment);
        scope.showComments = true;

        return scope.navToLineRange(comment.root.lineRange).then(function () {
          return $timeout(function () {
            var scrollContainer = angular.element($document[0].querySelector('.diff-comments'));
            return scrollContainer.scrollTo($document[0].getElementById('comment-' + comment.id), 60, 600).then(function () {
              return resolve();
            });
          }, 100);
        });
      });
    };

    scope.navToLineRange = function (lineRange) {
      return $q(function (resolve, reject) {
        scope.selectRange(lineRange);

        return $timeout(function () {
          return diffLines.scrollTo($document[0].getElementById('position-' + lineRange[0]), 72, 600).then(function () {
            return resolve();
          });
        }, 100);
      });
    };

    scope.navToPrevComment = function () {
      if (!scope.activeComment) {
        scope.selectComment(scope.getLineRangeComments(scope.activeRange)[0]);
      }

      var comments = scope.getLineComments();
      var commentIndex = comments.indexOf(scope.activeComment);
      var prevComment = commentIndex === 0 ? last(comments) : comments[commentIndex - 1];
      var prevFile = find(scope.files, { 1: prevComment.file });

      return scope.navToFile(prevFile).then(function () {
        return scope.navToComment(prevComment);
      });
    };

    scope.navToNextComment = function () {
      if (!scope.activeComment) {
        scope.selectComment(scope.getLineRangeComments(scope.activeRange)[0]);
      }

      var comments = scope.getLineComments();
      var commentIndex = comments.indexOf(scope.activeComment);
      var nextComment = commentIndex === comments.length - 1 ? first(comments) : comments[commentIndex + 1];
      var nextFile = find(scope.files, { 1: nextComment.file });

      return scope.navToFile(nextFile).then(function () {
        return scope.navToComment(nextComment);
      });
    };

    scope.onReviewCommentsClick = function () {
      if (scope.showComments) {
        scope.toggleComments();
      } else {
        scope.navToComment(scope.getFileComments(scope.activeFile)[0]);
      }
    };

    scope.$watch(function () { return $window.innerHeight - 317; }, function (height) {
      container.css('height', height + 'px');
    });

    scope.$watchGroup(['showComments', 'showFiles'], function (group) {
      if (group[0] && group[1]) {
        diffFiles.css({'height': '30%'});
        diffLines.css({'height': '40%'});
        diffComments.css({'height': '30%'});
      } else if (group[0]) {
        diffFiles.css({'height': '0%'});
        diffLines.css({'height': '50%'});
        diffComments.css({'height': '50%'});
      } else if (group[1]) {
        diffFiles.css({'height': '50%'});
        diffLines.css({'height': '50%'});
        diffComments.css({'height': '0%'});
      } else {
        diffFiles.css({'height': '0%'});
        diffLines.css({'height': '100%'});
        diffComments.css({'height': '0%'});
      }
    });

    scope.$watch('patchset', function (patchset, prevPatchset) {
      if (patchset !== prevPatchset) {
        scope.navToPatchset(patchset);
      }
    });

    // navigate to file specified in URL
    scope.navToFile(find(scope.files, { 1: $stateParams.file })).then(function () {
      // if a comment is also specified in URL, navigate to it as well
      var commentParam = $location.hash();
      if (commentParam) {
        var commentId = parseInt(commentParam.split('comment-')[1]);
        var comment = find(scope.change.comments, { id: commentId });
        scope.navToComment(comment);
      }
    });
  }

  return {
    link: link,
    template: diffTemplate
  };
}

export default ng
  .module('cd.components.diff', [
    uiRouter,
    'duScroll',
    files,
    line
  ])
  .directive('cdDiff', diffComponent)
  .name;
