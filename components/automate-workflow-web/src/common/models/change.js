import ng from 'angular';
import restmod from 'angular-restmod/dist/angular-restmod-bundle';
import { find } from 'lodash';
import parseDiff from 'parse-diff/dist/parse-diff';
import appConfig from '../../config';
import Comment from './comment';

Change.$inject = [
  'restmod',
  '$http',
  'appConfig'
];

function Change(restmod, $http, appConfig) {
  var def = {
    comments: { hasMany: 'Comment' },

    compare: function (start, end, file) {
      var url = this.$url() + '/compare';
      var params = { start: start, end: end, file: file, context: 3 };

      return $http.get(url, { params: params }).then(function (resp) {
        var diff = resp.data.diff;

        diff.files = parseDiff(diff.content);

        diff.files.forEach(function (file) {
          file.chunks = [];

          // Traverse lines in diff and compute each chunk
          file.lines.forEach(function (line, index) {
            // If line is a chunk line
            if (line.chunk) {
              // Extract chunk info from chunk line:
              // @@ -fromStart,fromCount +toStart,toCount @@
              // @@ -fromStart,fromCount +toStart @@
              // @@ -fromStart +toStart,toCount @@
              // @@ -fromStart +toStart @@
              var chunkInfo = line.content.match(/^@@\s-(\d*),??(\d*)??\s\+(\d*),??(\d*)??\s@@.*$/);
              // Add new chunk
              file.chunks.push({
                content: line.content,
                fromStart: parseInt(chunkInfo[1]),
                fromCount: chunkInfo[2] ? parseInt(chunkInfo[2]) : 1,
                toStart: parseInt(chunkInfo[3]),
                toCount: chunkInfo[4] ? parseInt(chunkInfo[4]) : 1,
                additions: 0,
                deletions: 0,
                lines: []
              });
            // Else line is a normal, add, or del line
            } else {
              // Push line into current chunk
              var chunk = file.chunks[file.chunks.length - 1];
              chunk.lines.push(line);

              // If next line is a chunk line or current line is the last line
              var nextLine = file.lines[index+1];
              if ((nextLine && nextLine.chunk) || index === file.lines.length - 1) {
                // Traverse lines in current chunk
                chunk.lines.forEach(function (line, index) {
                  // Compute additions/deletions within chunk
                  chunk.additions = line.add ? chunk.additions + 1 : chunk.additions;
                  chunk.deletions = line.del ? chunk.deletions + 1 : chunk.deletions;

                  // Compute each hunk within chunk

                  // If first line in chunk
                  if (index === 0) {
                    // Add first hunk in chunk, including the line and its type
                    chunk.hunks = [{ type: line.type, lines: [line] }];
                  } else {
                    // Push line into current hunk
                    var hunk = chunk.hunks[chunk.hunks.length - 1];
                    hunk.lines.push(line);

                    // If next line is a different type
                    var nextLine = chunk.lines[index+1];
                    if (nextLine && line.type !== nextLine.type) {
                      // If current line is a deleted line and next line is an added line
                      if (line.type === 'del' && nextLine.type === 'add') {
                        // Change current hunk type to replace
                        hunk.type = 'replace';
                      } else {
                        // Start new hunk and add it to the chunk
                        hunk = { type: nextLine.type, lines: [] };
                        chunk.hunks.push(hunk);
                      }
                    }
                  }

                  // Calculate position of line as if within a full line context diff (used for line comments)
                  // P = (index of line within chunk) + (chunk line offset) + (sum of additions or deletions of previous chunks)

                  // Sum up additions and deletions of lines from each previous chunk
                  var sums = file.chunks.slice(0, file.chunks.indexOf(chunk)).reduce(function (sums, chunk) {
                    return [sums[0] + chunk.additions, sums[1] + chunk.deletions];
                  }, [0, 0]);

                  // If a normal or deleted line
                  if (line.type !== 'add') {
                    line.position = index + chunk.fromStart + sums[0];
                  // Else line is an added line
                  } else {
                    line.position = index + chunk.toStart + sums[1];
                  }
                });
              }
            }
          });
        });

        return diff;
      });
    },

    delete: function () {
      return $http.delete(this.$url(), {}).then(angular.bind(this, function (resp) {
        return this.$fetch();
      }));
    },

    approve: function () {
      return $http.post(this.$url() + '/merge', {}).then(angular.bind(this, function (resp) {
        return this.$fetch();
      }));
    },

    deliver: function () {
      return $http.post(this.links.deliver.href, {}).then(angular.bind(this, function (resp) {
        return this.$fetch();
      }));
    },

    isVerified: function () {
      return !!find(this.stages, { stage: 'verify', status: 'passed' });
    },

    inUnion: function () {
      return !!find(this.stages, { stage: 'union' });
    },

    rerun: function (url) {
      return $http.post(url, {}).then((resp) => {
        return this.$fetch().$promise;
      });
    }
  };

  return restmod.model(null, def);
}

export default ng
  .module('cd.common.models.change', [
    'restmod',
    appConfig,
    Comment
  ])
  .factory('Change', Change)
  .name;
