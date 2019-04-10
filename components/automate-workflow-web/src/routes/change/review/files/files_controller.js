import ng from 'angular';
import { find, last } from 'lodash';

filesController.$inject = [
  '$scope',
  '$location',
  'Breadcrumb',
  'organization',
  'project',
  'change',
  'patchset'
];

function filesController(
  $scope, $location, Breadcrumb, organization, project, change, patchset) {

  $scope.change = change;

  $scope.patchset = patchset;

  $scope.files = patchset.filesChanged;

  $scope.lastFile = function () {
    return last($scope.files);
  };

  $scope.currentFile = function () {
    return find($scope.files, { 1: $location.search().file });
  };

  $scope.isActivePatchset = function (patchset) {
    var activeSequenceNumber = parseInt($location.search().end.split('p')[1]);
    return activeSequenceNumber === patchset.sequenceNumber;
  };

  $scope.selectPatchset = function (patchset) {
    $scope.patchset = patchset;
    $location.search('end', 'p' + patchset.sequenceNumber);
  };
}

export default ng
  .module('cd.review.files.controller', [])
  .controller('filesController', filesController)
  .name;
