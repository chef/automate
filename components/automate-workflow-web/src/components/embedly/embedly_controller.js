import ng from 'angular';
import appConfig from '../../config';
import photoTemplate from './photo.html';
import textTemplate from './text.html';

function embedlyController($scope, $rootScope, $compile, $log, appConfig) {

  var UTILS = {
    none: function(obj) {
      return obj === null || obj === undefined;
    },
  };

  function local_display() {
    // If response type is error return false to exit function
    if (this.type === 'error') {
      return false;
    }

    this.title = this.title || this.url;

    this.style = this.imageStyle();

    if (UTILS.none(this.url) && !UTILS.none(this.thumbnail_url)) {
      this.url = this.thumbnail_url;
    }

    var my_scope = $scope.$new($scope);
    my_scope.original_url = this.original_url;
    my_scope.style = this.style;
    my_scope.url = this.url;
    my_scope.title = this.title;
    my_scope.thumbnail_url = this.thumbnail_url;
    my_scope.description = this.description;

    if (this.type === 'photo') {
      // HTML embed code for photos
      this.code = $compile(photoTemplate)(my_scope);
    } else if (this.type === 'video' || this.type === 'rich') {
      // HTML embed code for videos and rich media (included in Embedly API response)
      this.code = this.html;
    } else {
      this.code = $compile(textTemplate)(my_scope);
    }

    // Insert HTML embed code in the DOM as specified by method type (use replace method by default)
    if (this.options.method === 'after') {
      this.$elem.after(this.code);
    } else if (this.options.method === 'afterParent') {
      this.$elem.parent().after(this.code);
    } else if (this.options.method === 'replaceParent') {
      this.$elem.parent().replaceWith(this.code);
    } else {
      this.$elem.replaceWith(this.code);
    }

    this.$elem.triggerHandler('displayed', [this]);
  }

  $scope.key = appConfig.embedly_api_key;
  $scope.method = "afterParent";
  $scope.addImageStyles = true;
  $scope.display = local_display;
}

export default ng
  .module('cd.components.embedly.controller', [
    appConfig
  ])
  .controller('embedlyController', embedlyController)
  .name;
