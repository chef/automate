import ng from 'angular';
import { debounce } from 'lodash';

infiniteScrollComponent.$inject = ['$rootScope', '$document', '$window'];

// This provide a simple way to do infinite scrolling in the main view. It probably
// won't work for things outside the main view.
function infiniteScrollComponent($rootScope, $document, $window) {

  function link(scope, element, attrs) {
    // doc is a reference to the document wrapped in jqlite.
    var doc = angular.element($document);
    // Callback is the function that eventually gets called when we reach the bottom
    // of the page.
    var callback = debounce(function () {
      scope.scrollFun.call();
    }, 1500,
    {
      leading: true,
      trailing: false
    });

    function loader () {
      // Height of the element being infinitly scrolled
      var elHeight = element[0].scrollHeight;
      // Height of the window
      var winHeight = $window.innerHeight;
      // When to trigger the callback. The scrolled element should be at the bottom
      // when the documents scrollHeight matches the element's height minus the
      // height of the window.
      var loadAt = elHeight - winHeight;
      // Add some padding to the doc.scrollTop. This should cause the load to
      // happen a bit before the bottom of the screen is reached.
      var pad = 100;

      if ((doc.scrollTop() + pad) >= loadAt) {
        callback();
      }

    }

    // Unbind listeners
    function unbind() {
      doc.off('scroll', loader);
    }

    // Bind listeners
    doc.on('scroll', loader);
    scope.$on('$destroy', unbind);
  }

  return {
    link: link,
    scope: {
      scrollFun: '&cdInfiniteScroll'
    }
  };
}

export default ng
  .module('cd.components.infiniteScroll', [])
  .directive('cdInfiniteScroll', infiniteScrollComponent)
  .name;
