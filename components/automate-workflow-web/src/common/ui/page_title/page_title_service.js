import ng from 'angular';

function pageTitleProvider() {
  let defaultTitle;
  
  this.setDefault = function (title) {
    defaultTitle = title;
  };

  function pageTitle($document) {
    let page = $document[0];

    if (!defaultTitle) {
      defaultTitle = page.title;
    }

    set(defaultTitle);

    function get() {
      return page.title;
    }

    function set(title) {
      page.title = title || defaultTitle;
      return page.title;
    }

    return {
      get: get,
      set: set
    };
  }
  
  this.$get = ['$document', pageTitle];
}

export default ng
  .module('cd.common.ui.pageTitle.service', [])
  .provider('pageTitle', pageTitleProvider)
  .name;
