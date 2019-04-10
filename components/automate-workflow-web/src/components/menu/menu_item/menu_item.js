import ng from 'angular';
import actionTemplate from './action.html';
import toggleTemplate from './toggle.html';
import buttonTemplate from './button.html';

menuItemComponent.$inject = ['$rootScope', '$compile'];

function menuItemComponent($rootScope, $compile) {

  function getCss(item) {
    return item.css ? item.css.join(' ') : '';
  }

  function actionType(element, scope) {
    var template = $compile(actionTemplate)(scope);
    var item = scope.item;

    element.addClass(getCss(item));
    element.append(template);
    element.on('click', function () {
      item.fun();
      // Close the menu when we are done
      $rootScope.$broadcast('closemenu');
    });
  }

  function toggleType(element, scope) {
    var item = scope.item;
    var template = $compile(toggleTemplate)(scope);

    element.addClass(getCss(item));
    element.append(template);
  }

  function buttonType(element, scope) {
    var template = $compile(buttonTemplate)(scope);
    var item = scope.item;

    element.addClass('btn');
    element.addClass(getCss(item));
    element.append(template);
    element.on('click', function() {
      item.fun();
      // Close the menu when we are done
      $rootScope.$broadcast('closemenu');
    });
  }

  function link (scope, element, attrs) {
    // Apply the correct behavior based on the item type.
    switch (scope.item.type) {
      case 'action':
        actionType(element, scope);
        break;
      case 'toggle':
        toggleType(element, scope);
        break;
      case 'button':
        buttonType(element, scope);
        break;
    }
  }

  return {
    restrict: 'A',
    link: link,
    scope: {
      item: '=cdMenuItem'
    }
  };
}

export default ng
  .module('cd.components.menu.item', [])
  .directive('cdMenuItem', menuItemComponent)
  .name;
