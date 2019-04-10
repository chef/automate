import ng from 'angular';
import ngAnimate from 'angular-animate';
import { filter, isArray, isEmpty, reject } from 'lodash';
import pos from '../../helpers/position';
import menuTemplate from './menu.html';
import menuItem from './menu_item/menu_item';

menuComponent.$inject = ['$animate', '$document', '$compile'];

function menuComponent($animate, $document, $compile) {

  function destroy(menu) {
    if (!menu) return;
    $animate.leave(menu);
  }

  // A menu can either be a list of items, or a list of lists.
  // The view expects a list of lists, so we take all items in
  // the top-level list that are not lists (orphans) and wrap
  // them in an array.
  function format(menu) {
    var orphans, sections;
    // Wrap orphaned items in an array
    orphans = reject(menu, isArray);
    sections = filter(menu, isArray);

    // Add then back as a proper section
    sections.push(orphans);

    // Return the new menu rejecting any empty sections
    return reject(sections, isEmpty);
  }

  function create(scope) {
    var body, menu;
    body = $document.find('body');
    scope.menu = format(scope.menu);
    menu = $compile(menuTemplate)(scope);

    // This prevents the menu from being closed if you click on a toggle
    menu.on('click', function (event) {
      event.stopPropagation();
    });

    $animate.enter(menu, body);

    return menu;
  }

  function positionMenu(element, menu) {
    var offsetTop = pos.offsetTop(element[0]) + element[0].offsetHeight;
    var offsetLeft = pos.offsetLeft(element[0]);

    menu.css({ top: offsetTop + 'px', left: offsetLeft + 'px' });
    if (pos.hasFixedParent(element[0])) {
      menu.css({ position: 'fixed' });
    }

    return menu;
  }

  function link(scope, element, attrs) {
    var menu;

    scope.menu_visible = false;

    element.on('click', function (event) {
      event.stopPropagation();
      scope.menu_visible = !scope.menu_visible;
      scope.$apply();
    });

    scope.$watch('menu_visible', function (showMenu, oldVal) {
      if (showMenu) {
        menu = create(scope);
        positionMenu(element, menu);
      } else {
        destroy(menu);
      }
    });

    // Broadcast a closemenu event when someone clicks off the menu.
    $document.on('click', function () {
      scope.menu_visible = false;
      scope.$apply();
    });

    // Close the menu when we get a closemenu event.
    scope.$on('closemenu', function () {
      destroy(menu);
      scope.$apply();
    });
  }

  return {
    restrict: 'A',
    scope: {
      menu: '=cdMenu'
    },
    link: link
  };
}

export default ng
  .module('cd.components.menu', [
    ngAnimate,
    menuItem
  ])
  .directive('cdMenu', menuComponent)
  .name;
