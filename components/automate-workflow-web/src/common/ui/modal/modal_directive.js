import ng from 'angular';
import ngAnimate from 'angular-animate';
import ModalService from './modal_service';
import modalTemplate from './modal.html';
import modalContent from './content/content';
import modalButtons from './buttons/buttons';

modalDirective.$inject = ['$compile', '$animate', 'Modal'];

function modalDirective($compile, $animate, Modal) {

  function showModal(overlay, modal, body) {
    $animate.enter(overlay, body);
    $animate.enter(modal, overlay);
  }

  function closeModal(overlay, modal) {
    if (!modal) return;
    $animate.leave(overlay);
    $animate.leave(modal);
  }

  function link(scope, element, attrs) {
    var open, close, modal, overlay;

    overlay = angular.element('<div class="cd-modal-overlay"></div>');

    open = function (header, template, class_list, passedScope) {
      if (passedScope) {
        scope = passedScope;
      }

      scope.header = header;
      scope.template = template;

      modal = $compile(modalTemplate)(scope);

      if(class_list) {
        modal.addClass(class_list);
      }

      showModal(overlay, modal, element);

      modal.on('click', function (event) {
        var target = event.target;

        if (target.className.match(/close/) || target.className.match(/cd-modal-overlay/)) {
          closeModal(overlay, modal);
          scope.$apply();
        } else {
          event.stopPropagation();
        }
      });

      overlay.on('click', function () {
        closeModal(overlay, modal);
        scope.$apply();
      });
    };

    close = function () {
      closeModal(overlay, modal);
    };

    Modal.register(open, close);
  }

  return {
    restrict: 'A',
    link: link,
    scope: false
  };
}

export default ng
  .module('cd.common.ui.modal.directive', [
    ngAnimate,
    ModalService,
    modalContent,
    modalButtons
  ])
  .directive('cdModal', modalDirective)
  .name;
