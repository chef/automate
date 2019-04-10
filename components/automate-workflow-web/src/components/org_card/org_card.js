import ng from 'angular';
import Flash from '../../common/ui/flash/flash';
import { pick } from 'lodash';
import pos from '../../helpers/position';
import orgCardTemplate from './org_card.html';
import ApiUrl from '../../common/api/api_url';
import Modal from '../../common/ui/modal/modal';
import removeSlackConfirmationModal
  from '../../common/ui/modal/remove_slack_confirmation_modal.html';

orgCardComponent.$inject = ['$http', 'ApiUrl', 'Flash', 'Modal'];

function orgCardComponent($http, ApiUrl, Flash, Modal) {

  function link(scope, element, attrs) {
    scope.webhook = {};
    scope.initialWebhook = {};
    scope.showEditForm = false;

    scope.toggleEditForm = function () {
      scope.showEditForm = !scope.showEditForm;
      if (scope.showEditForm) {
        $http.get(ApiUrl(`/orgs/${scope.org.name}/notifications/slack-webhook`))
          .then(function (resp) {
            if (resp.data.webhook) {
              scope.webhook = resp.data.webhook;
              // Make a copy so we can detect changes.
              scope.initialWebhook = Object.assign({}, resp.data.webhook);
            } else {
              scope.webhook.enabled = true;
            }
        });
      }
    };

    scope.cancel = function () {
      scope.showEditForm = false;
      scope.webhook = {};
    };

    scope.saveAndClose = (webhook) => {

      if (webhook.hasOwnProperty('url') &&
          webhook.url.trim() === '' &&
          scope.initialWebhook.url) {
        scope.openModal();
        return;
      }

      scope.createWebhook(webhook);
      scope.cancel();
    };

    scope.delete = function () {
      scope.org.$destroy();
    };

    scope.testWebhook = (webhook) => {
      scope.testResult = '';

      $http.put(ApiUrl('/notifications/slack-webhook/test'), {
        url: webhook.url
      })
      .then(() => {
        scope.testResult = 'success';
      })
      .catch((response) => {
        scope.testResult = (response.status === 504 ? 'error-504' : 'error-any');
      });
    };

    scope.createWebhook = (webhook) => {
      var createUrl = ApiUrl(`/orgs/${scope.org.name}/notifications/slack-webhook`);

      if (webhook.name && webhook.url) {
        $http.put(createUrl, webhook)
          .catch(function (response) {
            if (response.status === 403) {
              Flash.error('Forbidden',
                'You must be an administrator to update an organization.');
            } else {
              Flash.error('Error',
                'There was a problem saving the Slack webhook.');
            }
          });
      }
    };

    scope.openModal = () => {
      Modal.open(
        'Confirmation',
        removeSlackConfirmationModal,
        'red-modal',
        scope
      );
    };

    scope.closeModal = () => {
      Modal.close();
    };

    scope.cancelModal = () => {
      Modal.close();
    };

    scope.deleteWebhook = () => {
      scope.closeModal();
      $http.delete(ApiUrl(`/orgs/${scope.org.name}/notifications/slack-webhook`))
        .then(function (response) {
          scope.cancel();
          scope.initialWebhook = {};
        },
        function (response) {
          if (response.status === 403) {
            Flash.error('Forbidden',
              'You must be an administrator to update an organization.');
          } else {
            Flash.error('Error',
              'An unexpected error occurred. Failed to delete Slack webhook.');
          }
        });
    };
  }

  return {
    link: link,
    scope: {
      org: '='
    },
    template: orgCardTemplate
  };
}

export default ng
  .module('cd.components.orgCard', [
      Flash,
      Modal
    ])
  .directive('cdOrgCard', orgCardComponent)
  .name;
