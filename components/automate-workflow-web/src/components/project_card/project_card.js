import ng from 'angular';
import Projects from '../../common/projects/projects';
import Flash from '../../common/ui/flash/flash';
import bsButton from 'angular-strap/dist/modules/button';
import { pick } from 'lodash';
import projectCardTemplate from './project_card.html';
import Modal from '../../common/ui/modal/modal';
import removeSlackConfirmationModal
  from '../../common/ui/modal/remove_slack_confirmation_modal.html';

function projectCardComponent(Projects, Flash, $http, ApiUrl, Modal) {

  function link(scope, element, attrs) {
    scope.webhook = {};
    scope.initialWebhook = {};

    if (scope.project.scm.type === "githubV2") { scope.project.scm.type = "github"; }

    scope.toggleEditForm = function () {
      scope.showEditForm = !scope.showEditForm;
      if (scope.showEditForm) {
        $http.get(`${scope.project.$url()}/notifications/slack-webhook`)
          .then(function (resp) {
            if (resp.data.webhook) {
              scope.webhook = resp.data.webhook;
              // Make a copy so we can detect changes. Saving the initial state
              // allows us to distinguish between slack webhook fields that have
              // been cleared from those that were never populated. When the
              // webhook URL field is cleared we delete the notification.
              scope.initialWebhook = Object.assign({}, resp.data.webhook);
            } else {
              scope.webhook.enabled = true;
            }
        });
      }
    };

    scope.cancel = function () {
      scope.showEditForm = false;
      scope.editProject = angular.copy(pick(scope.project, 'name', 'scm'));
      scope.webhook = {};
      scope.testResult = '';
    };

    scope.saveAndClose = function (webhook) {
      // clear out the bitbucket project key and repo if saving to local.
      if (scope.editProject.scm.type === 'local') {
        scope.editProject.scm = {type: 'local'};
      }

      Projects
        .update(scope.project, scope.editProject)
        .then(() => {
          ng.extend(scope.project, scope.editProject);

          if (webhook) {
            createOrDeleteWebhook(webhook);
          }

          scope.cancel();
        })
        .catch((resp) => {
          Flash.error('Error', resp.data.message);
        });
    };

    function createOrDeleteWebhook(webhook) {
      if (webhook.hasOwnProperty('url') &&
          webhook.url.trim() === '' &&
          scope.initialWebhook.url) {
        scope.openModal();
        return;
      } else {
        scope.createWebhook(webhook, scope.project);
      }
    }

    scope.testWebhook = function (webhook) {
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

    scope.createWebhook = function (webhook, project) {
      var createUrl = `${project.$url()}/notifications/slack-webhook`;

      if (webhook.name && webhook.url) {
        $http.put(createUrl, webhook, {'ignoreApiInterceptor':true})
        .catch((resp) => {
          Flash.error(
            'Error',
            'Your new project was created, but there was a problem saving the Slack webhook.'
          );
        });
      }
    };

    scope.deleteWebhook = () => {
      $http.delete(`${scope.project.$url()}/notifications/slack-webhook`)
        .then(function (response) {
          scope.showEditForm = false;
          scope.initialWebhook = {};
          scope.closeModal();
        },
        function (response) {
          scope.cancelModal();
          if (response.status === 403) {
            Flash.error('Forbidden',
              'You must be an administrator to update an organization.');
          } else {
            Flash.error('Error',
              'An unexpected error occurred. Failed to delete Slack webhook.');
          }
        });
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
      scope.cancel();
    };

    scope.cancelModal = () => {
      Modal.close();
    };


    scope.scmIcon = () => {
      if (scope.project.scm.type === 'bitbucket') {
        return 'bitbucket';
      } else if (scope.project.scm.type === 'github') {
        return 'github';
      }
    };

    scope.scmTypeToolTipText = () => {
      let scmName;
      let scmType = scope.project.scm.type;

      if (scmType === 'bitbucket') {
        scmName = 'Bitbucket';
      } else if (scmType === 'github') {
        scmName = 'GitHub';
      }

      if (scope.project.scm.url) {
        return 'Go to ' + scmName + ' Repo';
      } else {
        return scmName + ' Project';
      }
    };

    scope.cancel();
  }

  return {
    link: link,
    scope: {
      org: '=',
      project: '=',
      scmProviders: '='
    },
    template: projectCardTemplate
  };
}

export default ng
  .module('cd.components.projectCard', [
    Projects,
    Flash,
    'mgcrea.ngStrap.button',
    Modal
  ])
  .directive('cdProjectCard', projectCardComponent)
  .name;
