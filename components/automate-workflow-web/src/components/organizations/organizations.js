import ng from 'angular';
import Flash from '../../common/ui/flash/flash';
import Organization from '../../common/models/organization';
import organizationsTemplate from './organizations.html';
import orgCard from '../org_card/org_card';
import Session from '../../common/auth/session';
import { last } from 'lodash';

organizationsComponent.$inject = ['$http', 'ApiUrl', 'Flash'];

function organizationsComponent($http, ApiUrl, Flash) {
  function link(scope) {
    scope.reverse = false;
    scope.showNewOrgForm = false;

    scope.sortReverse = function (reverse) {
      scope.reverse = reverse;
    };

    scope.toggleNewOrgForm = function () {
      scope.showNewOrgForm = !scope.showNewOrgForm;
    };

    scope.saveAndClose = function (newOrg, webhook) {
      scope.orgs.$on('after-create', function(response) {
        scope.saveAndCloseSuccess();
        scope.createWebhook(webhook);
      });

      scope.orgs.$on('after-create-error', function(response) {
        if (response.status === 403) {
          Flash.error('Forbidden',
            'You must be an administrator to create an organization.');
        } else {
          Flash.error('Error',
            'There was an error saving your new organization.');
        }
      });

      scope.orgs.$create(newOrg);
    };

    scope.saveAndCloseSuccess = () => {
      scope.newOrg = null;
      scope.newNotification = null;
      scope.closeForm();
    };

    scope.closeForm = function () {
      scope.newOrg = null;
      scope.newNotification = null;
      scope.showNewOrgForm = false;
      scope.testResult = '';
      scope.webhook = {
        name: null,
        url: null,
        enabled: true
      };
    };

    scope.createWebhook = (webhook) => {
      if (webhook && webhook.name && webhook.url) {
        var org = last(scope.orgs);
        var createUrl = `${org.links.full.href}/notifications/slack-webhook`;

        $http.put(createUrl, webhook)
          .then(
            () => {},
            (response) => {
              Flash.error('Error', 'Your new organization was created, but there ' +
                'was a problem saving the Slack webhook.');
            }
          );
      }
    };

    scope.testWebhook = function(webhook) {

      $http.put(ApiUrl('/notifications/slack-webhook/test'), {
        url: webhook.url
      })
        .then(function() {
          scope.testResult = 'success';
        }, function(response) {
          scope.testResult = (response.status === 504 ? 'error-504' : 'error-any');
        });
    };
  }

  return {
    template: organizationsTemplate,
    link: link,
    scope: {
      orgs: '=orgs'
    }
  };
}

export default ng
  .module('cd.routes.organizations', [
    Flash,
    Organization,
    orgCard,
    Session
  ])
  .directive('cdOrganizations', organizationsComponent)
  .name;
