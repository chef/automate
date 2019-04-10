import ng from 'angular';
import ApiUrl from '../../common/api/api_url';
import Flash from '../../common/ui/flash/flash';
import Modal from '../../common/ui/modal/modal';
import Session from '../../common/auth/session';
import samlSetupTemplate from './saml_setup.html';
import 'angular-spinner/angular-spinner';
import overwriteSamlConfirmationModalTemplate
  from  '../../common/ui/modal/overwrite_saml_confirmation_modal.html';
import deleteSamlConfirmationModalTemplate
  from  '../../common/ui/modal/delete_saml_confirmation_modal.html';
import bsParseOptions from 'angular-strap/dist/modules/parse-options';
import bsTooltip from 'angular-strap/dist/modules/tooltip';
import bsTooltipTpl from 'angular-strap/dist/modules/tooltip.tpl';
import bsSelect from 'angular-strap/dist/modules/select';
import bsSelectTpl from 'angular-strap/dist/modules/select.tpl';
import bsCompiler from 'angular-strap/dist/modules/compiler';
import bsDimensions from 'angular-strap/dist/modules/dimensions';

samlSetupComponent.$inject = ['$http', 'ApiUrl', 'Flash', 'Modal', 'Session'];

function samlSetupComponent($http, ApiUrl, Flash, Modal, Session) {

  function link(scope) {
    scope.enterprise = Session.get().enterprise;
    scope.authorized = scope.saml.authorized;
    scope.config = scope.saml.config || {};
    scope.isManual = !scope.config.metadata_url;
    scope.configExists = !!scope.saml.config;
    scope.roles = {'committer': false, 'reviewer': false, 'shipper': false, 'observer': false};

    if(!scope.configExists){
      scope.config.name_id = "default";
      scope.roles.observer = true;
    } else if (!scope.config.name_id){
      scope.config.name_id = "default";
    } else if (!scope.saml.config.default_roles) {
      scope.roles.observer = true;
    } else {
      for (let role of scope.config.default_roles) {
        scope.roles[role] = true;
      }
    }

    scope.policies = [
      {name: 'Default (No Policy)', value: 'default'},
      {name: 'Persistent', value: 'urn:oasis:names:tc:SAML:2.0:nameid-format:persistent'},
      {name: 'Email Address', value: 'urn:oasis:names:tc:SAML:1.1:nameid-format:emailAddress'},
      {name: 'Unspecified', value: 'urn:oasis:names:tc:SAML:1.1:nameid-format:unspecified'},
      {name: 'Windows Domain Qualified Name', value: 'urn:oasis:names:tc:SAML:1.1:nameid-format:WindowsDomainQualifiedName'},
      {name: 'Kerberos', value: 'urn:oasis:names:tc:SAML:2.0:nameid-format:kerberos'},
      {name: 'Entity', value: 'urn:oasis:names:tc:SAML:2.0:nameid-format:entity'}
    ];

    scope.save = () => {
      if (scope.configExists) {
        Modal.open(
          'Confirmation',
          overwriteSamlConfirmationModalTemplate,
          'red-modal',
          scope
        );
      } else {
        scope.doSave();
      }
    };

    scope.toggleManual = () => {
      scope.isManual = !scope.isManual;
    };

    scope.closeModal = () => {
      Modal.close();
    };

    function onPutSuccess() {
      Flash.notify('Success', 'Your SAML configuration has been saved.');
      scope.configExists = true;
    }

    function onPutFailure(response) {
      if(scope.isManual) {
        Flash.error('Error saving SAML configuration', response.data.message);
      } else {
        Flash.error('Error saving SAML configuration',
          'Please try configuring the SAML metadata manually.' +
          (response.data.message ? ` ${response.data.message}` : ''));
      }
    }

    scope.doSave = () => {
      Modal.close();
      scope.determineDefaultRoles();
      scope.config.sso_binding  = 'HTTP-Redirect';
      if(scope.isManual) { // how do we handle this on the API side?
        delete scope.config.metadata_url;
      } else {
        delete scope.config.sso_login_url;
        delete scope.config.idp_url;
        delete scope.config.cert;
      }
      $http.put(ApiUrl('/saml/config'), scope.config, {'ignoreApiInterceptor':true})
        .then(onPutSuccess, onPutFailure);
    };

    scope.confirmDelete = () => {
      Modal.open(
        'Confirmation',
        deleteSamlConfirmationModalTemplate,
        'red-modal',
        scope);
    };

    scope.doDelete = () => {
      Modal.close();
      $http.delete(ApiUrl('/saml/config'))
        .then(onDeleteSuccess, onDeleteFailure);
    };

    scope.toggleRole = (role) => {
      scope.roles[role] = ! scope.roles[role];
    };

    function onDeleteSuccess(response) {
      Flash.notify('Success', 'Your SAML configuration has been deleted.');
      scope.config = {};
      scope.configExists = false;
    }

    function onDeleteFailure(response) {
      Flash.error('Error deleting SAML configuration.');
    }

    scope.determineDefaultRoles = () => {
      scope.config.default_roles = [];
      for (let role in scope.roles) {
        if (scope.roles[role]) { scope.config.default_roles.push(role); }
      }
    };
  }

  return {
    link: link,
    template: samlSetupTemplate,
    scope: {
      saml: '='
    }
  };
}

export default ng
  .module('cd.routes.saml_setup', [
    ApiUrl,
    'angularSpinner',
    'mgcrea.ngStrap.helpers.parseOptions',
    'mgcrea.ngStrap.tooltip',
    'mgcrea.ngStrap.select',
    Flash,
    Modal,
    Session
  ])
  .directive('cdSamlSetup', samlSetupComponent)
  .name;
