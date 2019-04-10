import ng from 'angular';

function scmTypesComponent() {

  function link(scope, element, attrs, ngModel) {
    let states = {
      'local': {
        'supported_conversions': {
          'local': true,
          'bitbucket': true,
          'github': true
        },
        'error_messages': {
          'local': null,
          'bitbucket': null,
          'github': null
        }
      },
      'bitbucket': {
        'supported_conversions': {
          'local': true,
          'bitbucket': true,
          'github': false
        },
        'error_messages': {
          'local': null,
          'bitbucket': null,
          'github': "This project cannot be edited to use GitHub as the Source Code Provider. If you wish to set up a GitHub-integrated project, please add a new project."
        }
      },
      'github': {
        'supported_conversions': {
          'local': true,
          'bitbucket': false,
          'github': true
        },
        'error_messages': {
          'local': null,
          'bitbucket': "This project cannot be edited to use Bitbucket as the Source Code Provider. If you wish to set up a Bitbucket-integrated project, please add a new project.",
          'github': null
        }
      }
    };

    // Validation Functions
    scope.supportedTransition = (modelVal, viewVal) => {
      let fromState = scope.currentType;
      let toState = modelVal;
      return states[fromState].supported_conversions[toState];
    };

    scope.scmConfigured = (modelVal, viewVal) => {
      let toState = modelVal;
      let scmConfigured = false;

      for (let config of scope.scmConfigs) {
        if (config.type === toState && config.scmSetupConfigs.length > 0) {
          scmConfigured = true;
        }
      }
      return scmConfigured;
    };
    ngModel.$validators.supportedTransition = scope.supportedTransition;
    ngModel.$validators.scmConfigured = scope.scmConfigured;

    // Validaiton Error Message Functions
    scope.messages = {
      supportedTransition: (fromState, toState) => {
        return states[fromState].error_messages[toState];
      },

      scmConfigured: (scm) => {
        let scmName;
        switch(scm) {
        case 'github':
          scmName = 'GitHub';
          break;
        case 'bitbucket':
          scmName = 'Bitbucket';
        }
        return "To add a project that connects to a " + scmName +
          " repo, a Chef Automate administrator must first configure" +
          " the link with " + scmName + " in";
      }
    };
    ngModel.messages = scope.messages;

    ngModel.toggle = (value) => {
      ngModel.$setViewValue(value); // also sets modelValue and triggers validation
    };

    ngModel.isActive = (value) => {
      return value == ngModel.$modelValue;
    };
  }

  return {
    require: 'ngModel',
    scope: {
      currentType: '=cdScmTypes',
      scmConfigs:  '=cdScmConfigs'
    },
    link: link
  };
}

export default ng
  .module('cd.components.scmTypes', [])
  .directive('cdScmTypes', scmTypesComponent)
  .name;
