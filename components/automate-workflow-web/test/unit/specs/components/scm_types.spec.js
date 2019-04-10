import ng from 'angular';
import 'angular-mocks';
import scmTypesComponent from '../../../../src/components/scm_types/scm_types';

describe('ScmTypesComponent', () => {
  let scope, element, isolateScope;

  let states = {
    'local': {
      'supported_conversions': {
        'local': true,
        'bitbucket': true,
        'github': true
      }
    },
    'bitbucket': {
      'supported_conversions': {
        'local': true,
        'bitbucket': true,
        'github': false
      }
    },
    'github': {
      'supported_conversions': {
        'local': true,
        'bitbucket': false,
        'github': true
      }
    }
  }

  beforeEach(ng.mock.module(scmTypesComponent));

  function createDirective(scmType, formType, scmConfigs) {
    return inject(($compile, $rootScope) => {
      scope = $rootScope.$new();
      scope.scmConfigs = scmConfigs;
      scope.scmType = scmType;   // fromState in supportedTransition
      scope.formType = formType; // toState in supportedTransition

      element = $compile(ng.element('<div ng-model="formType" cd-scm-types="scmType" cd-scm-configs="scmConfigs"></div>'))(scope);
      isolateScope = element.isolateScope();
      scope.$digest();
    });
  };

  describe('supportedTransition validator', () => {
    for (let fromState in states) {
      for (let toState in states[fromState].supported_conversions) {
        let result = states[fromState].supported_conversions[toState];

        it('returns ' + result + ' for ' + fromState + ' to ' + toState, () => {
          createDirective(fromState, toState, []);

          // note that the supportedTranstion also takes the same value as both of the
          // inputs and derives the fromState from the scope of the form
          // see comments in createDirective above
          expect(isolateScope.supportedTransition(toState, toState)).toBe(result);
        });
      }
    }
  });

  describe('scmConfigured validator', () => {
    beforeEach(() => {
      let scmConfigs = [
        {
          'type': 'bitbucket',
          'scmSetupConfigs': [true]
        },
        {
          'type': 'local',
          'scmSetupConfigs': [true]
        }
      ];
      createDirective('local', 'bitbucket', scmConfigs);
    });

    it('returns true when an SCM config exists for the scmType', () => {
      expect(isolateScope.scmConfigured('bitbucket', 'bitbucket')).toBe(true);
    });

    it('returns false when there is no SCM config for the scmType', () => {
      expect(isolateScope.scmConfigured('github', 'github')).toBe(false);
    });

    describe('when github is configured', () => {
      beforeEach(() => {
        let scmConfigs = [
          {
            'type': 'github',
            'scmSetupConfigs': [true]
          }
        ];
        createDirective('local', 'bitbucket', scmConfigs);
      });


      it("returns true for scmType 'github'", () => {
        expect(isolateScope.scmConfigured('github', 'github')).toBe(true);
      });
    });
  });

  describe('messages.supportedTransition', () => {
    for (let fromState in states) {
      for (let toState in states[fromState].supported_conversions) {
        let valid = states[fromState].supported_conversions[toState];
        let description = 'returns ' + (valid ? 'null':'a message') + ' for ' + fromState + ' to ' + toState;
        it(description , () => {
          createDirective(fromState, toState, []);

          if (valid) {
            expect(isolateScope.messages.supportedTransition(fromState, toState)).toBe(null);
          } else {
            expect(isolateScope.messages.supportedTransition(fromState, toState)).not.toBe(null);
          }
        });
      }
    }
  });
});
