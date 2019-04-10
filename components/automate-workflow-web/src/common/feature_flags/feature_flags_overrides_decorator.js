import ng from 'angular';
import 'angular-feature-flags/dist/featureFlags';
import featureFlagsOverridesTemplate from './feature_flags_overrides.html';

featureFlagsOverridesDecorator.$inject = ['$delegate'];

function featureFlagsOverridesDecorator($delegate) {
  let directive = $delegate[0];
  directive.template = featureFlagsOverridesTemplate;
  return $delegate;
}

export default ng
  .module('cd.common.featureFlags.decorator', ['feature-flags'])
  .decorator('featureFlagOverridesDirective', featureFlagsOverridesDecorator)
  .name;
