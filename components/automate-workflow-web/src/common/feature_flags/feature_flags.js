import ng from 'angular';
import featureFlagsInitializer from './feature_flags_initializer';
import featureFlagsOverridesDecorator from './feature_flags_overrides_decorator';

export default ng
  .module('cd.common.featureFlags', [
    featureFlagsInitializer,
    featureFlagsOverridesDecorator
  ])
  .name;
