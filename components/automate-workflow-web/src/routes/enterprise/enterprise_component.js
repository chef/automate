import ng from 'angular';
import enterpriseTemplate from './enterprise.html';

/*
 You might (rightfully) be wondering why we have two different templates in here.
 There is a temporary need right now to have two different UIs, one of which is
 behind a feature flag. The second template is part of that effort and will go
 away after ChefConf 2016 when the UI gets unified for real.
*/
function enterpriseComponent() {
  return {
    template: enterpriseTemplate
  };
}

export default ng
  .module('cd.routes.enterprise.component', [])
  .directive('cdEnterprise', enterpriseComponent)
  .name;
