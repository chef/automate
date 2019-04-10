import ng from 'angular';
import 'angular-mocks';
import uiRouter from 'angular-ui-router';
import anchorScroll 
  from '../../../../../../src/common/ui/anchor_scroll/anchor_scroll';

describe('anchorScrollConfig', () => {
  let $anchorScrollProvider, $uiViewScrollProvider;

  // https://medium.com/@a_eife/testing-config-and-run-blocks-in-angularjs-1809bd52977e
  beforeEach(() => {
    ng
      .module('anchorScrollConfig', [uiRouter])
      .config((_$anchorScrollProvider_, _$uiViewScrollProvider_) => {
        $anchorScrollProvider = _$anchorScrollProvider_;
        $uiViewScrollProvider = _$uiViewScrollProvider_;
        spyOn($anchorScrollProvider, 'disableAutoScrolling');
        spyOn($uiViewScrollProvider, 'useAnchorScroll');
      });
    ng.mock.module('anchorScrollConfig');
    ng.mock.module(anchorScroll);
    inject();
  });

  it('should configure `anchorScroll` to disable auto-scrolling', () => {
    expect($anchorScrollProvider.disableAutoScrolling).toHaveBeenCalled();
  });

  it('should configure `$uiViewScroll` to use `anchorScroll`', () => {
    expect($uiViewScrollProvider.useAnchorScroll).toHaveBeenCalled();
  });
});
