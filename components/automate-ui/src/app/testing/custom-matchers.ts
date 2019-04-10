export const customMatchers: jasmine.CustomMatcherFactories = {

  // Affirms the element contains a sub-element at the given CSS path.
  toContainPath: function (_util, _customEqualityTesters) {
    return {
      compare: function (element: HTMLElement, path: string) {
        const result = {
          pass: false,
          message: ''
        };
        if (!element) {
          result.message = 'Expected element to be non-empty';
        } else if (!path) {
          result.message = 'Expected path to be non-empty';
        } else {
          result.pass = element.querySelector(path) !== null;
          result.message =
            'Expected ' + element + (result.pass ? ' not' : '') + ' to contain ' + path;
        }
        return result;
      }
    };
  }
};
