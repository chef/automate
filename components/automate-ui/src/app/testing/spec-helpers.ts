import { Check } from 'app/components/authorized/authorized.component';

/*
  The typical use of this function is to create a data-driven test,
  where you want to exercise a given block of code with different
  sets of input. Example:

  using([
    [null, null, null, 'all nulls'],
    [null, null,   '', 'location & relation nulls'],
    [null,   '', null, 'location & constraint nulls'],
    [null,   '',   '', 'location null'],
    [  '', null, null, 'relation & constraint nulls'],
    [  '', null,   '', 'relation null'],
    [  '',   '', null, 'constraint null'],
    [  '',   '',   '', 'all empty']
  ],
  function(location, relation, constraint, description) {
    it('StoreIt coalesces any nulls to empty strings for ' + description,
        function() {
          // . . . Do things with location, relation, constraint
        });
  });

  Note that the first argument is an array of arrays, where each sub-array enumerates
  the parameters to the function immediately following the data,
  matching in number and kind.
  Convention is to add 'description' as the final parameter to be incorporated
  into the test name (the first argument to 'it').

  Note that instead of setting the type of the values argument to 'any[][]'
  I used just 'any[]' to allow for the built-in convenience code that
  allows you to write a single-parameter function with just a single array, i.e.:

  using(['one', 'two', 'three'],
  function(myParm) {
    it('Process with arg' + myParm,
        function() {
          // . . . Do things with myParm
        });
  });
 */


// adapted from: https://github.com/jphpsf/jasmine-data-provider/blob/master/spec/SpecHelper.js
export function using(values: any[], func: Function) {
  for (let i = 0, count = values.length; i < count; i++) {
    if (Object.prototype.toString.call(values[i]) !== '[object Array]') {
      values[i] = [values[i]];
    }
    func.apply(this, values[i]);
  }
}

export function checkFirstPerm(
    label: string,
    permsFromTemplate: string,
    permsFromLandingComp: Check[]) {
  // This issue (https://github.com/angular/angular/issues/28786)
  // limits `permsFromTemplate` to a max 30 chars, sufficient
  // only to check the first path and verb, hence the name of this function.
  // No way to guarantee a full check of all paths in one routeList element here. Sigh.

  // The flow and text of these expectations is designed to give meaningful information
  // when you edit either the template or the component but forget to do the other.
  // To see this, go into the sidebar template and either:
  // (1) change an 'anyOf' to an 'allOf' in an entry;
  // (2) change the path or verb inside an anyOf/allOf in an entry.
  if (!permsFromLandingComp) {
    expect(permsFromTemplate).toBeNull(
      label + ': is empty in ComplianceLanding but not in compliance-sidebar template');
    return;
  } else if (!permsFromTemplate) {
    expect(permsFromLandingComp).toBeNull(
      label + ': is not empty in ComplianceLanding but is in compliance-sidebar template');
    return;
  }
  const firstCheckItem = permsFromLandingComp[0];
  const [ path, verb ] = permsFromTemplate.split(',');
  // If permsFromTemplate path is more than 30 characters,
  // verb will be undefined. We will be unable to test the verb
  // and need to mark firstCheckItem[1] as undefined to pass test.
  // AND If permsFromTemplate verb charaters is less than the
  // firstCheckItem[1] we need to match the character length to pass test.
  firstCheckItem[1] = verb ? firstCheckItem[1].substring(0, verb.length) : undefined;
  // If permsFromTemplate path is more than 30 characters, the path
  // will be cutt off and firstCheckItem[0] will need to match
  // character length to pass test.
  expect(path).toBe(firstCheckItem[0].substring(0, 30));
  expect(verb).toBe(firstCheckItem[1]);
}
