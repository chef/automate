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
