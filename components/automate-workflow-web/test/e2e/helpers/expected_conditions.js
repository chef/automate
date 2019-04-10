let ec = {
  presenceOf: (selector) => protractor.ExpectedConditions.presenceOf($(selector)),
  urlContains: (str) => protractor.ExpectedConditions.urlContains(str)
};

export default ec;

global.presenceOf = ec.presenceOf;
global.urlContains = ec.urlContains;
