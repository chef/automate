import _ from 'lodash';

export default {
 
  toBePresent: () => {
    return {
      compare: (element) => {
        let result = {
          pass: element.isPresent().then((isPresent) => {
            let pass = !!isPresent;
            result.message = `Expected${(pass ? ' NOT ' : '')} to be present`;
            return pass;
          })
        };
        return result;
      }
    };
  },
 
  toBeDisplayed: () => {
    return {
      compare: (element) => {
        let result = {
          pass: element.isDisplayed().then((isDisplayed) => {
            let pass = !!isDisplayed;
            result.message = `Expected${(pass ? ' NOT ' : '')} to be displayed`;
            return pass;
          })
        };
        return result;
      }
    };
  },
 
  toContainText: () => {
    return {
      compare: (element, expectedText) => {
        let result = {
          pass: element.getText().then((actualText) => {
            let pass = actualText.indexOf(expectedText) >= 0;
            if (pass) {
              result.message = `Expected NOT to contain text ${expectedText}`;
            } else {
              result.message = `Expected to contain text ${expectedText} BUT text is ${actualText}`;
            }
            return pass;
          })
        };
        return result;
      }
    };
  },
 
  toHaveExactText: () => {
    return {
      compare: (element, expectedText) => {
        let result = {
          pass: element.getText().then((actualText) => {
            let pass = actualText === expectedText;
            if (pass) {
              result.message = `Expected NOT to have text ${expectedText}`;
            } else {
              result.message = `Expected to have text ${expectedText} BUT has text ${actualText}`;
            }
            return pass;
          })
        };
        return result;
      }
    };
  },
 
  toHaveValue: () => {
    return {
      compare: (element, expectedValue) => {
        let result = {
          pass: element.getAttribute('value').then((actualValue) => {
            let pass = actualValue === expectedValue;
            if (pass) {
              result.message = `Expected NOT to have value ${expectedValue}`;
            } else {
              result.message = `Expected to have value ${expectedValue} BUT has value ${actualValue}`;
            }
            return pass;
          })
        };
        return result;
      }
    };
  },
 
  toBeChecked: () => {
    return {
      compare: (element) => {
        let result = {
          pass: element.getAttribute('checked').then((checked) => {
            let pass = checked === 'true';
            result.message = `Expected${(pass ? ' NOT ' : '')} to be checked`;
            return pass;
          })
        };
        return result;
      }
    };
  },
 
  toHaveClass: () => {
    return {
      compare: (element, expectedClasses) => {
        let result = {
          pass: element.getAttribute('class').then((actualClasses) => {
            let actualClassesArr = actualClasses.split(/\s/),
              expectedClassesArr = expectedClasses.split(/\s/),
              notSatisfiedClassesArr = _.difference(expectedClassesArr, actualClassesArr);
 
            if (expectedClassesArr.length === 1) {
              result.message = `Expected to have class ${expectedClassesArr[0]}`;
            } else {
              result.message = `Expected to have classes ${expectedClassesArr.join(', ')} but does not have classes ${notSatisfiedClassesArr.join(', ')}`;
            }
 
            return notSatisfiedClassesArr.length === 0;
 
          })
        };
        return result;
      },
 
      negativeCompare: (element, forbiddenClasses) => {
        let result = {
          pass: element.getAttribute('class').then(function (actualClasses) {
            let actualClassesArr = actualClasses.split(/\s/),
              forbiddenClassesArr = forbiddenClasses.split(/\s/),
              satisfiedClassesArr = _.intersection(forbiddenClassesArr, actualClassesArr);
 
            if (forbiddenClassesArr.length === 1) {
              result.message = `Expected to NOT have class ${forbiddenClassesArr[0]}`;
            } else {
              result.message = `Expected to NOT have classes ${forbiddenClassesArr.join(', ')} but does have classes ${satisfiedClassesArr.join(', ')}`;
            }
 
            return satisfiedClassesArr.length === 0;
 
          })
        };
        return result;
      }
    };
  }
}
