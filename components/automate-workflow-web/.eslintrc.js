// http://eslint.org
// http://eslint.org/docs/user-guide/configuring

module.exports = {

  parserOptions: {

    // We organize our code into ECMAScript modules
    sourceType: 'module'
  },

  // Begin by applying ESLint recommendations
  // http://eslint.org/docs/user-guide/configuring#using-eslintrecommended
  extends: 'eslint:recommended',

  // Additional rules want to apply
  rules: {

    // Two-space indents
    indent: ['error', 2],

    // Single-quoted strings (backticks okay too, when substituting)
    quotes: ['error', 'single'],

    // Explicit semicolons
    semi: ['error', 'always']
  },

  // Common build & runtime definitions supplied by ESLint
  env: {

    // Browser globals (window, etc.)
    browser: true,

    // NodeJS globals (global, process, etc.)
    node: true,

    // All ECMAScipt 6 features, 'cause we use it
    es6: true,

    // Jasmine (describe, it, etc. http://jasmine.github.io/)
    jasmine: true,

    // Protractor (browser, etc. http://www.protractortest.org/#/)
    protractor: true
  },

  // Additional globals not covered by the above
  globals: {

    // AngularJS
    angular: true,
    inject: true,

    // Our custom E2E helpers
    featureFlag: true,
    login: true,
    logout: true,
    mockApi: true,
    mockSse: true
  }
};
