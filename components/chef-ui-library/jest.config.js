module.exports = {
  transform: {
    '^.+\\.(ts|tsx)$': '<rootDir>/jest.preprocessor.js'
  },
  testRegex: '(/__tests__/.*|\\.(test|spec))\\.(tsx?|jsx?)$',
  "modulePathIgnorePatterns": [
    "<rootDir>/dist/",
    "<rootDir>/www/"
  ],
  moduleFileExtensions: [
    'ts',
    'tsx',
    'js',
    'json',
    'jsx'
  ]
};
