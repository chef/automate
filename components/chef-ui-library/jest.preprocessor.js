var ts = require('typescript');
var tsConfig = require('./tsconfig.jest.json');
var stencilPreprocessor = require('@stencil/core/testing/jest.preprocessor.js');

module.exports = {
  process: function(src, path) {
    if (path.endsWith('.d.ts')) {
      return '';
    }

    if (path.endsWith('.tsx')) {
      return stencilPreprocessor.process(src, path);
    }

    else {
      return ts.transpile(
        src,
        tsConfig.compilerOptions,
        path,
        []
      );
    }
  }
};
