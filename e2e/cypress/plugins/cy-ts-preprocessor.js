// This file was generated when running the js-to-ts conversion.
// See https://github.com/bahmutov/add-typescript-to-cypress

const wp = require('@cypress/webpack-preprocessor')

const webpackOptions = {
  resolve: {
    extensions: ['.ts', '.js']
  },
  module: {
    rules: [
      {
        test: /\.ts$/,
        exclude: [/node_modules/],
        use: [
          {
            loader: 'ts-loader'
          }
        ]
      }
    ]
  }
}

const options = {
  webpackOptions
}

module.exports = wp(options)
