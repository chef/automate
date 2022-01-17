// This file was generated when running the js-to-ts conversion.
// See https://github.com/bahmutov/add-typescript-to-cypress

const cypressTypeScriptPreprocessor = require('./cy-ts-preprocessor')

// This function is called when a project is opened or re-opened (e.g. due to
// the project's config changing)

module.exports = on => {
  // `on` is used to hook into various events Cypress emits
  on('file:preprocessor', cypressTypeScriptPreprocessor)
}
