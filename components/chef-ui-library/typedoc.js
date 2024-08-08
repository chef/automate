module.exports = {
  // "mode": "file",
  "json": "src/assets/docs.json",
  // "ignoreCompilerErrors": true,
  // "experimentalDecorators": true,
  // "emitDecoratorMetadata": true,
  "excludeNotDocumented": true,
  "exclude": "**/*+(index|.spec|.e2e).tsx",
  "skipErrorChecking": true,
  "plugin": "./typeDocPlugin.js"
};
