/// <reference path="../../../node_modules/@types/jasmine/index.d.ts" />

declare namespace jasmine {
  interface Matchers<T> {
    toContainPath(path: string): boolean;
  }
}
