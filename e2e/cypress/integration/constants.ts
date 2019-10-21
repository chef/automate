export type IAMVersion = 'v1' | 'v2' | 'v2.1';
export const iamVersion: IAMVersion = Cypress.env('IAM_VERSION') || 'v2';
export const describeIfIAMV2 = iamVersion.match(/v2/) ? describe : describe.skip;
export const describeIfIAMV2p1 = iamVersion === 'v2.1' ? describe : describe.skip;
export const runFlaky: boolean = Cypress.env('RUN_FLAKY') || false;
export const itFlaky = runFlaky ? it : it.skip;
