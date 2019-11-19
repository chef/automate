export type IAMVersion = 'v1' | 'v2.1';
export type FlakyResponse = 'yes' | 'no';
export const iamVersion: IAMVersion = Cypress.env('IAM_VERSION');
export function isV1() { return iamVersion === 'v1'; }
export const describeIfIAMV2p1 = iamVersion === 'v2.1' ? describe : describe.skip;
export const runFlaky: FlakyResponse = Cypress.env('RUN_FLAKY');
export const itFlaky = runFlaky === 'yes' ? it : it.skip;
