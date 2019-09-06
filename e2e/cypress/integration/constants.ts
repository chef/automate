export const iamVersion = <string><Object>Cypress.env('IAM_VERSION') || 'v2.0';
export const describeIfIAMV2 = iamVersion.match(/v2/) ? describe : describe.skip;
export const describeIfIAMV2p1 = iamVersion === 'v2.1' ? describe : describe.skip;
