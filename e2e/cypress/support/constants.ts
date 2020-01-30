export type FlakyResponse = 'yes' | 'no';
export const runFlaky: FlakyResponse = Cypress.env('RUN_FLAKY');
export const itFlaky = runFlaky === 'yes' ? it : it.skip;
