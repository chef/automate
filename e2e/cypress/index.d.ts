declare namespace Cypress {
  // All added commands in commands.ts must be included in this interface.
  interface Chainable<Subject> {
    login(url: string, username: string): Cypress.Chainable<Object>
    adminLogin(url: string): Cypress.Chainable<Object>
    generateAdminToken(idToken: string): void
    logout(): Cypress.Chainable<Object>
    saveStorage(): void
    restoreStorage(): void
    applyProjectsFilter(projectsToFilterOn: string[]): void
    cleanupV2IAMObjectsByIDPrefixes(idPrefix: string, objectPlurals: string[]): void
    cleanupUsersByNamePrefix(namePrefix: string): void
    cleanupTeamsByDescriptionPrefix(namePrefix: string): void
    waitUntilApplyRulesNotRunning(attempts: number): void
  }
}
