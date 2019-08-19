declare namespace Cypress {
  // All added commands in commands.ts must be included in this interface.
  interface Chainable<Subject> {
    login(url: string, username: string): Cypress.Chainable<Object>
    adminLogin(url: string): Cypress.Chainable<Object>
    logout(): Cypress.Chainable<Object>
    saveStorage(): void
    restoreStorage(): void
    applyProjectsFilter(projectsToFilterOn: string[]): void
    generateAdminToken(idToken: string): void
    cleanupPoliciesByIDPrefix(idToken: string, idPrefix: string): void
    cleanupProjectsByIDPrefix(idToken: string, idPrefix: string): void
    cleanupUsersByNamePrefix(idToken: string, namePrefix: string): void
    cleanupTeamsByDescriptionPrefix(idToken: string, namePrefix: string): void
  }
}
