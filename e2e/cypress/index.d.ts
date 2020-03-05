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
    applyRulesAndWait(attempts: number): void
    waitForNodemanagerNode(nodeId: string, maxRetries: number): void
    waitForClientRunsNode(nodeId: string, maxRetries: number): void
    waitForComplianceNode(nodeId: string, start: string, end: string,
      maxRetries: number): void
  }
}
