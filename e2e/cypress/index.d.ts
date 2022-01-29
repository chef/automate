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
    cleanupIAMObjectsByIDPrefixes(idPrefix: string, objectPlurals: string[]): void
    applyRulesAndWait(): void
    waitForNodemanagerNode(nodeId: string): void
    waitForClientRunsNode(nodeId: string): void
    waitForComplianceNode(nodeId: string, start: string, end: string): void
    waitForAction(entityName: string, start: string, end: string): void
    deleteClientRunsNode(clientRunsNodeId: string): void
    waitUntilRunIsIngested(clientRunsNodeId: string, runId: string): void
    waitUntilNodeIsMissing(clientRunsNodeId: string): void
    waitUntilNodemanagerNodeIsDeleted(nodeName: string): void
    waitUntilConfigMgmtNodeIsDeleted(clientRunsNodeId: string): void
    sendToDataCollector(report: any): void
  }
}
