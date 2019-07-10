declare namespace Cypress {
  interface Chainable<Subject> {
    login(url: string, username: string): Cypress.Chainable<Object>
    adminLogin(url: string): Cypress.Chainable<Object>
    logout(): Cypress.Chainable<Object>
    saveStorage(): void
    restoreStorage(): void
  }
}
