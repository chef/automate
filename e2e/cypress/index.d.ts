declare namespace Cypress {
  interface Chainable<Subject> {
    login(url: string, username: string): Cypress.Chainable<any>
    adminLogin(url: string): Cypress.Chainable<any>
    logout(): Cypress.Chainable<any>
    saveStorage(): void
    restoreStorage(): void
  }
}
