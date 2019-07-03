describe('global projects filter', () => {
  const proj1 = { id: "cypress-project-1", name: "Cypress Project 1 " + Cypress.moment().format('MMDDYYhhmm') }
  const proj2 = { id: "cypress-project-2", name: "Cypress Project 2 " + Cypress.moment().format('MMDDYYhhmm')}
  const proj3 = { id: "cypress-project-3", name: "Cypress Project 3 " + Cypress.moment().format('MMDDYYhhmm') }
  // TODO uncomment with non-admin test
  // const pol_id = "cypress-policy"
  // const nonAdminUsername = "nonadmin"

  before(() => {
    cy.adminLogin('/').then(() => {
      let admin = JSON.parse(localStorage.getItem('chef-automate-user'))
      cy.cleanupProjects(admin.id_token)
      cy.createProject(admin.id_token, proj1)
      cy.createProject(admin.id_token, proj2)
      cy.createProject(admin.id_token, proj3)
      // TODO uncomment with non-admin test/ move up project creation
      // cy.createUser(admin.id_token, nonAdminUsername)
      // cy.createPolicy(admin.id_token, pol_id, nonAdminUsername, [proj1, proj2])
      cy.logout()
    })
  })

  it('shows all projects for admin', () => {
    cy.adminLogin('/settings')

    cy.get('chef-sidebar')
      .should('have.attr', 'minor-version')
      .then((version) => {
        if (version === 'v1') {
          cy.get('[data-cy=projects-filter-button]').click()
          
          const allowedProjects = [proj1.name, proj2.name, proj3.name, '(unassigned)'];
          // we don't check that projects in dropdown match *exactly* as
          // we can't control creation of other projects in the test env
          allowedProjects.forEach(project => {
            cy.get('[data-cy=projects-filter-dropdown]').contains(project)
          })
        }
      })
    cy.logout()
  })

  // TODO can uncomment when we have a flag to remove legacy policies,
  // which are currently allowing full project access to all users.
  // it('shows allowed projects for non-admin', () => {
  //   cy.login('/settings', nonAdminUsername)
  //   // hide modal unrelated to test flow
  //   cy.get('app-welcome-modal').invoke('hide')

  //   cy.get('chef-sidebar')
  //     .should('have.attr', 'minor-version')
  //     .then((version) => {
  //       if (version === 'v1') {
  //         cy.get('[data-cy=projects-filter-button]').click()

  //         const allowedProjects = [proj1, proj2];
  //         cy.get('[data-cy=projects-filter-dropdown] chef-checkbox')
  //           .should(($elements) => { expect($elements).to.have.length(allowedProjects.length) })
  //         allowedProjects.forEach(project => {
  //           cy.get('[data-cy=projects-filter-dropdown]').contains(project)
  //         })
  //       }
  //     })
  //   cy.logout()
  // })
})
