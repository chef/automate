describe('global projects filter', () => {
  const nonAdminUsername = "nonadmin"
  const proj1  = "cypress-project-1"
  const proj2  = "cypress-project-2"
  const proj3  = "cypress-project-3"
  const pol_id = "cypress-policy"

  before(() => {
    cy.adminLogin('/').then(() => {
      let admin = JSON.parse(localStorage.getItem('chef-automate-user'))

      cy.createProject(admin.id_token, proj1)
      cy.createProject(admin.id_token, proj2)
      cy.createProject(admin.id_token, proj3)
      cy.createUser(admin.id_token, nonAdminUsername)
      cy.createPolicy(admin.id_token, pol_id, nonAdminUsername, [proj1, proj2])
      cy.logout()
    })
  })

  after(() => {
    cy.adminLogin('/').then(() => {
      let admin = JSON.parse(localStorage.getItem('chef-automate-user'))
      cy.cleanupProjects(admin.id_token)
      cy.cleanupUsers(admin.id_token)
      cy.request({
        auth: { bearer: admin.id_token },
        method: 'DELETE',
        url: `/apis/iam/v2beta/policies/${pol_id}`,
        failOnStatusCode: false
      }).then((response) => {
        expect([200, 404]).to.include(response.status)
      })
    })
  })

  it('shows all projects for admin', () => {
    cy.adminLogin('/settings')
    // hide modal unrelated to test flow
    cy.get('app-welcome-modal').invoke('hide')

    cy.get('chef-sidebar')
      .should('have.attr', 'minor-version')
      .then((version) => {
        if (version === 'v1') {
          cy.get('[data-cy=projects-filter-button]').click()
          
          const allowedProjects = [proj1, proj2, proj3, '(unassigned)'];
          cy.get('[data-cy=projects-filter-dropdown] chef-checkbox')
            .should(($elements) => { expect($elements).to.have.length(allowedProjects.length) })
          allowedProjects.forEach(project => {
            cy.get('[data-cy=projects-filter-dropdown]').contains(project)
          })
        }
      })
    cy.logout()
  })

  it('shows allowed projects for non-admin', () => {
    cy.login('/settings', nonAdminUsername)
    // hide modal unrelated to test flow
    cy.get('app-welcome-modal').invoke('hide')

    cy.get('chef-sidebar')
      .should('have.attr', 'minor-version')
      .then((version) => {
        if (version === 'v1') {
          cy.get('[data-cy=projects-filter-button]').click()

          const allowedProjects = [proj1, proj2];
          cy.get('[data-cy=projects-filter-dropdown] chef-checkbox')
            .should(($elements) => { expect($elements).to.have.length(allowedProjects.length) })
          allowedProjects.forEach(project => {
            cy.get('[data-cy=projects-filter-dropdown]').contains(project)
          })
        }
      })
    cy.logout()
  })
})
