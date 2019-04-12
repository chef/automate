describe('team management', () => {
  before(() => {
    cy.login('/settings/teams').then(() => {
      // clean up leftover teams in case of previous test failures
      // TODO...
    })
  })

  beforeEach(() => {
    cy.restoreStorage()
  })
  afterEach(() => {
    cy.saveStorage()
  }) 

  it('lists system teams', () => {
    cy.contains('Create Team')
    cy.get('chef-sidebar')
      .should('have.attr', 'major-version')
      .then((version) => {
        switch (version) {
          case 'v2': {
            cy.get('#table-container chef-th').contains('ID')
            cy.get('#table-container chef-th').contains('Name')

            const systemTeams = ['admins', 'editors', 'viewers'];
            systemTeams.forEach(name => {
            cy.get('#table-container chef-tr').contains(name)
              .parent().parent().find('chef-control-menu').as('control-menu')
            })
             break;
          }
          default: {
            cy.get('#table-container chef-th').contains('Name')
            cy.get('#table-container chef-th').contains('Description')

            const systemTeams = ['admins'];
            systemTeams.forEach(name => {
            cy.get('#table-container chef-tr').contains(name)
              .parent().parent().find('chef-control-menu').as('control-menu')
            })
 
          }
        }
      });
 })

  it('displays team details for admins team', () => {
    cy.get('chef-td').contains('admins').click()

    cy.get('chef-breadcrumbs').contains('Teams')
    cy.get('chef-breadcrumbs').contains('admins')

    cy.get('.page-title').contains('admins')
    cy.contains('Add User')
  })

  it('displays team users for admins team', () => {
    cy.get('chef-option').contains('Users')
    cy.get('app-user-table chef-th').contains('Name')
    cy.get('app-user-table chef-th').contains('Username')
    cy.get('app-user-table chef-td').contains('Local Administrator')
    cy.get('app-user-table chef-td').contains('admin')
  })
})
