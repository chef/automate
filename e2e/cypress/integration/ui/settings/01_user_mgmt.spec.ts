describe('user management', () => {
  before(() => {
    cy.adminLogin('/settings/users').then(() => {

      // clean up leftover users in case of previous test failures
      let admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'))
      cleanupUsers(admin.id_token)
    })
  })

  beforeEach(() => {
    cy.restoreStorage()
  })
  afterEach(() => {
    cy.saveStorage()
  }) 

  const now = Cypress.moment().format('MMDDYYhhmm')
  const name = 'cypress test user ' + now
  const username = 'testing' + now

  it('can create a user', () => {
    cy.get('app-user-table').should('exist')

    // open modal
    cy.get('app-user-table chef-button').contains('Create User').click()
    cy.get('app-user-management chef-modal').should('exist')

    cy.get('[formcontrolname=fullname]')
      .type(name)

    cy.get('[formcontrolname=username]')
      .type(username)

    cy.get('[formcontrolname=password]')
      .type('chefautomate')

    cy.get('[formcontrolname=confirmPassword]')
      .type('chefautomate').then(() => {
        // save new user
        cy.get('[data-cy=save-user]').click().then(() => {
          // confirm modal closed
          cy.get('app-user-management chef-modal').should('not.be.visible')
          // success alert displays
          cy.get('chef-notification.info').should('be.visible')
          // confirm new user row added
          cy.contains(username).should('exist')
          cy.contains(name).should('exist')
        })
      })
  })

  // it("can view and edit a user's details", () => {
  //   let updated_name = name + 'update'
  //   let updated_password = 'chefautomate1'
  //   cy.contains(name).click().then(() => {
  //     cy.get('app-user-details').should('exist')
  //     cy.get('div.name-column').contains(name).should('exist')
  //     cy.get('span').contains(username).should('exist')
  //     cy.get('chef-form-field').contains('New Password').should('exist')
  //     cy.get('chef-form-field').contains('Confirm New Password').should('exist')

  //     cy.get('chef-button.edit-button').click().then(() => {
  //       cy.get('[formcontrolname=fullName]').find('input').type(updated_name)

  //       cy.get('chef-button.save-button').click(() => {
  //         cy.get('div.name-column').contains(updated_name).should('exist')
  //       })
  //     })

  //     cy.get('[formcontrolname=newPassword]').find('input').type(updated_password).then(() => {
  //       cy.get('[formcontrolname=confirmPassword]').find('input').type(updated_password).then(() => {
  //         cy.get('chef-button').contains('Update Password').click().then(() => {
  //           // success alert displays
  //           cy.get('chef-notification.info').should('be.visible')

  //           // back to user list page
  //           cy.get('.breadcrumb').contains('Users').click()
  //         })
  //       })
  //     })
  //   })
  // })

  // it("can delete user", () => {
  //   // find the created user row
  //   cy.get('chef-tbody chef-tr').contains(name).parent().parent().find('chef-control-menu').as('control-menu')

  //   cy.get('@control-menu').click().then(() => {

  //     // force:true disables waiting for actionability 
  //     // https://docs.cypress.io/guides/core-concepts/interacting-with-elements.html#Actionability
  //     // in this case, the delete button is hidden under the control menu so we decrease test flakiness
  //     // by having less stringent checks before clicking
  //     cy.get('@control-menu').find('[data-cy=delete]').click({ force: true }).then(() => {
  //       // confirm user delete
  //       cy.get('chef-button').contains('Delete User').click().then(() => {
  //         cy.get('chef-tbody chef-td').contains(username).should('not.exist')
  //       })
  //     })
  //   })
  // })
})

function cleanupUsers(id_token: string):void {
  cy.request({
    auth: { bearer: id_token },
    method: 'GET',
    url: '/api/v0/auth/users',
    failOnStatusCode: false
  }).then((resp) => {
    let body = resp.body
    for (let user of body.users) {
      if (user.name.startsWith('cypress test user ')) {
        cy.request({
          auth: { bearer: id_token },
          method: 'DELETE',
          url: `/api/v0/auth/users/${user.username}`,
          failOnStatusCode: false
        })
      }
    }
  })
}
