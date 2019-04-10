let LOCAL_STORAGE_MEMORY = {};
let SESSION_MEMORY = {};

Cypress.Commands.add("saveStorage", () => {
  Object.keys(localStorage).forEach(key => {
    LOCAL_STORAGE_MEMORY[key] = localStorage[key];
  });
  Object.keys(sessionStorage).forEach(key => {
    SESSION_MEMORY[key] = sessionStorage[key];
  });
});

Cypress.Commands.add("restoreStorage", () => {
  Object.keys(LOCAL_STORAGE_MEMORY).forEach(key => {
    localStorage.setItem(key, LOCAL_STORAGE_MEMORY[key]);
  });
  Object.keys(SESSION_MEMORY).forEach(key => {
	  sessionStorage.setItem(key, SESSION_MEMORY[key]);
  });
  
  cy.server()
  // mock refresh token call in case it fails
  let user = JSON.parse(localStorage.getItem('chef-automate-user'))
  cy.route({
    method: 'GET',
    url: '**/session/refresh',
    status: 200,
    response: {
      id_token: user.id_token
    }
  })
});

Cypress.Commands.add("login", (url) => {
  // CYPRESS_BASE_URL environment variable must be set
  cy.visit(url)
  cy.get('button').contains('Log in with Username').click().then(() => {
    cy.url().should('include', '/dex/auth/')

    // login
    cy.get('#login')
      .type('admin')

    cy.get('#password')
      .type('chefautomate')

    cy.get('[type=submit]').click().then(() => {
      expect(localStorage.getItem('chef-automate-user')).to.contain('admin')
    
      // close welcome modal
      cy.get('[data-cy=close-welcome]').first().click()
      cy.saveStorage()
    })
  })
})

// TODO bhd 3/1/19: make this generic
Cypress.Commands.add("cleanupUsers", (id_token) => {
  cy.request({
    auth: { bearer: id_token },
    method: 'GET',
    url: `/api/v0/auth/users`,
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
})
