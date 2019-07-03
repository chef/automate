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

Cypress.Commands.add("adminLogin", (url) => {
  // CYPRESS_BASE_URL environment variable must be set
  cy.login(url, 'admin')
})

Cypress.Commands.add("login", (url, username) => {
  // CYPRESS_BASE_URL environment variable must be set
  cy.visit(url)

  // only environments using SAML or LDAP present this login method selection
  cy.location('pathname')
    .then(path => path.startsWith('/dex/auth/local'))
    .then(local => {
      if (local) {
        login(username)
      } else {
        cy.get('button').contains('Log in with Username').click().then(() => login(username))
      }
    })
})

function login(username) {
  cy.url().should('include', '/dex/auth/local')
  cy.server()
  cy.route('POST', '/api/v0/auth/introspect_some').as('getAuth')

  // login
  cy.get('#login').type(username)
  cy.get('#password').type('chefautomate')

  cy.get('[type=submit]').click().then(() => {
    expect(localStorage.getItem('chef-automate-user')).to.contain(username)

    // close welcome modal if present
    cy.get('app-welcome-modal').invoke('hide')
    cy.saveStorage()

    cy.wait('@getAuth')
  })
}

Cypress.Commands.add("logout", () => {
  cy.get('[data-cy=user-profile-button]').click()
  cy.get('[data-cy=sign-out-button]').click()
  cy.url().should('include', '/dex/auth')
})

// IAM helpers
// TODO bhd 3/1/19: make this generic
Cypress.Commands.add("cleanupUsers", (id_token) => {
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
})

Cypress.Commands.add("cleanupProjects", (id_token) => {
  cy.request({
    auth: { bearer: id_token },
    method: 'GET',
    url: '/apis/iam/v2beta/projects',
    failOnStatusCode: false
  }).then((resp) => {
    for (let project of resp.body.projects) {
      cy.request({
        auth: { bearer: id_token },
        method: 'DELETE',
        url: `/apis/iam/v2beta/projects/${project.id}`,
        failOnStatusCode: false
      })
    }
  })
})

Cypress.Commands.add("createUser", (id_token, username) => {
  cy.request({
    auth: { bearer: id_token },
    method: 'POST',
    url: '/apis/iam/v2beta/users',
    failOnStatusCode: false,
    body: {
      id: username,
      name: "cypress test user",
      password: "chefautomate"
    }
  })
})

Cypress.Commands.add("createPolicy", (id_token, id, username, projects) => {
  cy.request({
    auth: { bearer: id_token },
    method: 'POST',
    url: '/apis/iam/v2beta/policies',
    failOnStatusCode: false,
    body: {
      id,
      name: "non-admin policy",
      members: ["user:local:" + username],
      statements: [
        {
          effect: "ALLOW",
          actions: ["iam:teams:list", "iam:teams:get"],
          projects
        }
      ]
    }
  }).then((response) => {
    expect([200, 409]).to.include(response.status)
  })
})

Cypress.Commands.add("createProject", (id_token, { id, name }) => {
  cy.request({
    auth: { bearer: id_token },
    method: 'POST',
    url: '/apis/iam/v2beta/projects',
    failOnStatusCode: false,
    body: {
      id,
      name
    }
  }).then((response) => {
    expect([200, 409]).to.include(response.status)
  })
})
