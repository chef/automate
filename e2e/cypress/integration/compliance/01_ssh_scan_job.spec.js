describe('create a manual node ssh scan job and cleanup after', () => {
  before(() => {
    cy.login('/')
  })
  beforeEach(() => {
    // cypress clears local storage between tests
    cy.restoreStorage()
  })
  afterEach(() => {
    cy.saveStorage()
  })
  const nowTime = Cypress.moment().format('YYYY-MM-DD hh:mm')
  const credName = 'a test ' + nowTime
  const nodePrefix = '0-' + nowTime
  const jobName = 'job ' + nowTime
  it('can create a credential with a username and password', () => {
    // navigate to credentials create page:
    // click on settings
    cy.get('.nav-link').contains('Settings').click()

    // we save the route that will be called when we navigate to the page
    // in order to be able to wait for it later
    cy.route('POST', '/api/v0/secrets/search').as('getSecrets')

    // click on node credentials in sidebar
    cy.get('chef-sidebar-entry').contains('Node Credentials').click()
    cy.url().should('include', '/settings/node-credentials')

    // wait for data to return
    cy.wait('@getSecrets')

    // click on add credential button to open create form
    cy.contains('Add Credential').click().then(() => {
      cy.url().should('include', '/settings/node-credentials/add')

      // fill in name for credential, username, and password
      cy.get('form input[formcontrolname="name"]').first().type(credName)
      cy.get('form input[formcontrolname="username"]').first().type(Cypress.env('AUTOMATE_ACCEPTANCE_TARGET_USERNAME'))
      cy.get('form input[formcontrolname="password"]').first().type(Cypress.env('AUTOMATE_ACCEPTANCE_TARGET_PASSWORD'))

      // we save the route that will be called when we navigate to the page
      // in order to be able to wait for it later
      cy.route('POST', '/api/v0/secrets').as('createSecret')

      // save the credential
      cy.contains('Save').click().then(() => {

        cy.url().should('include', '/settings/node-credentials')

        // wait for data to return
        cy.wait('@createSecret')
        cy.wait('@getSecrets')

        // assert table has credential
        cy.contains(credName).should('exist')
      })
    })
  })

  it('can create a node and attach a credential', () => {
    // navigate to node create page:
    // click on scan jobs
    cy.get('.nav-link').contains('Scan Jobs').click()
    cy.url().should('include', '/compliance/scanner/jobs')

    // we save the route that will be called when we navigate to the page
    // in order to be able to wait for it later
    cy.route('POST', '/api/v0/nodes/search').as('getNodes')

    // click on nodes tab
    cy.get('.nav-tab').contains('Nodes added').click().then(() => {
      cy.url().should('include', '/compliance/scanner/nodes')

      // wait for data to return
      cy.wait('@getNodes')

      // click on add node button to open create form
      cy.contains('Add Nodes').parent().invoke('show').click().then(() => {
        cy.url().should('include', '/compliance/scanner/nodes/add')

        // fill in hostname and select a credential for the node
        cy.get('form input[formcontrolname="hosts"]').type(Cypress.env('AUTOMATE_ACCEPTANCE_TARGET_HOST'))
        // add a prefix to ensure the node is at top of list
        cy.get('form input[formcontrolname="customPrefix"]').type(nodePrefix)
        cy.get('form select[formcontrolname="secrets"]').select(credName)

        // we save the route that will be called when we navigate to the page
        // in order to be able to wait for it later
        cy.route('POST', '/api/v0/nodes/bulk-create').as('createNode')

        // save the node
        cy.get('chef-button').contains('Add 1 Node(s)').click().then(() => {

          cy.url().should('include', '/compliance/scanner/nodes')

          // wait for data to return
          cy.wait('@createNode')

          // assert table has node
          cy.contains(nodePrefix).should('exist')
        })
      })
    })
  })

  it('can install a profile', () => {
    // we save the route that will be called when we navigate to the page
    // in order to be able to wait for it later
    cy.route('POST', '/api/v0/compliance/profiles/search').as('getProfiles')

    // navigate to asset store
    cy.get('.nav-link').contains('Asset Store').click()
    cy.url().should('include', '/profiles')

    // wait for data to return
    cy.wait('@getProfiles')

    // the ideal here would be to ensure the profiles call we're waiting on
    // is the available profiles call, but we can't specify the request body we are
    // looking for when using cy.route, so instead we just wait a little more time
    cy.wait(5000)

    // click on market profiles tab
    cy.get('chef-option[value="available"]').click().then(() => {
      cy.contains('CIS Amazon Linux 2 Benchmark Level 1').parent().parent().as('row')
      cy.get('@row').contains('Get').parent().invoke('show').parent().invoke('show').click().then(() => {
        // assert table has profile
        cy.contains('CIS Amazon Linux 2 Benchmark Level 1').should('exist')
      })
    })
  })

  it('can create a scan job with a reccurence schedule', () => {
    // navigate to scan create page:
    // click on scan jobs
    cy.get('.nav-link').contains('Scan Jobs').click()
    cy.url().should('include', '/compliance/scanner/jobs')

    // we save the route that will be called when we navigate to the next page
    // in order to be able to wait for it later
    cy.route('POST', '/api/v0/compliance/profiles/search').as('getProfiles')

    // open scan create form
    cy.contains('Create new job').click().then(() => {
      cy.url().should('include', '/jobs/add')

      // select nodes
      cy.get('chef-job-nodes-form .manager-header').contains('Automate').as('manager-row')
      cy.get('@manager-row').parent().parent().find('input[type="checkbox"]').check()

      // click next
      cy.contains('Next').click().then(() => {
        // wait for data to return
        cy.wait('@getProfiles')

        // select profiles
        cy.get('chef-job-profiles-form').find('[data-cy=select-all-profiles]').check().then(() => {
          // click next
          cy.contains('Next').click().then(() => {

            // give the scan job a name
            cy.get('form input[formcontrolname="name"]').type('a ' + jobName)

            // expand the recurrence schedule and schedule job to repeat every 1 day
            cy.get('chef-job-schedule-form input[type="checkbox"]').check()
            cy.get('.schedule-body input[type="checkbox"]').check()
            cy.get('fieldset[formgroupname="repeat"]').get('select').last().select('Days')

            // click save
            cy.contains('Save').click().then(() => {
              cy.url().should('include', '/compliance/scanner/jobs')

              // assert table has scan job
              cy.contains(jobName).should('exist')

              // click into the scan job to view runs page
              cy.contains(jobName)
                .click().then(() => {
                  cy.url().should('include', '/scans')
                })
            })
          })
        })
      })
    })
  })

  it('can delete the created credential', () => {
    // navigate to credentials create page:
    // click on settings
    cy.get('.nav-link').contains('Settings').click()

    // we save the route that will be called when we navigate to the page
    // in order to be able to wait for it later
    cy.route('POST', '/api/v0/secrets/search').as('getSecrets')

    // click on node credentials in sidebar
    cy.get('chef-sidebar-entry').contains('Node Credentials').click()
    cy.url().should('include', '/settings/node-credentials')

    // wait for data to return
    cy.wait('@getSecrets')

    // delete the credential
    cy.contains(credName).parent().parent().find('chef-control-menu').as('row-menu')
    cy.get('@row-menu').click().then(() => {
      cy.get('@row-menu').find('[data-cy=delete]').click()
    })
  })
  it('can delete the created node', () => {
    // navigate to node create page:
    // click on scan jobs
    cy.get('.nav-link').contains('Scan Jobs').click()
    cy.url().should('include', '/compliance/scanner/jobs')

    // we save the route that will be called when we navigate to the page
    // in order to be able to wait for it later
    cy.route('POST', '/api/v0/nodes/search').as('getNodes')

    // click on nodes tab
    cy.get('.nav-tab').contains('Nodes added').click().then(() => {
      cy.url().should('include', '/compliance/scanner/nodes')

      // wait for data to return
      cy.wait('@getNodes')

      // delete the node
      cy.contains(nodePrefix)
        .parent().find('chef-button[label="delete"]').click()
    })
  })
  it('can delete the created profile', () => {
    // navigate to asset store
    cy.get('.nav-link').contains('Asset Store').click()
    cy.url().should('include', '/profiles')

    // delete profile
    cy.contains('CIS Amazon Linux 2 Benchmark Level 1')
      .click().then(() => {
        cy.get('chef-page-header').find('#uninstall-btn').click().then(() => {
          cy.get('chef-modal').find('chef-button').contains('Delete').click()
        })
      })

    // ensure that the profile is deleted
    cy.url().should('include', '/profiles')
    cy.get('chef-tbody').should(($b) => {
      expect($b).not.to.contain('CIS Amazon Linux 2 Benchmark Level 1')
    })
  })
  it('can delete the created scan job', () => {
    // save the route for the delete request so that we can ensure the
    // response is returned
    cy.route('DELETE', '/api/v0/compliance/scanner/jobs/id/**').as('deleteScanJob')

    // navigate to scan create page:
    // click on scan jobs
    cy.get('.nav-link').contains('Scan Jobs').click()
    cy.url().should('include', '/compliance/scanner/jobs')

    // delete scan job
    cy.contains(jobName)
      .parent().parent().find('chef-button[label="delete"]').click().then(() => {
        cy.get('chef-modal').find('chef-button').contains('Delete').click()
      })

    // wait for the delete to finish:

    // this line, along with the expect below are here to make sure
    // that we're actually deleting the scan jobs at the end of this
    // test. we've run into scenarios in our long-lived test
    // environments where the scan job doesn't end up getting deleted
    // and jobs start failing once we accumulate more than 100 jobs
    // and the results are paginated
    cy.wait('@deleteScanJob')

    // ensure that the job is deleted
    cy.url().should('include', '/compliance/scanner/jobs')
    cy.get('chef-tbody').should(($b) => {
      expect($b).not.to.contain(jobName)
    })
  })
})
