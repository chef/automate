const avengersProject = {
  id: `avengers-project-${Cypress.moment().format('MMDDYYhhmm')}`,
  name: "Test Avengers Project"
}

const xmenProject = {
  id: `xmen-project-${Cypress.moment().format('MMDDYYhhmm')}`,
  name: "Test X-men Project"
}

const avengersRule = {
  id: "avengers-rule-1",
  name: "first rule of avengers project",
  type: "NODE",
  project_id: avengersProject.id,
  conditions: [
    {
      attribute: "CHEF_ORGS",
      operator: "EQUALS",
      values: ["avengers"]
    }
  ]
}

const xmenRule = {
  id: "xmen-rule-1",
  name: "first rule of xmen project",
  type: "NODE",
  project_id: xmenProject.id,
  conditions: [
    {
      attribute: "CHEF_ORGS",
      operator: "EQUALS",
      values: ["xmen"]
    }
  ]
}

describe('projects API', () => {

  if (Cypress.env('IAM_VERSION') !== 'v2.1') {
    describe('applying project rules', () => {
      it.skip('must be run on IAM v2.1')
    })
  } else {
    describe('applying project rules', () => {
      before(() => {
        cy.adminLogin('/').then(() => {
          let admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'))

          // Cypress recommends state cleanup in the before block to ensure
          // it gets run every time:
          // https://docs.cypress.io/guides/references/best-practices.html#Using-after-or-afterEach-hooks
          cleanupTestProjects(admin.id_token)

          cy.request({
            auth: { bearer: admin.id_token },
            method: 'POST',
            url: 'api/v0/ingest/events/chef/node-multiple-deletes',
            body: {
              node_ids: [
                "f6a5c33f-bef5-433b-815e-a8f6e69e6b1b",
                "82760210-4686-497e-b039-efca78dee64b",
                "9c139ad0-89a5-44bc-942c-d7f248b155ba",
                "6453a764-2415-4934-8cee-2a008834a74a"
              ]
            },
            failOnStatusCode: false,
          })

          // user our Admin user's ID token to generate an admin-level API token
          // for use in the tests
          cy.generateAdminToken(admin.id_token)

          // create projects or confirm they already exist
          for (let project of [avengersProject, xmenProject]) {
            cy.request({
              headers: { 'api-token': Cypress.env('adminTokenValue') },
              method: 'POST',
              url: '/apis/iam/v2beta/projects',
              failOnStatusCode: false,
              body: project
            }).then((response) => {
              expect([200, 409]).to.include(response.status)
            })
          }

          let totalNodes = 0
          cy.request({
            headers: {
              'api-token': Cypress.env('adminTokenValue'),
              projects: '(unassigned)'
            },
            method: 'GET',
            url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
          }).then((response) => {
            totalNodes = response.body.length
          })

          cy.fixture('converge/avengers1.json').then(node1 => {
            cy.fixture('converge/avengers2.json').then(node2 => {
              cy.fixture('converge/xmen1.json').then(node3 => {
                cy.fixture('converge/xmen2.json').then(node4 => {
                  for (let node of [node1, node2, node3, node4]) {
                    cy.request({
                      headers: { 'api-token': Cypress.env('adminTokenValue') },
                      method: 'POST',
                      url: '/data-collector/v0',
                      body: node
                    })
                  }
                })
              })
            })
          })
          const maxRetries = 50
          waitForNodes(admin.id_token, totalNodes, 0, maxRetries)

          // confirm nodes are unassigned
          cy.request({
            headers: {
              'api-token': Cypress.env('adminTokenValue'),
              projects: '(unassigned)'
            },
            method: 'GET',
            url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
          }).then((response) => {
            expect(response.body).to.have.length(totalNodes + 4)
          })

          cy.request({
            headers: {
              'api-token': Cypress.env('adminTokenValue'),
              projects: avengersProject.id
            },
            method: 'GET',
            url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
          }).then((response) => {
            expect(response.body).to.have.length(0)
          })

          cy.request({
            headers: {
              'api-token': Cypress.env('adminTokenValue'),
              projects: xmenProject.id
            },
            method: 'GET',
            url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
          }).then((response) => {
            expect(response.body).to.have.length(0)
          })
        })
      })

      it('new rules get applied to nodes', () => {

        for (let rule of [avengersRule, xmenRule]) {
          cy.request({
            headers: { 'api-token': Cypress.env('adminTokenValue') },
            method: 'POST',
            url: '/apis/iam/v2beta/rules',
            body: rule
          })
        }

        // confirm rules are staged
        for (let project of [avengersProject, xmenProject]) {
          cy.request({
            headers: { 'api-token': Cypress.env('adminTokenValue') },
            method: 'GET',
            url: `/apis/iam/v2beta/projects/${project.id}/rules`
          }).then((response) => {
            expect(response.body.rules).to.have.length(1)
            for (let rule of response.body.rules) {
              expect(rule).to.have.property('status', 'STAGED')
            }
          })
        }

        cy.request({
          headers: { 'api-token': Cypress.env('adminTokenValue') },
          method: 'POST',
          url: '/apis/iam/v2beta/apply-rules'
        })
        // adding a wait here instead of a request poll since the apply status 
        // does not have precise enough timing for such a small data set
        cy.wait(5000)        

        // confirm rules are applied
        for (let project of [avengersProject, xmenProject]) {
          cy.request({
            headers: { 'api-token': Cypress.env('adminTokenValue') },
            method: 'GET',
            url: `/apis/iam/v2beta/projects/${project.id}/rules`
          }).then((response) => {
            expect(response.body.rules).to.have.length(1)
            for (let rule of response.body.rules) {
              expect(rule).to.have.property('status', 'APPLIED')
            }
          })
        }

        // confirm nodes are assigned to projects correctly
        cy.request({
          headers: {
            'api-token': Cypress.env('adminTokenValue'),
            projects: avengersProject.id
          },
          method: 'GET',
          url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
        }).then((response) => {
          expect(response.body).to.have.length(2)
        })

        cy.request({
          headers: {
            'api-token': Cypress.env('adminTokenValue'),
            projects: xmenProject.id
          },
          method: 'GET',
          url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
        }).then((response) => {
          expect(response.body).to.have.length(2)
        })
      })

      it('rules with updated conditions get applied to nodes', () => {

        // change avengers rule to include both organizations
        let updatedAvengersRule = avengersRule
        
        updatedAvengersRule.conditions = [
          {
            attribute: "CHEF_ORGS",
            operator: "MEMBER_OF",
            values: ["avengers", "xmen"]
          }
        ]

        cy.request({
          headers: { 'api-token': Cypress.env('adminTokenValue') },
          method: 'PUT',
          url: `/apis/iam/v2beta/rules/${avengersRule.id}`,
          body: updatedAvengersRule
        })

        cy.request({
          headers: { 'api-token': Cypress.env('adminTokenValue') },
          method: 'POST',
          url: '/apis/iam/v2beta/apply-rules'
        })
        cy.wait(5000) 

        cy.request({
          headers: {
            'api-token': Cypress.env('adminTokenValue'),
            projects: avengersProject.id
          },
          method: 'GET',
          url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
        }).then((response) => {
          expect(response.body).to.have.length(4)
        })
      })

      it('deleted rules get applied to nodes', () => {

        cy.request({
          headers: { 'api-token': Cypress.env('adminTokenValue') },
          method: 'DELETE',
          url: `/apis/iam/v2beta/rules/${avengersRule.id}`,
          body: avengersRule
        })

        cy.request({
          headers: { 'api-token': Cypress.env('adminTokenValue') },
          method: 'POST',
          url: '/apis/iam/v2beta/apply-rules'
        })
        cy.wait(5000) 

        cy.request({
          headers: {
            'api-token': Cypress.env('adminTokenValue'),
            projects: avengersProject.id
          },
          method: 'GET',
          url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
        }).then((response) => {
          expect(response.body).to.have.length(0)
        })

        cy.request({
          headers: {
            'api-token': Cypress.env('adminTokenValue'),
            projects: '(unassigned)'
          },
          method: 'GET',
          url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
        }).then((response) => {
          expect(response.body).to.have.length(2)
        })
      })
    })
  }
})

function waitForNodes(idToken: string, totalNodes: number, retries: number, maxRetries: number) {
  cy
    .request({
      auth: { bearer: idToken },
      method: 'GET',
      url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
    })
    .then((resp: Cypress.ObjectLike) => {
      // to avoid getting stuck in an infinite loop
      if (retries > maxRetries)
        return
      if (resp.body.length == totalNodes + 4)
        return

      retries = retries + 1
      waitForNodes(idToken, totalNodes, retries, maxRetries)
    })
}

function cleanupTestProjects(id_token: string): void {
  cy.request({
    auth: { bearer: id_token },
    method: 'GET',
    url: '/apis/iam/v2beta/projects',
    failOnStatusCode: false
  }).then((resp) => {
    let body = resp.body
    for (let project of body.projects) {
      if (project.id.startsWith('avengers') || project.id.startsWith('xmen')) {
        cy.request({
          auth: { bearer: id_token },
          method: 'DELETE',
          url: `/apis/iam/v2beta/projects/${project.id}`,
          failOnStatusCode: false
        })
      }
    }
  })
}
