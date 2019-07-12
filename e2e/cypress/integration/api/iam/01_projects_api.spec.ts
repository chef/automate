const admin_token = 'ZNhnqJyDqgkXVk_bpobS3YhIMz0='
const admin_token_obj = {
  id: `test-token-${Cypress.moment().format('MMDDYYhhmm')}`,
  name: "cypress-api-test-admin-token",
  value: admin_token
}

const avengers_project = {
  id: `avengers-project-${Cypress.moment().format('MMDDYYhhmm')}`,
  name: "Test Avengers Project"
}

const xmen_project = {
  id: `xmen-project-${Cypress.moment().format('MMDDYYhhmm')}`,
  name: "Test X-men Project"
}

let avengersRule = {
  id: "avengers-rule-1",
  name: "first rule of avengers project",
  type: "NODE",
  project_id: avengers_project.id,
  conditions: [
    {
      attribute: "CHEF_ORGS",
      operator: "EQUALS",
      values: ["avengers"]
    }
  ]
}

const xmen_rule = {
  id: "xmen-rule-1",
  name: "first rule of xmen project",
  type: "NODE",
  project_id: xmen_project.id,
  conditions: [
    {
      attribute: "CHEF_ORGS",
      operator: "EQUALS",
      values: ["xmen"]
    }
  ]
}

describe('projects API', () => {
  describe('applying project rules', () => {

    before(() => {
      // TODO skip if not on 2.1

      cy.adminLogin('/').then(() => {
        let admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'))

        // generate admin API token
        cy.request({
          auth: { bearer: admin.id_token },
          method: 'POST',
          url: '/apis/iam/v2beta/tokens',
          failOnStatusCode: false,
          body: admin_token_obj
        }).then((response) => {
          expect([200, 409]).to.include(response.status)
        })

        cy.request({
          auth: { bearer: admin.id_token },
          method: 'POST',
          url: '/apis/iam/v2beta/policies/administrator-access/members:add',
          body: {
            members: [`token:${admin_token_obj.id}`]
          }
        })

        // create projects or confirm they already exist
        for (let project of [avengers_project, xmen_project]) {
          cy.request({
            headers: { 'api-token': admin_token },
            method: 'POST',
            url: '/apis/iam/v2beta/projects',
            failOnStatusCode: false,
            body: project
          }).then((response) => {
            expect([200, 409]).to.include(response.status)
          })
        }

        cy.fixture('converge/avengers1.json').then(node1 => {
          cy.fixture('converge/avengers2.json').then(node2 => {
            cy.fixture('converge/xmen1.json').then(node3 => {
              cy.fixture('converge/xmen2.json').then(node4 => {
                for (let node of [node1, node2, node3, node4]) {
                  cy.request({
                    headers: { 'api-token': admin_token },
                    method: 'POST',
                    url: '/data-collector/v0',
                    body: node
                  })
                }
              })
            })
          })
        })

        cy.wait(5000)  // TODO replace with polling apply status request

        // confirm nodes are unassigned
        cy.request({
          headers: {
            'api-token': admin_token,
            'projects': '(unassigned)'
          },
          method: 'GET',
          url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
        }).then((response) => {
          expect(response.body).to.have.length(4)
        })

        cy.request({
          headers: {
            'api-token': admin_token,
            'projects': avengers_project.id
          },
          method: 'GET',
          url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
        }).then((response) => {
          expect(response.body).to.have.length(0)
        })

        cy.request({
          headers: {
            'api-token': admin_token,
            'projects': xmen_project.id
          },
          method: 'GET',
          url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
        }).then((response) => {
          expect(response.body).to.have.length(0)
        })
      })
    })

    after(() => {
      for (let project of [avengers_project, xmen_project]) {
        cy.request({
          headers: { 'api-token': admin_token },
          method: 'DELETE',
          url: `/apis/iam/v2beta/projects/${project.id}`
        })
      }

      cy.request({
          headers: { 'api-token': admin_token },
          method: 'POST',
          url: 'api/v0/ingest/events/chef/node-multiple-deletes',
          body: {
            node_ids: [
              "7188b88b-2236-4e27-a875-d3a10a70c497",
              "639844f4-2ce6-42ba-8c9d-853db69adff3", 
              "75c2376e-d07e-4d2b-ab43-d658c6250a62",
              "da60e383-67f6-4501-b726-1f28e03bf6ea"
            ]
          }
        })

      cy.request({
        headers: { 'api-token': admin_token },
        method: 'DELETE',
        url: `/apis/iam/v2beta/tokens/${admin_token_obj.id}`
      })
    })

    it('new rules get applied to nodes', () => {

      for (let rule of [avengersRule, xmen_rule]) {
        cy.request({
          headers: { 'api-token': admin_token },
          method: 'POST',
          url: '/apis/iam/v2beta/rules',
          body: rule
        })
      }

      // confirm rules are staged
      for (let project of [avengers_project, xmen_project]) {
        cy.request({
          headers: { 'api-token': admin_token },
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
        headers: { 'api-token': admin_token },
        method: 'POST',
        url: '/apis/iam/v2beta/apply-rules'
      })
      cy.wait(5000) // TODO replace with polling apply status request

      // confirm rules are applied
      for (let project of [avengers_project, xmen_project]) {
        cy.request({
          headers: { 'api-token': admin_token },
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
          'api-token': admin_token,
          'projects': avengers_project.id
        },
        method: 'GET',
        url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
      }).then((response) => {
        expect(response.body).to.have.length(2)
      })

      cy.request({
        headers: {
          'api-token': admin_token,
          'projects': xmen_project.id
        },
        method: 'GET',
        url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
      }).then((response) => {
        expect(response.body).to.have.length(2)
      })
    })

    it('rules with updated conditions get applied to nodes', () => {
      // Add condition to avengers rule
      avengersRule.conditions.push({
        attribute: "CHEF_ORGS",
        operator: "EQUALS",
        values: ["xmen"]
      })

      cy.request({
        headers: { 'api-token': admin_token },
        method: 'PUT',
        url: `/apis/iam/v2beta/rules/${avengersRule.id}`,
        body: avengersRule
      })

      cy.request({
        headers: { 'api-token': admin_token },
        method: 'POST',
        url: '/apis/iam/v2beta/apply-rules'
      })
      cy.wait(5000) // TODO replace with polling apply status request

      // TODO investigate failure by repro locally
      cy.request({
        headers: {
          'api-token': admin_token,
          'projects': avengers_project.id
        },
        method: 'GET',
        url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
      }).then((response) => {
        expect(response.body).to.have.length(4)
      })
    })

    it('deleted rules get applied to nodes', () => {
      cy.request({
        headers: { 'api-token': admin_token },
        method: 'DELETE',
        url: `/apis/iam/v2beta/rules/${avengersRule.id}`,
        body: avengersRule
      })
      
      cy.request({
        headers: { 'api-token': admin_token },
        method: 'POST',
        url: '/apis/iam/v2beta/apply-rules'
      })
      cy.wait(5000)  // TODO replace with polling apply status request
      
      cy.request({
        headers: {
          'api-token': admin_token,
          'projects': avengers_project.id
        },
        method: 'GET',
        url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
      }).then((response) => {
        expect(response.body).to.have.length(0)
      })

      cy.request({
        headers: {
          'api-token': admin_token,
          'projects': '(unassigned)'
        },
        method: 'GET',
        url: '/api/v0/cfgmgmt/nodes?pagination.size=10'
      }).then((response) => {
        expect(response.body).to.have.length(2)
      })
    })
  })
})
