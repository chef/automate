describe('projects API', () => {
  describe('applying project rules', () => {

    const admin_token = 'ZNhnqJyDqgkXVk_bpobS3YhIMz0='

    const avengers_project = {
      id: `avengers-project-${Cypress.moment().format('MMDDYYhhmm')}`,
      name: "Test Avengers Project"
    }

    const xmen_project = {
      id: `xmen-project-${Cypress.moment().format('MMDDYYhhmm')}`,
      name: "Test X-men Project"
    }

    const avengers_rule = {
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

    before(() => {
      cy.adminLogin('/').then(() => {
        let admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'))

        // generate admin API token
        let token_id = `test-token-${Cypress.moment().format('MMDDYYhhmm')}`
        cy.request({
          auth: { bearer: admin.id_token },
          method: 'POST',
          url: '/apis/iam/v2beta/tokens',
          failOnStatusCode: false,
          body: {
            id: token_id,
            name: "cypress-api-test-admin-token",
            value: admin_token
          }
        }).then((response) => {
          expect([200, 409]).to.include(response.status)
        })

        cy.request({
          auth: { bearer: admin.id_token },
          method: 'POST',
          url: '/apis/iam/v2beta/policies/administrator-access/members:add',
          body: {
            members: [`token:${token_id}`]
          }
        })

        // TODO fix this type
        const nodes = <any>[]

        Cypress.Promise.all([
          cy.fixture('converge/avengers1.json').then(node => nodes.push(node)),
          cy.fixture('converge/avengers2.json').then(node => nodes.push(node)),
          cy.fixture('converge/xmen1.json').then(node => nodes.push(node)),
          cy.fixture('converge/xmen2.json').then(node => nodes.push(node))
        ]).then((node) => {
          const [avengers1, avengers2, xmen1, xmen2] = nodes
        })

        for (let node of nodes) {
          cy.request({
            headers: { 'api-token': admin_token },
            method: 'POST',
            url: `/data-collector/v0`,
            body: node
          })
        }

        // confirm nodes are unassigned
        cy.request({
          headers: { 
            'api-token': admin_token,
            'projects': avengers_project.id  
          },
          method: 'GET',
          url: `/api/v0/cfgmgmt/nodes?pagination.size=10`
        }).then((response) => {
          expect(response.body).to.have.length(0)
        })

        cy.request({
          headers: {
            'api-token': admin_token,
            'projects': xmen_project.id
          },
          method: 'GET',
          url: `/api/v0/cfgmgmt/nodes?pagination.size=10`
        }).then((response) => {
          expect(response.body).to.have.length(0)
        })

        // create projects or confirm them already exist
        for (let project of [avengers_project, xmen_project]) {
          cy.request({
            headers: { 'api-token': admin_token },
            method: 'POST',
            url: `/apis/iam/v2beta/projects`,
            failOnStatusCode: false,
            body: project
          }).then((response) => {
            expect([200, 409]).to.include(response.status)
          })
        }
      })
    })

    it('new rules get applied to nodes', () => {

      for (let rule of [avengers_rule, xmen_rule]) {
        cy.request({
          headers: { 'api-token': admin_token },
          method: 'POST',
          url: `/apis/iam/v2beta/rules`,
          body: rule
        })
      }
        
      // apply rules
      // confirm nodes are assigned to projects correctly

    })

    it('rules with updated conditions get applied to nodes', () => {

      // update first rule to add another condition to add more nodes to the project
      // apply rules
      // confirm new nodes added to project

      // update the same rule to remove the first condition to remove nodes from the project
      // apply rules
      // confirm nodes removed from the project
    })

    it('deleted rules get applied to nodes', () => {

      // delete other rule
      // apply rules
      // confirm its nodes go back to unassigned
    })
  })
})
