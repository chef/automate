import { Project, Rule } from '../../../support/types';

describe('project management', () => {

  let adminIdToken = '';
  const now = Cypress.moment().format('MMDDYYhhmm');
  const cypressPrefix = 'proj-mgmt';
  const projectID = `${cypressPrefix}-project1-${now}`;
  const projectName = `${cypressPrefix} project1 ${now}`;

  const customWithoutPolsProjectID = `${cypressPrefix}-custom1-${now}`;
  const customWithPolsProjectID = `${cypressPrefix}-custom2-${now}`;
  const associatedPolicies = ['project-editors', 'project-viewers', 'project-owners'];
  const associatedTeams = ['project-owners', 'editors', 'viewers'];

  before(() => {
    cy.adminLogin('/settings/projects').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      adminIdToken = admin.id_token;
      cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['projects']);

      // if there are projects that already exist, set the projects filter to all projects
      cy.request({
        auth: { bearer: adminIdToken },
        url: '/apis/iam/v2/projects'
      }).then((response) => {
        expect(response.status).to.equal(200);
        if (response.body.projects.length > 0) {
          cy.applyProjectsFilter([]);
        }
      });
    });
  });

  beforeEach(() => {
    cy.restoreStorage();
    cy.get('body').then($body => {
      if ($body.find('[data-cy=close-x]').length > 0) {
        // evaluates as true if button exists at all
        cy.get('[data-cy=close-x]').then($btn => {
          if ($btn.is(':visible')) {
            // you get here only if button EXISTS and is VISIBLE
            cy.get('[data-cy=close-x]').click();
          } else {
            // you get here only if button EXISTS but is INVISIBLE
          }
        });
      } else {
        // you get here if the button DOESN'T EXIST
        assert.isOk('everything', 'everything is OK');
      }
    });
  });

  afterEach(() => {
    cy.saveStorage();
  });

  after(() => {
    cy.cleanupIAMObjectsByIDPrefixes(cypressPrefix, ['projects']);
  });

  it('displays a list of projects', () => {
    cy.request({
      auth: { bearer: adminIdToken },
      method: 'GET',
      url: '/apis/iam/v2/projects'
    }).then((response) => {
      expect(response.status).to.equal(200);
      response.body.projects.map((project: Project) => project.id)
        .forEach((id: string) => {
          cy.get('chef-table chef-td').contains(id);
        });
    });
  });

  it('can create a project without adding a custom ID', () => {
    cy.wait(2000);
    // if (Cypress.$('app-welcome-modal').length) {  // zero length means not found
    //   // cy.get('[data-cy=close-x]').click();
    //   cy.get('[data-cy=close-x]').then($button => {
    //     if ($button.is(':visible')){
    //       cy.get('[data-cy=close-x]').click();
    //       //you get here only if button is visible
    //     }
    //   })
    // }

    cy.get('[data-cy=create-project]').contains('Create Project').click();
    cy.get('app-project-list chef-modal').should('have.class', 'visible');

    cy.get('[data-cy=create-name]').focus()
      .type(projectName).should('have.value', projectName);

    cy.get('[data-cy=create-id]').should('not.be.visible');
    cy.get('[data-cy=edit-button]').contains('Edit ID').click();
    cy.get('[data-cy=id-label]').should('not.be.visible');
    cy.get('#id-input').should('have.value', projectID);

    cy.get('app-create-object-modal chef-checkbox')
      .should('have.attr', 'aria-checked', 'true');

    // don't create associated project policies
    cy.get('app-create-object-modal chef-checkbox').click();

    cy.get('[data-cy=save-button]').click();
    cy.get('app-project-list chef-modal').should('not.be.visible');

    // verify success notification and then dismiss it
    cy.get('app-notification.info').contains(`Created project ${projectID}`);
    cy.get('app-notification.info chef-icon').click();

    cy.contains(projectName).should('exist');
    cy.contains(projectID).should('exist');

    cy.url().should('include', '/settings/projects');
  });

  it('can create a project with a custom ID without generating associated policies', () => {
    cy.get('[data-cy=create-project]').contains('Create Project').click();
    cy.get('app-project-list chef-modal').should('have.class', 'visible');

    cy.get('[data-cy=create-name]').focus()
      .type(projectName).should('have.value', projectName);

    cy.get('[data-cy=create-id]').should('not.be.visible');
    cy.get('[data-cy=edit-button]').contains('Edit ID').click();
    cy.get('[data-cy=id-label]').should('not.be.visible');

    // don't create associated project policies
    cy.get('app-create-object-modal chef-checkbox').click();

    cy.get('[data-cy=create-id]').should('be.visible').clear()
      .type(customWithoutPolsProjectID).should('have.value', customWithoutPolsProjectID);

    cy.get('[data-cy=save-button]').click();
    cy.get('app-project-list chef-modal').should('not.be.visible');

    // verify success notification and then dismiss it
    cy.get('app-notification.info').contains(`Created project ${customWithoutPolsProjectID}`);
    cy.get('app-notification.info chef-icon').click();

    cy.contains(customWithoutPolsProjectID).should('exist');

    // verify no associated policies were generated
    associatedPolicies.forEach((policy) => {
      cy.request({
        auth: { bearer: adminIdToken },
        url: `/apis/iam/v2/policies/${customWithoutPolsProjectID}-${policy}`,
        failOnStatusCode: false
      }).then((response) => {
        expect(response.status).to.equal(404);
      });
    });

    cy.url().should('include', '/settings/projects');
  });

  it('can create a project with a custom ID and generate associated policies', () => {
    cy.get('[data-cy=create-project]').contains('Create Project').click();
    cy.get('app-project-list chef-modal').should('have.class', 'visible');

    cy.get('[data-cy=create-name]').focus()
      .type(projectName).should('have.value', projectName);

    cy.get('[data-cy=create-id]').should('not.be.visible');
    cy.get('[data-cy=edit-button]').contains('Edit ID').click();
    cy.get('[data-cy=id-label]').should('not.be.visible');

    // addPolicies checkbox is always checked on modal open
    cy.get('app-create-object-modal chef-checkbox')
      .should('have.attr', 'aria-checked', 'true');

    cy.get('[data-cy=create-id]').should('be.visible').clear()
      .type(customWithPolsProjectID).should('have.value', customWithPolsProjectID);

    cy.get('[data-cy=save-button]').click();
    cy.get('app-project-list chef-modal').should('not.be.visible');

    // verify success notification
    cy.get('app-notification.info')
      .contains(`Created project ${customWithPolsProjectID} and associated policies`);

    // dismiss all three success notifications
    cy.get('app-notification.info chef-icon').click({ multiple: true });

    cy.contains(customWithPolsProjectID).should('exist');

    // verify associated policies were generated
    associatedPolicies.forEach((policy) => {
      cy.request({
        auth: { bearer: adminIdToken },
        url: `/apis/iam/v2/policies/${customWithPolsProjectID}-${policy}`
      }).then((response) => {
        expect(response.status).to.equal(200);
      });
    });

    cy.url().should('include', '/settings/projects');
  });

  it('can open the new project\'s details page', () => {
    cy.get('[data-cy=project-details]').contains(projectName).click();
    cy.url().should('include', `/settings/projects/${projectID}`);
  });

  it('can update a project name', () => {
    const updatedProjectName = `updated ${projectName}`;
    cy.route('GET', `/apis/iam/v2/projects/${projectID}`).as('getProject');

    cy.get('[data-cy=details-tab]').click();
    cy.wait('@getProject');

    cy.get('[data-cy=update-project-name]').focus().clear()
      .type(updatedProjectName)
      .should('have.value', updatedProjectName);
    cy.get('app-project-details chef-button').contains('Save').click();

    cy.get('app-project-details h1.page-title').contains(updatedProjectName);
  });

  it('cannot delete a project with rules', () => {
    const rule: Rule = {
      id: `${cypressPrefix}-rule`,
      name: 'test rule',
      project_id: customWithoutPolsProjectID,
      type: 'NODE',
      conditions: [
        {
          attribute: 'CHEF_ORGANIZATION',
          operator: 'EQUALS',
          values: ['foo']
        }
      ]
    };
    // add rule via the API to save time,
    // since we test rules more thoroughly in project_rule_mgmt.spec.ts
    cy.request({
      auth: { bearer: adminIdToken },
      method: 'POST',
      url: `/apis/iam/v2/projects/${customWithoutPolsProjectID}/rules`,
      body: rule
    }).then((response) => {
      expect(response.status).to.equal(200);
    });

    // navigate back to the project list page and get latest project data
    cy.route('GET', '/apis/iam/v2/projects').as('getProjects');
    cy.get('.breadcrumb').click();
    cy.wait('@getProjects');

    cy.get('app-project-list chef-td').contains(customWithoutPolsProjectID).parent()
      .find('.mat-select-trigger').as('controlMenu');

    // we throw in a `should` so cypress retries until introspection allows menu to be shown
    cy.get('@controlMenu').scrollIntoView().should('be.visible')
      .click();
    cy.get('[data-cy=delete-project]').should('be.visible')
      .click();

    cy.get('app-project-list app-message-modal')
      .contains('Could Not Delete Project').should('be.visible');

    //  close modal
    cy.get('app-project-list app-message-modal chef-button:first').click();

    // delete rule
    cy.request({
      auth: { bearer: adminIdToken },
      method: 'DELETE',
      url: `/apis/iam/v2/projects/${customWithoutPolsProjectID}/rules/${rule.id}`
    });
  });

  it('can delete a project without rules', () => {
    cy.get('app-project-list chef-td').contains(customWithPolsProjectID).parent()
      .find('.mat-select-trigger').as('controlMenu');

    // we throw in a `should` so cypress retries until introspection allows menu to be shown
    cy.get('@controlMenu').scrollIntoView().should('be.visible')
      .click();
    cy.get('[data-cy=delete-project]').should('be.visible')
      .click();

    // accept dialog
    cy.get('app-project-list chef-button').contains('Delete Project').click();

    // verify success notification and then dismiss it
    cy.get('app-notification.info').contains(`Deleted project ${customWithPolsProjectID}`);
    cy.get('app-notification.info chef-icon').click();

    // Once we get this notification we know the network call to delete succeeded,
    // so now we can check if there are other projects or not.
    cy.request({
      auth: { bearer: adminIdToken },
      method: 'GET',
      url: '/apis/iam/v2/projects'
    }).then((response) => {
      expect(response.status).to.equal(200);
      // no projects are left so we shouldn't render the table at all
      if (response.body.projects.length === 0) {
        cy.get('app-project-list chef-tbody').should('not.exist');
        // otherwise, check that the projectID is no longer in the table
      } else {
        cy.get('app-project-list chef-tbody chef-td')
          .contains(customWithPolsProjectID).should('not.exist');
      }
    });
  });
});
