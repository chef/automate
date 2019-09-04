import { describeIfIAMV2p1 } from '../../constants';

describe('teams API', () => {
  let adminToken = '';
  const defaultAdminReq = {
    auth: { bearer: adminToken },
    method: 'GET',
    url: '/apis/iam/v2beta/teams'
  };
  const cypressPrefix = 'test-teams-api';
  const now = Cypress.moment().format('MMDDYYhhmm');
  const project1 = {
      id: `${cypressPrefix}-project1-${now}`,
      name: 'Test Project 1'
  };
  const project2 = {
      id: `${cypressPrefix}-project2-${now}`,
      name: 'Test Project 2'
  };

  before(() => {
    cy.adminLogin('/').then(() => {
      const admin = JSON.parse(<string>localStorage.getItem('chef-automate-user'));
      adminToken = admin.id_token;
      defaultAdminReq.auth.bearer = adminToken;
      cy.cleanupV2IAMObjectsByIDPrefixes(adminToken, cypressPrefix,
        ['policies', 'projects', 'teams']);

      for (const project of [project1, project2]) {
        cy.request({
            auth: { bearer: adminToken },
            method: 'POST',
            url: '/apis/iam/v2beta/projects',
            body: project
        });
      }
    });
  });

  after(() => {
    cy.cleanupV2IAMObjectsByIDPrefixes(adminToken, cypressPrefix,
      ['policies', 'projects', 'teams']);
  });

  describeIfIAMV2p1('project assignment enforcement', () => {
    let nonAdminToken = '';
    const nonAdminTokenID = `${cypressPrefix}-nonadmin-token-${now}`;
    const teamID = `${cypressPrefix}-team-${now}`;
    const defaultNonAdminReq = {
      headers: { 'api-token': nonAdminToken },
      method: 'POST',
      url: '/apis/iam/v2beta/teams'
    };

    before(() => {
      cy.request({ ...defaultAdminReq,
        method: 'POST',
        url: '/apis/iam/v2beta/tokens',
        body: {
            id: nonAdminTokenID,
            name: 'Nonadmin Token'
        }
      }).then((response) => {
          nonAdminToken = response.body.token.value;
          defaultNonAdminReq.headers = { 'api-token': nonAdminToken };
      });

      cy.request({ ...defaultAdminReq,
        url: '/apis/iam/v2beta/policies',
        method: 'POST',
        body: {
            name: 'token-access',
            id: `${cypressPrefix}-token-access-policy-${now}`,
            members: [ `token:${nonAdminTokenID}` ],
            statements: [
              {
                effect: 'ALLOW',
                actions: [ 'iam:teams:*' ],
                projects: [ '*' ]
              },
              {
                effect: 'ALLOW',
                actions: [ 'iam:projects:assign' ],
                projects: [ project1.id ]
              }
            ],
            projects: []
        }
      });
    });

    beforeEach(() => {
      cy.cleanupV2IAMObjectsByIDPrefixes(adminToken, cypressPrefix, ['teams']);
    });

    afterEach(() => {
      cy.cleanupV2IAMObjectsByIDPrefixes(adminToken, cypressPrefix, ['teams']);
    });

    describe('POST /apis/iam/v2beta/teams', () => {
      it('admin can create a new team with no projects', () => {
        cy.request({ ...defaultAdminReq,
            method: 'POST',
            body: teamWithProjects(teamID, [])
        }).then((response) => {
            expect(response.body.team.projects).to.have.length(0);
        });
      });

      it('admin can create a new team with multiple projects', () => {
        cy.request({ ...defaultAdminReq,
            method: 'POST',
            body: teamWithProjects(teamID, [project1.id, project2.id])
        }).then((response) => {
            expect(response.body.team.projects).to.have.length(2);
        });
      });

      it('admin gets a 404 when it attempts to create ' +
         'a team with a project that does not exist', () => {
        cy.request({ ...defaultAdminReq,
            method: 'POST',
            failOnStatusCode: false,
            body: teamWithProjects(teamID, [project1.id, project2.id, 'notfound'])
        }).then((response) => {
            expect(response.status).to.equal(404);
        });
      });

      it('non-admin with project1 assignment access can create a new team ' +
      'with no projects', () => {
        cy.request({ ...defaultNonAdminReq,
            method: 'POST',
            body: teamWithProjects(teamID, [])
        }).then((response) => {
            expect(response.body.team.projects).to.have.length(0);
        });
      });

      it('non-admin with project1 assignment access can create ' +
      'a new team with project1', () => {
        cy.request({ ...defaultNonAdminReq,
            method: 'POST',
            body: teamWithProjects(teamID, [project1.id])
        }).then((response) => {
            expect(response.body.team.projects).to.have.length(1);
        });
      });

      it('non-admin with project1 assignment access gets a 404 when it attempts to create ' +
      'a team with a project that does not exist', () => {
        cy.request({ ...defaultNonAdminReq,
            method: 'POST',
            failOnStatusCode: false,
            body: teamWithProjects(teamID,
              [project1.id, project2.id, 'notfound'])
        }).then((response) => {
            expect(response.status).to.equal(404);
        });
      });

      it('non-admin with project1 assignment access cannot create ' +
      'a new team with other projects (403)', () => {
        cy.request({ ...defaultNonAdminReq,
            method: 'POST',
            failOnStatusCode: false,
            body: teamWithProjects(teamID, [project1.id, project2.id])
        }).then((response) => {
            expect(response.status).to.equal(403);
        });
      });
    });

    describe('PUT /apis/iam/v2beta/teams', () => {
      it('admin can update a team with no projects to have projects', () => {
        cy.request({ ...defaultAdminReq,
            method: 'POST',
            body: teamWithProjects(teamID, [])
        }).then((response) => {
            expect(response.body.team.projects).to.have.length(0);
        });

        cy.request({ ...defaultAdminReq,
            method: 'PUT',
            url: `/apis/iam/v2beta/teams/${teamID}`,
            body: teamWithProjects(teamID, [project1.id, project2.id])
        }).then((response) => {
            expect(response.body.team.projects).to.have.length(2);
        });
      });

      it('admin can update a team with projects to have no projects', () => {
        cy.request({ ...defaultAdminReq,
            method: 'POST',
            body: teamWithProjects(teamID, [project1.id, project2.id])
        }).then((response) => {
            expect(response.body.team.projects).to.have.length(2);
        });

        cy.request({ ...defaultAdminReq,
            method: 'PUT',
            url: `/apis/iam/v2beta/teams/${teamID}`,
            body: teamWithProjects(teamID, [])
        }).then((response) => {
            expect(response.body.team.projects).to.have.length(0);
        });
      });

      it('admin cannot update a team to have projects that do not exist', () => {
        cy.request({ ...defaultAdminReq,
            method: 'POST',
            body: teamWithProjects(teamID, [])
        }).then((response) => {
            expect(response.body.team.projects).to.have.length(0);
        });

        cy.request({ ...defaultAdminReq,
            method: 'PUT',
            url: `/apis/iam/v2beta/teams/${teamID}`,
            failOnStatusCode: false,
            body: teamWithProjects(teamID, [project1.id, 'notfound'])
        }).then((response) => {
            expect(response.status).to.equal(404);
        });
      });

      it('non-admin with project1 assignment access can update ' +
      'a team with no projects to have project1', () => {
        cy.request({ ...defaultAdminReq,
            method: 'POST',
            body: teamWithProjects(teamID, [])
        }).then((response) => {
            expect(response.body.team.projects).to.have.length(0);
        });

        cy.request({ ...defaultNonAdminReq,
            method: 'PUT',
            url: `/apis/iam/v2beta/teams/${teamID}`,
            body: teamWithProjects(teamID, [project1.id])
        }).then((response) => {
            expect(response.body.team.projects).to.have.length(1);
        });
      });

      it('non-admin with project1 assignment access can update ' +
      'a team to remove project1', () => {
        cy.request({ ...defaultAdminReq,
            method: 'POST',
            body: teamWithProjects(teamID, [project1.id])
        }).then((response) => {
            expect(response.body.team.projects).to.have.length(1);
        });

        cy.request({ ...defaultNonAdminReq,
            method: 'PUT',
            url: `/apis/iam/v2beta/teams/${teamID}`,
            body: teamWithProjects(teamID, [])
        }).then((response) => {
            expect(response.body.team.projects).to.have.length(0);
        });
      });

      it('non-admin with project1 assignment access gets a 404 ' +
      'when updating a team to have non-existent teams', () => {
        cy.request({ ...defaultAdminReq,
            method: 'POST',
            body: teamWithProjects(teamID, [])
        }).then((response) => {
            expect(response.body.team.projects).to.have.length(0);
        });

        cy.request({ ...defaultNonAdminReq,
            method: 'PUT',
            url: `/apis/iam/v2beta/teams/${teamID}`,
            failOnStatusCode: false,
            body: teamWithProjects(teamID, [project1.id, 'notfound'])
        }).then((response) => {
            expect(response.status).to.equal(404);
        });
      });

      it('non-admin with project1 assignment access cannot update ' +
      'a team with no projects to have other projects', () => {
        cy.request({ ...defaultAdminReq,
            method: 'POST',
            body: teamWithProjects(teamID, [])
        }).then((response) => {
            expect(response.body.team.projects).to.have.length(0);
        });

        cy.request({ ...defaultNonAdminReq,
            method: 'PUT',
            url: `/apis/iam/v2beta/teams/${teamID}`,
            failOnStatusCode: false,
            body: teamWithProjects(teamID, [project1.id, project2.id])
        }).then((response) => {
            expect(response.status).to.equal(403);
        });
      });

      it('non-admin with project1 assignment access cannot update ' +
      'a team to remove other projects', () => {
        cy.request({ ...defaultAdminReq,
            method: 'POST',
            body: teamWithProjects(teamID, [project1.id, project2.id])
        }).then((response) => {
            expect(response.body.team.projects).to.have.length(2);
        });

        cy.request({ ...defaultNonAdminReq,
            method: 'PUT',
            url: `/apis/iam/v2beta/teams/${teamID}`,
            failOnStatusCode: false,
            body: teamWithProjects(teamID, [project1.id])
        }).then((response) => {
            expect(response.status).to.equal(403);
        });
      });
    });
  });
});

function teamWithProjects(id: string, projects: string[]): any {
  return {
    id: id,
    name: 'fun name',
    projects: projects
  };
}
