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
    const scenarios = [
      {req: defaultNonAdminReq, id: 'project-1-admin'},
      {req: defaultAdminReq, id: 'superadmin'}
    ];

    before(() => {
      for (const project of [project1, project2]) {
        cy.request({
            auth: { bearer: adminToken },
            method: 'POST',
            url: '/apis/iam/v2beta/projects',
            body: project
        });
      }

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
      context('both superadmin and project1 admin', () => {
        scenarios.forEach((scenario) => {
          it(`${scenario.id} can create a new team with no projects`, () => {
            cy.request({ ...scenario.req,
                method: 'POST',
                body: teamWithProjects(teamID, [])
            }).then((response) => {
                expect(response.body.team.projects).to.have.length(0);
            });
          });

          it(`${scenario.id} cannot create a team with a project that does not exist (404)`, () => {
            cy.request({ ...scenario.req,
                method: 'POST',
                failOnStatusCode: false,
                body: teamWithProjects(teamID, [project1.id, project2.id, 'notfound'])
            }).then((response) => {
                expect(response.status).to.equal(404);
            });
          });

          it(`${scenario.id} can create a team with project1`, () => {
            cy.request({ ...scenario.req,
                method: 'POST',
                body: teamWithProjects(teamID, [project1.id])
            }).then((response) => {
                expect(response.body.team.projects).to.have.length(1);
            });
          });
        });
      });

      context('superadmin', () => {
        it('can create a new team with projects other than project1', () => {
          cy.request({ ...defaultAdminReq,
              method: 'POST',
              body: teamWithProjects(teamID, [project1.id, project2.id])
          }).then((response) => {
              expect(response.body.team.projects).to.have.length(2);
          });
        });
      });

      context('project1 admin', () => {
        it('cannot create a team with projects other than project1 (403)', () => {
          cy.request({ ...defaultNonAdminReq,
              method: 'POST',
              failOnStatusCode: false,
              body: teamWithProjects(teamID, [project1.id, project2.id])
          }).then((response) => {
              expect(response.status).to.equal(403);
          });
        });
      });
    });

    describe('PUT /apis/iam/v2beta/teams', () => {
      context('both superadmin and project1 admin', () => {
        scenarios.forEach((scenario) => {
          it(`${scenario.id} can update a team with no projects to have project1`, () => {
            cy.request({ ...defaultAdminReq,
                method: 'POST',
                body: teamWithProjects(teamID, [])
            }).then((response) => {
                expect(response.body.team.projects).to.have.length(0);
            });

            cy.request({ ...scenario.req,
                method: 'PUT',
                url: `/apis/iam/v2beta/teams/${teamID}`,
                body: teamWithProjects(teamID, [project1.id])
            }).then((response) => {
                expect(response.body.team.projects).to.have.length(1);
            });
          });

          it(`${scenario.id} can update a team to remove project1`, () => {
            cy.request({ ...defaultAdminReq,
                method: 'POST',
                body: teamWithProjects(teamID, [project1.id, project2.id])
            }).then((response) => {
                expect(response.body.team.projects).to.have.length(2);
            });

            cy.request({ ...scenario.req,
                method: 'PUT',
                url: `/apis/iam/v2beta/teams/${teamID}`,
                body: teamWithProjects(teamID, [project2.id])
            }).then((response) => {
                expect(response.body.team.projects).to.have.length(1);
            });
          });

          it(`${scenario.id} can update a team to remove project1`, () => {
            cy.request({ ...defaultAdminReq,
                method: 'POST',
                body: teamWithProjects(teamID, [project1.id])
            }).then((response) => {
                expect(response.body.team.projects).to.have.length(1);
            });

            cy.request({ ...scenario.req,
                method: 'PUT',
                url: `/apis/iam/v2beta/teams/${teamID}`,
                body: teamWithProjects(teamID, [])
            }).then((response) => {
                expect(response.body.team.projects).to.have.length(0);
            });
          });

          it(`${scenario.id} cannot update a team to have projects that do not exist (404)`, () => {
            cy.request({ ...defaultAdminReq,
                method: 'POST',
                body: teamWithProjects(teamID, [])
            }).then((response) => {
                expect(response.body.team.projects).to.have.length(0);
            });

            cy.request({ ...scenario.req,
                method: 'PUT',
                url: `/apis/iam/v2beta/teams/${teamID}`,
                failOnStatusCode: false,
                body: teamWithProjects(teamID, [project1.id, 'notfound'])
            }).then((response) => {
                expect(response.status).to.equal(404);
            });
          });
        });
      });

      context('superadmin', () => {
        it('can update a team with no projects to have projects other than project1', () => {
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

        it('can update a team with projects other than project1 to have no projects', () => {
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
      });

      context('project1 admin', () => {
        it('cannot update a team to add projects other than project1 (403)', () => {
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

        it('cannot update a team to remove projects other than project1 (403)', () => {
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
});

function teamWithProjects(id: string, projects: string[]): any {
  return {
    id,
    projects,
    name: 'fun name'
  };
}
