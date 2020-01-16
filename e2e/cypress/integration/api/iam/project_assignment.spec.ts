import { describeIfIAMV2p1 } from '../../../support/constants';

describeIfIAMV2p1('project assignment (on object create, assign, unassign)', () => {
    const cypressPrefix = 'test-policies-api';
    const now = Cypress.moment().format('MMDDYYhhmm');
    const authorizedProject = {
        id: `${cypressPrefix}-authorized-${now}`,
        name: 'Authorized Project'
    };
    const unauthorizedProject = {
        id: `${cypressPrefix}-unauthorized-${now}`,
        name: 'Unauthorized Project'
    };


    const testObjectID = `${cypressPrefix}-test-object-${now}`;
    const authorizedOnlyProjReq = {
        headers: {}, // must fill in before use
        url: `/apis/iam/v2beta/teams/${testObjectID}`,
        method: 'PUT'
    };
    const authorizedAndUnassignedProjReq = {
        headers: {}, // must fill in before use
        url: `/apis/iam/v2beta/teams/${testObjectID}`,
        method: 'PUT'
    };

    const defaultAdminReq = {
        headers: {}, // must fill in before use
        url: `/apis/iam/v2beta/teams/${testObjectID}`,
        method: 'PUT'
    };

    const authorizedProjTokenID = `${cypressPrefix}-messi-${now}`;
    const authorizedAndUnassignedProjTokenID = `${cypressPrefix}-montag-${now}`;
    const unassigned = '(unassigned)';

    before(() => {
        defaultAdminReq.headers = { 'api-token': Cypress.env('ADMIN_TOKEN') };
        cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix,
            ['policies', 'projects', 'tokens', 'teams', 'projects']);

        cy.request({
            ...defaultAdminReq,
            method: 'POST',
            url: '/apis/iam/v2/tokens',
            body: {
              id: authorizedProjTokenID,
              name: authorizedProjTokenID
            }
        }).then((resp) => {
            expect(resp.status).to.equal(200);
            authorizedOnlyProjReq.headers = { 'api-token': resp.body.token.value };
        });

        cy.request({
            ...defaultAdminReq,
            method: 'POST',
            url: '/apis/iam/v2/tokens',
            body: {
              id: authorizedAndUnassignedProjTokenID,
              name: authorizedAndUnassignedProjTokenID
            }
        }).then((resp) => {
            expect(resp.status).to.equal(200);
            authorizedAndUnassignedProjReq.headers = { 'api-token': resp.body.token.value };
        });

        cy.request({
            ...defaultAdminReq,
            method: 'POST',
            url: '/apis/iam/v2/teams',
            body: {
              id: testObjectID,
              name: testObjectID,
              projects: []
            }
          }).then((resp) => {
            expect(resp.status).to.equal(200);
          });

        for (const project of [authorizedProject, unauthorizedProject]) {
            cy.request({
                headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
                method: 'POST',
                url: '/apis/iam/v2/projects',
                body: project
            });
        }

        cy.request({
            ...defaultAdminReq,
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: {
              id: `${cypressPrefix}-authorized-policy-${now}`,
              name: `${cypressPrefix} policy ${now}`,
              members: [`token:${authorizedProjTokenID}`],
              statements: [
                {
                    effect: 'ALLOW',
                    actions: ['iam:projects:assign'],
                    projects: [authorizedProject.id]
                },
                {
                    effect: 'ALLOW',
                    actions: ['iam:teams:*'],
                    projects: ['*']
                }
              ],
              projects: []
            }
        });

        cy.request({
            ...defaultAdminReq,
            method: 'POST',
            url: '/apis/iam/v2/policies',
            body: {
              id: `${cypressPrefix}-authorized-and-unassigned-policy-${now}`,
              name: `${cypressPrefix} policy ${now}`,
              members: [`token:${authorizedAndUnassignedProjTokenID}`],
              statements: [
                {
                    effect: 'ALLOW',
                    actions: ['iam:projects:assign'],
                    projects: [unassigned, authorizedProject.id]
                },
                {
                    effect: 'ALLOW',
                    actions: ['iam:teams:*'],
                    projects: ['*']
                }
              ],
              projects: []
            }
        });
    });

    after(() => {
        cy.cleanupV2IAMObjectsByIDPrefixes(cypressPrefix,
            ['policies', 'projects', 'tokens', 'teams', 'projects']);
    });

    afterEach(() => {
        // reset to unassigned after every test
        cy.request({
            ...defaultAdminReq,
            body: {
              name: testObjectID,
              projects: []
            }
        }).then((resp) => {
            expect(resp.status).to.equal(200);
            expect(resp.body.team.projects).to.have.length(0);
        });
    });

    it('assigning (unassigned) to unauthorized is not allowed ' +
        'for authorizedAndUnassignedProjToken',  () => {
        cy.request({
            ...authorizedAndUnassignedProjReq,
            body: {
                name: testObjectID,
                projects: [unauthorizedProject.id]
            },
            failOnStatusCode: false
        }).then((resp) => {
            expect(resp.status).to.equal(403);
        });
    });

    it('assigning authorized to unauthorized is not allowed ' +
        'for authorizedAndUnassignedProjToken',  () => {
        cy.request({
            ...defaultAdminReq,
            body: {
                name: testObjectID,
                projects: [authorizedProject.id]
            }
        }).then((resp) => {
            expect(resp.status).to.equal(200);
            expect(resp.body.team.projects).to.have.length(1);
            expect(resp.body.team.projects[0]).to.equal(authorizedProject.id);
        });

        cy.request({
            ...authorizedAndUnassignedProjReq,
            body: {
                name: testObjectID,
                projects: [unauthorizedProject.id]
            },
            failOnStatusCode: false
        }).then((resp) => {
            expect(resp.status).to.equal(403);
        });
    });

    it('assigning (unassigned) to authorized is not allowed ' +
        'for authorizedOnlyProjReq',  () => {
        cy.request({
            ...authorizedOnlyProjReq,
            body: {
                name: testObjectID,
                projects: [authorizedProject.id]
            },
            failOnStatusCode: false
        }).then((resp) => {
            expect(resp.status).to.equal(403);
        });
    });

    // (TC) We do allow users to remove their project, even if results in unassigned and
    // they don't have iam:project:assign perms on unassigned (special case).
    it('assigning authorized to (unassigned) is allowed ' +
        'for authorizedOnlyProjReq (special case)',  () => {
        cy.request({
            ...defaultAdminReq,
            body: {
                name: testObjectID,
                projects: [authorizedProject.id]
            }
        }).then((resp) => {
            expect(resp.status).to.equal(200);
            expect(resp.body.team.projects).to.have.length(1);
            expect(resp.body.team.projects[0]).to.equal(authorizedProject.id);
        });

        cy.request({
            ...authorizedOnlyProjReq,
            body: {
                name: testObjectID,
                projects: []
            }
        }).then((resp) => {
            expect(resp.status).to.equal(200);
            expect(resp.body.team.projects).to.have.length(0);
        });
    });

    it('not changing projects on update is allowed for authorizedOnlyProjReq ',  () => {
        cy.request({
            ...defaultAdminReq,
            body: {
                name: testObjectID,
                projects: [unauthorizedProject.id]
            }
        }).then((resp) => {
            expect(resp.status).to.equal(200);
            expect(resp.body.team.projects).to.have.length(1);
            expect(resp.body.team.projects[0]).to.equal(unauthorizedProject.id);
        });

        cy.request({
            ...authorizedOnlyProjReq,
            body: {
                name: testObjectID,
                projects: [unauthorizedProject.id]
            },
            failOnStatusCode: false
        }).then((resp) => {
            expect(resp.status).to.equal(200);
            expect(resp.body.team.projects).to.have.length(1);
            expect(resp.body.team.projects[0]).to.equal(unauthorizedProject.id);
        });
    });

    it('passed one non-existent project returns not found ' +
        'for authorizedOnlyProjReq',  () => {
        cy.request({
            ...authorizedOnlyProjReq,
            body: {
                name: testObjectID,
                projects: ['not-found-id']
            },
            failOnStatusCode: false
        }).then((resp) => {
            expect(resp.status).to.equal(404);
        });
    });

    it('passed two non-existent projects and one unauthorized returns not found ' +
        'for authorizedOnlyProjReq',  () => {
        cy.request({
            ...authorizedOnlyProjReq,
            body: {
                name: testObjectID,
                projects: ['not-found-id', 'not-found-id2', unauthorizedProject.id]
            },
            failOnStatusCode: false
        }).then((resp) => {
            expect(resp.status).to.equal(404);
        });
    });

    it('passed one authorized and one non-existent project returns not found ' +
        'for authorizedOnlyProjReq',  () => {
        cy.request({
            ...authorizedOnlyProjReq,
            body: {
                name: testObjectID,
                projects: [authorizedProject.id, 'not-found-id']
            },
            failOnStatusCode: false
        }).then((resp) => {
            expect(resp.status).to.equal(404);
        });
    });

    it('passed one authorized and one unauthorized project is not allowed ' +
        'for authorizedOnlyProjReq',  () => {
        cy.request({
            ...authorizedOnlyProjReq,
            body: {
                name: testObjectID,
                projects: [authorizedProject.id, unauthorizedProject.id]
            },
            failOnStatusCode: false
        }).then((resp) => {
            expect(resp.status).to.equal(403);
        });
    });

    it('adding an unauthorized project to an authorized project is not allowed ' +
        'for authorizedOnlyProjReq',  () => {
        cy.request({
            ...defaultAdminReq,
            body: {
                name: testObjectID,
                projects: [authorizedProject.id]
            }
        }).then((resp) => {
            expect(resp.status).to.equal(200);
            expect(resp.body.team.projects).to.have.length(1);
            expect(resp.body.team.projects[0]).to.equal(authorizedProject.id);
        });

        cy.request({
            ...authorizedOnlyProjReq,
            body: {
                name: testObjectID,
                projects: [authorizedProject.id, unauthorizedProject.id]
            },
            failOnStatusCode: false
        }).then((resp) => {
            expect(resp.status).to.equal(403);
        });
    });
});
