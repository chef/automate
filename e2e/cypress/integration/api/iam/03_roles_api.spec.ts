if (Cypress.env('IAM_VERSION') === undefined) {
    // assume local env is on IAM v2.0 since that's the default setting
    Cypress.env('IAM_VERSION', 'v2.0');
}
describe('roles API', () => {
    const cypressPrefix = 'test-roles-api';
    let adminIdToken = '';
    let nonAdminToken = '';
    const nonAdminTokenID = `${cypressPrefix}-nonadmin-token`;
    const describeIfIAMV2p1 = Cypress.env('IAM_VERSION') === 'v2.1' ? describe : describe.skip;
    before(() => {
        cy.adminLogin('/').then(() => {
            adminIdToken = JSON.parse(
                <string>localStorage.getItem('chef-automate-user')).id_token;
        });
    });

    describeIfIAMV2p1('project assignment enforcement', () => {
        const project1 = {
            id: `${cypressPrefix}-project1-${Cypress.moment().format('MMDDYYhhmm')}`,
            name: 'Test Project 1'
        };

        const project2 = {
            id: `${cypressPrefix}-project2-${Cypress.moment().format('MMDDYYhhmm')}`,
            name: 'Test Project 2'
        };

        const roleID = `${cypressPrefix}-role1`;

        before(() => {
            cy.cleanupProjectsByIDPrefix(adminIdToken, cypressPrefix);
            cy.cleanupRolesByIDPrefix(adminIdToken, cypressPrefix);
            cy.cleanupPoliciesByIDPrefix(adminIdToken, cypressPrefix);
            cy.cleanupTokensByIDPrefix(adminIdToken, cypressPrefix);
            for (const project of [project1, project2]) {
                cy.request({
                    auth: { bearer: adminIdToken },
                    method: 'POST',
                    url: '/apis/iam/v2beta/projects',
                    body: project
                });
            }

            cy.request({
                auth: { bearer: adminIdToken },
                method: 'POST',
                url: '/apis/iam/v2beta/tokens',
                body: {
                    active: true,
                    id: nonAdminTokenID,
                    name: 'Nonadmin Token'
                }
            }).then((response) => {
                nonAdminToken = response.body.token.value;
                console.log('wat');
                console.log(nonAdminToken);
            });

            cy.request({
                auth: { bearer: adminIdToken },
                method: 'POST',
                url: '/apis/iam/v2beta/policies',
                body: {
                    name: 'token-access',
                    id: `${cypressPrefix}-token-access-policy`,
                    members: [ `token:${nonAdminTokenID}` ],
                    statements: [
                      {
                        effect: 'ALLOW',
                        actions: [ 'iam:roles:*' ],
                        projects: [ '*']
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

        after(() => {
            cy.cleanupProjectsByIDPrefix(adminIdToken, cypressPrefix);
            cy.cleanupRolesByIDPrefix(adminIdToken, cypressPrefix);
            cy.cleanupPoliciesByIDPrefix(adminIdToken, cypressPrefix);
            cy.cleanupTokensByIDPrefix(adminIdToken, cypressPrefix);
        });

        afterEach(() => {
            cy.cleanupRolesByIDPrefix(adminIdToken, cypressPrefix);
        });

        describe('POST /apis/iam/v2beta/roles', () => {
            it('admin can create a new role with no projects', () => {
                cy.request({
                    auth: { bearer: adminIdToken },
                    method: 'POST',
                    url: '/apis/iam/v2beta/roles',
                    body: {
                        name: 'role1',
                        id: roleID,
                        actions: ['iam:users:get'],
                        projects: []
                    }
                }).then((response) => {
                    expect(response.body.role.projects).to.have.length(0);
                });
            });

            it('admin can create a new role with multiple projects', () => {
                cy.request({
                    auth: { bearer: adminIdToken },
                    method: 'POST',
                    url: '/apis/iam/v2beta/roles',
                    body: {
                        name: 'role1',
                        id: roleID,
                        actions: ['iam:users:get'],
                        projects: [project1.id, project2.id]
                    }
                }).then((response) => {
                    expect(response.body.role.projects).to.have.length(2);
                });
            });

            it('admin gets a 404 when it attempts to create ' +
               'a role with a project that does not exist', () => {
                cy.request({
                    auth: { bearer: adminIdToken },
                    method: 'POST',
                    url: '/apis/iam/v2beta/roles',
                    failOnStatusCode: false,
                    body: {
                        name: 'role1',
                        id: roleID,
                        actions: ['iam:users:get'],
                        projects: [project1.id, project2.id, 'notfound']
                    }
                }).then((response) => {
                    expect(response.status).to.equal(404);
                });
            });

            it('non-admin with project1 assignment access can create a new role ' +
            'with no projects', () => {
                cy.request({
                    headers: { 'api-token': nonAdminToken },
                    method: 'POST',
                    url: '/apis/iam/v2beta/roles',
                    body: {
                        name: 'role1',
                        id: roleID,
                        actions: ['iam:users:get'],
                        projects: []
                    }
                }).then((response) => {
                    expect(response.body.role.projects).to.have.length(0);
                });
            });

            it('non-admin with project1 assignment access can create ' +
            'a new role with project1', () => {
                cy.request({
                    headers: { 'api-token': nonAdminToken },
                    method: 'POST',
                    url: '/apis/iam/v2beta/roles',
                    body: {
                        name: 'role1',
                        id: roleID,
                        actions: ['iam:users:get'],
                        projects: [project1.id]
                    }
                }).then((response) => {
                    expect(response.body.role.projects).to.have.length(1);
                });
            });

            it('non-admin with project1 assignment access gets a 404 when it attempts to create ' +
            'a role with a project that does not exist', () => {
             cy.request({
                headers: { 'api-token': nonAdminToken },
                method: 'POST',
                 url: '/apis/iam/v2beta/roles',
                 failOnStatusCode: false,
                 body: {
                     name: 'role1',
                     id: roleID,
                     actions: ['iam:users:get'],
                     projects: [project1.id, project2.id, 'notfound']
                 }
             }).then((response) => {
                 expect(response.status).to.equal(404);
             });
         });

            it('non-admin with project1 assignment access cannot create ' +
            'a new role with other projects (403)', () => {
                cy.request({
                    headers: { 'api-token': nonAdminToken },
                    method: 'POST',
                    url: '/apis/iam/v2beta/roles',
                    failOnStatusCode: false,
                    body: {
                        name: 'role1',
                        id: roleID,
                        actions: ['iam:users:get'],
                        projects: [project1.id, project2.id]
                    }
                }).then((response) => {
                    expect(response.status).to.equal(403);
                });
            });
        });

        describe('PUT /apis/iam/v2beta/roles', () => {
            it('admin can update a role with no projects to have projects', () => {
                cy.request({
                    auth: { bearer: adminIdToken },
                    method: 'POST',
                    url: '/apis/iam/v2beta/roles',
                    body: {
                        name: 'role1',
                        id: roleID,
                        actions: ['iam:users:get'],
                        projects: []
                    }
                }).then((response) => {
                    expect(response.body.role.projects).to.have.length(0);
                });

                cy.request({
                    auth: { bearer: adminIdToken },
                    method: 'PUT',
                    url: `/apis/iam/v2beta/roles/${roleID}`,
                    body: {
                        name: 'role1',
                        actions: ['iam:users:get'],
                        projects: [project1.id, project2.id]
                    }
                }).then((response) => {
                    expect(response.body.role.projects).to.have.length(2);
                });
            });

            it('admin can update a role with projects to have no projects', () => {
                cy.request({
                    auth: { bearer: adminIdToken },
                    method: 'POST',
                    url: '/apis/iam/v2beta/roles',
                    body: {
                        name: 'role1',
                        id: roleID,
                        actions: ['iam:users:get'],
                        projects: [project1.id, project2.id]
                    }
                }).then((response) => {
                    expect(response.body.role.projects).to.have.length(2);
                });

                cy.request({
                    auth: { bearer: adminIdToken },
                    method: 'PUT',
                    url: `/apis/iam/v2beta/roles/${roleID}`,
                    body: {
                        name: 'role1',
                        actions: ['iam:users:get'],
                        projects: []
                    }
                }).then((response) => {
                    expect(response.body.role.projects).to.have.length(0);
                });
            });

            it('admin cannot update a role to have projects that do not exist', () => {
                cy.request({
                    auth: { bearer: adminIdToken },
                    method: 'POST',
                    url: '/apis/iam/v2beta/roles',
                    body: {
                        name: 'role1',
                        id: roleID,
                        actions: ['iam:users:get'],
                        projects: []
                    }
                }).then((response) => {
                    expect(response.body.role.projects).to.have.length(0);
                });

                cy.request({
                    auth: { bearer: adminIdToken },
                    method: 'PUT',
                    url: `/apis/iam/v2beta/roles/${roleID}`,
                    failOnStatusCode: false,
                    body: {
                        name: 'role1',
                        actions: ['iam:users:get'],
                        projects: [project1.id, 'notfound']
                    }
                }).then((response) => {
                    expect(response.status).to.equal(404);
                });
            });

            it('non-admin with project1 assignment access can update ' +
            'a role with no projects to have project1', () => {
                cy.request({
                    auth: { bearer: adminIdToken },
                    method: 'POST',
                    url: '/apis/iam/v2beta/roles',
                    body: {
                        name: 'role1',
                        id: roleID,
                        actions: ['iam:users:get'],
                        projects: [ ]
                    }
                }).then((response) => {
                    expect(response.body.role.projects).to.have.length(0);
                });

                cy.request({
                    headers: { 'api-token': nonAdminToken },
                    method: 'PUT',
                    url: `/apis/iam/v2beta/roles/${roleID}`,
                    body: {
                        name: 'role1',
                        actions: ['iam:users:get'],
                        projects: [ project1.id ]
                    }
                }).then((response) => {
                    expect(response.body.role.projects).to.have.length(1);
                });
            });

            it('non-admin with project1 assignment access can update ' +
            'a role to remove project1', () => {
                cy.request({
                    auth: { bearer: adminIdToken },
                    method: 'POST',
                    url: '/apis/iam/v2beta/roles',
                    body: {
                        name: 'role1',
                        id: roleID,
                        actions: ['iam:users:get'],
                        projects: [ project1.id ]
                    }
                }).then((response) => {
                    expect(response.body.role.projects).to.have.length(1);
                });

                cy.request({
                    headers: { 'api-token': nonAdminToken },
                    method: 'PUT',
                    url: `/apis/iam/v2beta/roles/${roleID}`,
                    body: {
                        name: 'role1',
                        actions: ['iam:users:get'],
                        projects: [ ]
                    }
                }).then((response) => {
                    expect(response.body.role.projects).to.have.length(0);
                });
            });

            it('non-admin with project1 assignment access gets a 404 ' +
            'when updating a role to have non-existent roles', () => {
                cy.request({
                    auth: { bearer: adminIdToken },
                    method: 'POST',
                    url: '/apis/iam/v2beta/roles',
                    body: {
                        name: 'role1',
                        id: roleID,
                        actions: ['iam:users:get'],
                        projects: [ ]
                    }
                }).then((response) => {
                    expect(response.body.role.projects).to.have.length(0);
                });

                cy.request({
                    headers: { 'api-token': nonAdminToken },
                    method: 'PUT',
                    url: `/apis/iam/v2beta/roles/${roleID}`,
                    failOnStatusCode: false,
                    body: {
                        name: 'role1',
                        actions: ['iam:users:get'],
                        projects: [ project1.id, 'notfound' ]
                    }
                }).then((response) => {
                    expect(response.status).to.equal(404);
                });
            });

            it('non-admin with project1 assignment access cannot update ' +
            'a role with no projects to have other projects', () => {
                cy.request({
                    auth: { bearer: adminIdToken },
                    method: 'POST',
                    url: '/apis/iam/v2beta/roles',
                    body: {
                        name: 'role1',
                        id: roleID,
                        actions: ['iam:users:get'],
                        projects: []
                    }
                }).then((response) => {
                    expect(response.body.role.projects).to.have.length(0);
                });

                cy.request({
                    headers: { 'api-token': nonAdminToken },
                    method: 'PUT',
                    url: `/apis/iam/v2beta/roles/${roleID}`,
                    failOnStatusCode: false,
                    body: {
                        name: 'role1',
                        actions: ['iam:users:get'],
                        projects: [project1.id, project2.id]
                    }
                }).then((response) => {
                    expect(response.status).to.equal(403);
                });
            });

            it('non-admin with project1 assignment access cannot update ' +
            'a role to remove other projects', () => {
                cy.request({
                    auth: { bearer: adminIdToken },
                    method: 'POST',
                    url: '/apis/iam/v2beta/roles',
                    body: {
                        name: 'role1',
                        id: roleID,
                        actions: ['iam:users:get'],
                        projects: [project1.id, project2.id]
                    }
                }).then((response) => {
                    expect(response.body.role.projects).to.have.length(2);
                });

                cy.request({
                    headers: { 'api-token': nonAdminToken },
                    method: 'PUT',
                    url: `/apis/iam/v2beta/roles/${roleID}`,
                    failOnStatusCode: false,
                    body: {
                        name: 'role1',
                        actions: ['iam:users:get'],
                        projects: [project1.id]
                    }
                }).then((response) => {
                    expect(response.status).to.equal(403);
                });
            });

        });

    });
});
