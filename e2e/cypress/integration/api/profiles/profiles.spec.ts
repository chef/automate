
describe('Available profile', () => {
    // search for profile to get the version
    let version: string;
    beforeEach(() => {
        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: 'api/v0/compliance/profiles/search',
            body: {
            filters: [
                {type: 'name', values: ['linux-baseline']}
            ]
            }
        }).then((resp: Cypress.ObjectLike) => {
            version = resp.body.profiles[0].version;
        });
    });


    it('can be installed', () => {
        const cypressPrefix = 'test-install-available-profile';
        cy.request({
            headers: { 'api-token': Cypress.env('ADMIN_TOKEN') },
            method: 'POST',
            url: 'api/v0/compliance/profiles?owner=admin',
            body: {
            name: 'linux-baseline',
            version: version
            }
        }).then((resp: Cypress.ObjectLike) => {
            expect(resp.body.summary.valid).to.equal(true);
        });
    });
    it('can be installed even when lax header present', () => {
        const cypressPrefix = 'test-install-available-profile';
        cy.request({
            headers: {
                'api-token': Cypress.env('ADMIN_TOKEN'),
                'content-type': 'application/json+lax' },
            method: 'POST',
            url: 'api/v0/compliance/profiles?owner=admin',
            body: {
            name: 'linux-baseline',
            version: version
            }
        }).then((resp: Cypress.ObjectLike) => {
            expect(resp.body.summary.valid).to.equal(true);
        });
    });
});
