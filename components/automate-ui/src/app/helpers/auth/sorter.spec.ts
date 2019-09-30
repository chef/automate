import { ChefSorters } from './sorter';

describe('naturalSort', () => {

    it('returns a normally sorted array by chosen property', () => {
        const Group1 = [
            { checked: false, id: 'B', name: '123abc', status: 'NO_RULES'},
            { checked: false, id: '3', name: 'project-6', status: 'NO_RULES'},
            { checked: false, id: 'a', name: '12345zero', status: 'NO_RULES'},
            { checked: false, id: '1', name: '123xyz', status: 'NO_RULES'},
            { checked: false, id: 'abc123', name: 'abc123', status: 'NO_RULES'}
        ];

        const sortedNames = ['123abc', '123xyz', '12345zero', 'abc123', 'project-6'];
        const sortedIds = ['1', '3', 'a', 'abc123', 'B'];

        const nameOutput = ChefSorters.naturalSort(Group1, 'name').map(p => p.name);
        const idOutput = ChefSorters.naturalSort(Group1, 'id').map(p => p.id);
        expect(nameOutput).toEqual(sortedNames);
        expect(idOutput).toEqual(sortedIds);
    });

    it('intermixes capitals and lowercase with lowercase first', () => {
        const users = [
            { membership_id: 'uuid-1', name: 'Alice', id: 'alice2' },
            { membership_id: 'uuid-2', name: 'alice', id: 'alice1' },
            { membership_id: 'uuid-3', name: 'bob', id: 'builder2001' },
            { membership_id: 'uuid-4', name: 'Bob', id: 'builder2000' }
        ];

        ChefSorters.naturalSort(users, ['name', 'id']);

        expect(users.length).toEqual(4);
        expect(users[0]).toEqual(jasmine.objectContaining({ name: 'alice' }));
        expect(users[1]).toEqual(jasmine.objectContaining({ name: 'Alice' }));
        expect(users[2]).toEqual(jasmine.objectContaining({ name: 'bob' }));
        expect(users[3]).toEqual(jasmine.objectContaining({ name: 'Bob' }));
    });

    it('sorts by name then by id', () => {
        const users = [
            { membership_id: 'uuid-22', name: 'Bob', id: 'builder2001' },
            { membership_id: 'uuid-2', name: 'Bob', id: 'builder2000' },
            { membership_id: 'uuid-1', name: 'Alice in Wonderland', id: 'alice' },
            { membership_id: 'uuid-20', name: 'alice', id: 'the-other-alice' }
        ];

        ChefSorters.naturalSort(users, ['name', 'id']);

        expect(users.length).toEqual(4);
        expect(users[0]).toEqual(jasmine.objectContaining({ name: 'alice' }));
        expect(users[1]).toEqual(jasmine.objectContaining({ name: 'Alice in Wonderland' }));
        expect(users[2]).toEqual(jasmine.objectContaining({ name: 'Bob', id: 'builder2000' }));
        expect(users[3]).toEqual(jasmine.objectContaining({ name: 'Bob', id: 'builder2001' }));
    });

    it('uses natural ordering in name', () => {
        const users = [
            { membership_id: 'uuid-1', name: 'Alice500', id: 'alice1' },
            { membership_id: 'uuid-2', name: 'Alice05', id: 'alice2' },
            { membership_id: 'uuid-3', name: 'Alice1', id: 'alice3' },
            { membership_id: 'uuid-4', name: 'Alice51', id: 'alice4' },
            { membership_id: 'uuid-5', name: 'alice8', id: 'alice5' }
        ];

        ChefSorters.naturalSort(users, ['name', 'id']);

        expect(users.length).toEqual(5);
        expect(users[0]).toEqual(jasmine.objectContaining({ name: 'Alice1' }));
        expect(users[1]).toEqual(jasmine.objectContaining({ name: 'Alice05' }));
        expect(users[2]).toEqual(jasmine.objectContaining({ name: 'alice8' }));
        expect(users[3]).toEqual(jasmine.objectContaining({ name: 'Alice51' }));
        expect(users[4]).toEqual(jasmine.objectContaining({ name: 'Alice500' }));
    });

    it('uses natural ordering in id', () => {
        const users = [
            { membership_id: 'uuid-1', name: 'Alice', id: 'Alice01' },
            { membership_id: 'uuid-2', name: 'Alice', id: 'Alice300' },
            { membership_id: 'uuid-3', name: 'Alice', id: 'Alice3' },
            { membership_id: 'uuid-4', name: 'Alice', id: 'Alice-2' },
            { membership_id: 'uuid-5', name: 'Alice', id: 'alice' }
        ];

        ChefSorters.naturalSort(users, ['name', 'id']);

        expect(users.length).toEqual(5);
        expect(users[0]).toEqual(jasmine.objectContaining({ id: 'alice' }));
        expect(users[1]).toEqual(jasmine.objectContaining({ id: 'Alice-2' }));
        expect(users[2]).toEqual(jasmine.objectContaining({ id: 'Alice01' }));
        expect(users[3]).toEqual(jasmine.objectContaining({ id: 'Alice3' }));
        expect(users[4]).toEqual(jasmine.objectContaining({ id: 'Alice300' }));
    });

    it('sorts by whole string before case', () => {
        const policies = [
            { id: 'uuid-2', name: 'developer', members: [], statements: [] },
            { id: 'uuid-4', name: 'developer-Manager', members: [], statements: [] },
            { id: 'uuid-5', name: 'Developer', members: [], statements: [] }
        ];

        ChefSorters.naturalSort(policies, 'name');

        expect(policies.length).toBe(3);
        expect(policies[0]).toEqual(jasmine.objectContaining({ name: 'developer' }));
        expect(policies[1]).toEqual(jasmine.objectContaining({ name: 'Developer' }));
        expect(policies[2]).toEqual(jasmine.objectContaining({ name: 'developer-Manager' }));
        });

});
