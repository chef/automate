
import { ChefSorters } from './sorter';

describe('normalSort', () => {

    const inputObject: {} = [
        { checked: false, id: 'B', name: '123abc', status: 'NO_RULES', type: 'CUSTOM'},
        { checked: false, id: '3', name: 'project-6', status: 'NO_RULES', type: 'CUSTOM'},
        { checked: false, id: 'a', name: '12345zero', status: 'NO_RULES', type: 'CUSTOM'},
        { checked: false, id: '1', name: '123xyz', status: 'NO_RULES', type: 'CUSTOM'},
        { checked: false, id: 'abc123', name: 'abc123', status: 'NO_RULES', type: 'CUSTOM'}
      ];

    const sortedNames: string[] = ['123abc', '123xyz', '12345zero', 'abc123', 'project-6'];
    const sortedIds: string[] = ['1', '3', 'a', 'abc123', 'B'];


    it('returns a normally sorted array by chosen property', () => {
        const nameOutput = ChefSorters.normalSort(inputObject, 'name')
                                      .map(p => p.name);

        const idOutput = ChefSorters.normalSort(inputObject, 'id')
                                    .map(p => p.id);

        expect(nameOutput).toEqual(sortedNames);
        expect(idOutput).toEqual(sortedIds);
    });
});
