
import { ChefSorters } from './sorter';

describe('naturalSort', () => {

    let inputObject1: {} = [
        { checked: false, id: 'B', name: '123abc', status: 'NO_RULES', type: 'CUSTOM'},
        { checked: false, id: '3', name: 'project-6', status: 'NO_RULES', type: 'CUSTOM'},
        { checked: false, id: 'a', name: '12345zero', status: 'NO_RULES', type: 'CUSTOM'},
        { checked: false, id: '1', name: '123xyz', status: 'NO_RULES', type: 'CUSTOM'},
        { checked: false, id: 'abc123', name: 'abc123', status: 'NO_RULES', type: 'CUSTOM'}
      ];

    let inputObject2: {} = [
        { checked: false, id: 'a', name: 'one', status: 'NO_RULES', type: 'CUSTOM' },
        { checked: false, id: 'A', name: 'two', status: 'NO_RULES', type: 'CUSTOM' },
        { checked: false, id: '123', name: 'three', status: 'NO_RULES', type: 'CUSTOM' },
        { checked: false, id: '1234', name: 'four', status: 'NO_RULES', type: 'CUSTOM' },
        { checked: false, id: 'abc123', name: 'five', status: 'NO_RULES', type: 'CUSTOM' }
    ];

    const sortedNames: string[] = ['123abc', '123xyz', '12345zero', 'abc123', 'project-6'];
    const sortedIds: string[] = ['1', '3', 'a', 'abc123', 'B'];
    const sortedIds2: string[] = ['123', '1234', 'a', 'A', 'abc123' ];


    it('returns a normally sorted array by chosen property', () => {
        // related to inputObject1
        inputObject1 = Object.values(inputObject1);
        const nameOutput = ChefSorters.naturalSort(inputObject1, 'name')
                                      .map(p => p.name);

        const idOutput = ChefSorters.naturalSort(inputObject1, 'id')
                                    .map(p => p.id);
        
        expect(nameOutput).toEqual(sortedNames);
        expect(idOutput).toEqual(sortedIds);

        // related to inputObject2
        inputObject2 = Object.values(inputObject2);
        const idOutput2 = ChefSorters.naturalSort(inputObject2, 'id' )
                                     .map(p => p.id);

        // inputObject2
        expect(idOutput2).toEqual(sortedIds2);

    });
});
