
import { ChefSorters } from './sorter';

fdescribe('normalSort', () => {

    const inputObject: object[] = [
        { checked: false, id: '123abc', name: '123abc', status: 'NO_RULES', type: 'CUSTOM'},
        { checked: false, id: 'project-6', name: 'project-6', status: 'NO_RULES', type: 'CUSTOM'},
        { checked: false, id: '12345zero', name: '12345zero', status: 'NO_RULES', type: 'CUSTOM'},
        { checked: false, id: '123xyz', name: '123xyz', status: 'NO_RULES', type: 'CUSTOM'},
        { checked: false, id: 'abc123', name: 'abc123', status: 'NO_RULES', type: 'CUSTOM'}
      ];

    // const sortedOutput: object[] = [
    //     { checked: false, id: '123abc', name: 'abc123', status: 'NO_RULES', type: 'CUSTOM' },
    //     { checked: false, id: '123xyz', name: '123xyz', status: 'NO_RULES', type: 'CUSTOM' },
    //     { checked: false, id: '12345zero', name: '12345zero', status: 'NO_RULES', type: 'CUSTOM' },
    //     { checked: false, id: 'abc123', name: 'abc123', status: 'NO_RULES', type: 'CUSTOM' },
    //     { checked: false, id: 'project-6', name: 'project-6', status: 'NO_RULES', type: 'CUSTOM' }
    // ];

    const sortedNames: string[] = ['123abc', '123xyz', '12345zero', 'abc123', 'project-6'];

    it('returns a normally sorted array by chosen property', () => {
        expect(ChefSorters.normalSort(inputObject).map(p => p.name)).toBe(sortedNames);
    });
});

// Expected
// [
//     Object({ checked: false, id: '123abc', name: '123abc', status: 'NO_RULES', type: 'CUSTOM' }), 
//     Object({ checked: false, id: '123xyz', name: '123xyz', status: 'NO_RULES', type: 'CUSTOM' }), 
//     Object({ checked: false, id: '12345zero', name: '12345zero', status: 'NO_RULES', type: 'CUSTOM' }), 
//     Object({ checked: false, id: 'abc123', name: 'abc123', status: 'NO_RULES', type: 'CUSTOM' }), 
//     Object({ checked: false, id: 'project-6', name: 'project-6', status: 'NO_RULES', type: 'CUSTOM' })
// ] 

// to be
// [
//     Object({ checked: false, id: '123abc', name: 'abc123', status: 'NO_RULES', type: 'CUSTOM' }), 
//     Object({ checked: false, id: '123xyz', name: '123xyz', status: 'NO_RULES', type: 'CUSTOM' }), 
//     Object({ checked: false, id: '12345zero', name: '12345zero', status: 'NO_RULES', type: 'CUSTOM' }), 
//     Object({ checked: false, id: 'abc123', name: 'abc123', status: 'NO_RULES', type: 'CUSTOM' }), 
//     Object({ checked: false, id: 'project-6', name: 'project-6', status: 'NO_RULES', type: 'CUSTOM' })
// ]