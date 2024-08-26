/* eslint-env es6 */
const td = require("typedoc");
const ts = td.TypeScript;

/** @param {td.Application} app */
exports.load = function (app) {
    // Add decorator info to reflections
    // if you need parameters, you need app.converter.on(td.Converter.EVENT_CREATE_PARAMETER)
    app.converter.on(td.Converter.EVENT_CREATE_DECLARATION, addDecoratorInfo);

    // Add decorator info to serialized json
    app.serializer.addSerializer({
        priority: 0,
        supports(item) {
            return item instanceof td.DeclarationReflection;
        },
        toObject(item, obj, _ser) {
            if (item.decorators) {
                obj.decorators = item.decorators;
            }
            return obj;
        },
    });
};

/**
 * @param {td.Context} context
 * @param {td.DeclarationReflection} decl
 */
function addDecoratorInfo(context, decl) {
    const symbol = context.project.getSymbolFromReflection(decl);
    if (!symbol) return;

    const declaration = symbol.valueDeclaration;
    if (!declaration) return;
    // if (
    //     !ts.isPropertyDeclaration(declaration) &&
    //     !ts.isMethodDeclaration(declaration)
    // ) {
    //     return;
    // }

    const decorators = (declaration.modifiers || []).filter(ts.isDecorator);

    decl.decorators = (decorators || []).map((d) => ({
        name:d.expression.expression.escapedText,
        arguments: {
          opts: d.getText().substring(d.getText().indexOf("(")+1, d.getText().indexOf(")"))
        }
    }));
}
