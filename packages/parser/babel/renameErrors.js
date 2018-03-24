module.exports = ({ types: t }) => ({
  visitor: {
    VariableDeclarator({ node }) {
      if (
        node.id &&
        node.id.type === 'Identifier' &&
        node.id.name.includes('Error') &&
        node.init &&
        node.init.type === 'CallExpression' &&
        node.init.arguments[0] &&
        node.init.arguments[0].type === 'StringLiteral' &&
        node.init.arguments[0].value.includes('Error')
      ) {
        const argument = node.init.arguments[0]
        const argumentValueArr = (argument.value || '').split('.')
        const newValue = argumentValueArr[argumentValueArr.length - 1] || ''

        node.init.arguments[0] = t.stringLiteral(newValue)
      }
    }
  }
});
